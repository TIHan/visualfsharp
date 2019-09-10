// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

namespace Microsoft.VisualStudio.FSharp.Editor

open System
open System.Linq
open System.Composition
open System.Collections.Immutable
open System.Collections.Generic
open System.Diagnostics
open System.Threading

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.Classification
open Microsoft.CodeAnalysis.Editor
open Microsoft.CodeAnalysis.Host.Mef
open Microsoft.CodeAnalysis.Text
open Microsoft.CodeAnalysis.ExternalAccess.FSharp.Classification

// IEditorClassificationService is marked as Obsolete, but is still supported. The replacement (IClassificationService)
// is internal to Microsoft.CodeAnalysis.Workspaces which we don't have internals visible to. Rather than add yet another
// IVT, we'll maintain the status quo.
#nowarn "44"

open FSharp.Compiler.SourceCodeServices
open FSharp.Compiler.Compilation

module internal rec CompilationCache =

    let hasProjectVersionChanged (oldProject: Project) (newProject: Project) =
        oldProject.Version <> newProject.Version

    let hasDependentVersionChanged (oldProject: Project) (newProject: Project) =
        let oldProjectRefs = oldProject.ProjectReferences
        let newProjectRefs = newProject.ProjectReferences
        oldProjectRefs.Count() <> newProjectRefs.Count() ||
        (oldProjectRefs, newProjectRefs)
        ||> Seq.exists2 (fun p1 p2 ->
            let doesProjectIdDiffer = p1.ProjectId <> p2.ProjectId
            let p1 = oldProject.Solution.GetProject(p1.ProjectId)
            let p2 = newProject.Solution.GetProject(p2.ProjectId)
            doesProjectIdDiffer || p1.Version <> p2.Version
        )

    let isProjectInvalidated (oldProject: Project) (newProject: Project) =
        let hasProjectVersionChanged = hasProjectVersionChanged oldProject newProject
        hasProjectVersionChanged || hasDependentVersionChanged oldProject newProject

    [<RequireQualifiedAccess>]
    type CompilationItem =
        | FSharp of FSharpCompilation
        | Other of Compilation

    let gate = obj ()
    let lookup = Dictionary<ProjectId, Project * FSharpCompilation>()

    type cenv =
        {
            manager: FSharpProjectOptionsManager
        }

    let tryMkCompilationAsync cenv (project: Project) =
        async {
            let metaRefs = 
                project.MetadataReferences 
                |> Seq.choose (fun x -> 
                    match x with
                    | :? PortableExecutableReference as x ->
                        FSharpMetadataReference.FromPortableExecutableReference x |> Some
                    | :? CompilationReference as x ->
                        FSharpMetadataReference.FromRoslynCompilation x.Compilation |> Some
                    | _ -> None
                )
                |> ImmutableArray.CreateRange

            let! ct = Async.CancellationToken
            match! cenv.manager.TryGetOptionsByProject(project, ct) with
            | None -> return None
            | Some (parseOptions, projectOptions) ->

                let docs =
                    projectOptions.SourceFiles
                    |> Array.map (fun filePath ->
                        project.Documents |> Seq.find (fun x -> String.Equals (x.FilePath, filePath, StringComparison.OrdinalIgnoreCase))
                    )

                let srcs = ResizeArray()
                for doc in docs do
                    let! text = doc.GetTextAsync() |> Async.AwaitTask
                    srcs.Add (FSharpSource.FromText(text, doc.FilePath))
                let srcs = ImmutableArray.CreateRange srcs

                let outputKind =
                    if parseOptions.IsExe || parseOptions.IsInteractive then
                        FSharpOutputKind.Exe
                    else
                        FSharpOutputKind.WinExe

                let args = projectOptions.OtherOptions |> List.ofArray

                return Some (FSharpCompilation.Create(project.AssemblyName, srcs, metaRefs, outputKind = outputKind, args = args))
        }

    let addProjectAsync cenv (project: Project) =
        async {
            match project.Language with
            | "FSharp" ->
                match! tryMkCompilationAsync cenv project with
                | None -> return None
                | Some fscomp ->
                    lookup.[project.Id] <- (project, fscomp)
                    return Some (project, fscomp)
            | _ ->
                return None
        }

    let TryGetCompilationAsync cenv (project: Project) : Async<(Project * FSharpCompilation) option> =
        async {
            match lookup.TryGetValue project.Id with
            | true, (oldProject, fscomp) -> 
                if isProjectInvalidated oldProject project then
                    return! addProjectAsync cenv project
                else
                    let changes = project.GetChanges oldProject
                    let docs = changes.GetChangedDocuments(onlyGetDocumentsWithTextChanges = true) |> Seq.map (fun x -> project.GetDocument x)
                    if Seq.isEmpty docs then
                        return Some (project, fscomp)
                    else
                        let mutable fscomp = fscomp
                        for doc in docs do
                            let! text = doc.GetTextAsync() |> Async.AwaitTask

                            let oldSrc = fscomp.Sources |> Seq.find (fun x -> String.Equals (x.FilePath, doc.FilePath, StringComparison.OrdinalIgnoreCase))
                            let src = FSharpSource.FromText(text, doc.FilePath)
                            fscomp <- fscomp.ReplaceSource (oldSrc, src)
                        lookup.[project.Id] <- (project, fscomp)
                        return Some (project, fscomp)

                        
            | _ ->
                return! addProjectAsync cenv project
        }

    let TryGetSemanticModelAsync cenv (doc: Document) : Async<FSharpSemanticModel option> =
        async {
            Monitor.Enter gate
            try
                match! TryGetCompilationAsync cenv doc.Project with
                | None -> return None
                | Some (_, fscomp) -> 
                    let src = fscomp.Sources |> Seq.find (fun x -> String.Equals (x.FilePath, doc.FilePath, StringComparison.OrdinalIgnoreCase))
                    return Some (fscomp.GetSemanticModel src)
            finally
                Monitor.Exit gate
        }

[<Export(typeof<IFSharpClassificationService>)>]
type internal NewFSharpClassificationService
    [<ImportingConstructor>]
    (
        projectInfoManager: FSharpProjectOptionsManager
    ) =

    interface IFSharpClassificationService with
        // Do not perform classification if we don't have project options (#defines matter)
        member __.AddLexicalClassifications(_: SourceText, _: TextSpan, _: List<ClassifiedSpan>, _: CancellationToken) = ()
        
        member __.AddSyntacticClassificationsAsync(_document: Document, _textSpan: TextSpan, _result: List<ClassifiedSpan>, cancellationToken: CancellationToken) =
            async {
                ()
            } |> RoslynHelpers.StartAsyncUnitAsTask cancellationToken

        member __.AddSemanticClassificationsAsync(document: Document, textSpan: TextSpan, result: List<ClassifiedSpan>, cancellationToken: CancellationToken) =
            asyncMaybe {
                let! sm = CompilationCache.TryGetSemanticModelAsync { manager = projectInfoManager } document
                sm.SyntaxTree.GetTokens (textSpan, cancellationToken)
                |> Seq.iter (fun t ->
                    match sm.TryGetEnclosingSymbol (t.Span.Start, cancellationToken) with
                    | None -> ()
                    | Some symbol ->
                        match symbol with
                        | :? TypeSymbol as symbol ->
                            if symbol.IsValueType then
                                result.Add(ClassifiedSpan(t.Span, FSharpClassificationTypes.ValueType))
                            else
                                result.Add(ClassifiedSpan(t.Span, FSharpClassificationTypes.ReferenceType))
                        | _ -> ()
                )
            } 
            |> Async.Ignore |> RoslynHelpers.StartAsyncUnitAsTask cancellationToken

        // Do not perform classification if we don't have project options (#defines matter)
        member __.AdjustStaleClassification(_: SourceText, classifiedSpan: ClassifiedSpan) : ClassifiedSpan = classifiedSpan

//[<Export(typeof<IFSharpClassificationService>)>]
type internal FSharpClassificationService
    [<ImportingConstructor>]
    (
        checkerProvider: FSharpCheckerProvider,
        projectInfoManager: FSharpProjectOptionsManager
    ) =
    static let userOpName = "SemanticColorization"

    interface IFSharpClassificationService with
        // Do not perform classification if we don't have project options (#defines matter)
        member __.AddLexicalClassifications(_: SourceText, _: TextSpan, _: List<ClassifiedSpan>, _: CancellationToken) = ()
        
        member __.AddSyntacticClassificationsAsync(document: Document, textSpan: TextSpan, result: List<ClassifiedSpan>, cancellationToken: CancellationToken) =
            async {
                use _logBlock = Logger.LogBlock(LogEditorFunctionId.Classification_Syntactic)

                let defines = projectInfoManager.GetCompilationDefinesForEditingDocument(document)  
                let! sourceText = document.GetTextAsync(cancellationToken)  |> Async.AwaitTask
                result.AddRange(Tokenizer.getClassifiedSpans(document.Id, sourceText, textSpan, Some(document.FilePath), defines, cancellationToken))
            } |> RoslynHelpers.StartAsyncUnitAsTask cancellationToken

        member __.AddSemanticClassificationsAsync(document: Document, textSpan: TextSpan, result: List<ClassifiedSpan>, cancellationToken: CancellationToken) =
            asyncMaybe {
                use _logBlock = Logger.LogBlock(LogEditorFunctionId.Classification_Semantic)

                let! _, _, projectOptions = projectInfoManager.TryGetOptionsForDocumentOrProject(document, cancellationToken)
                let! sourceText = document.GetTextAsync(cancellationToken)
                let! _, _, checkResults = checkerProvider.Checker.ParseAndCheckDocument(document, projectOptions, sourceText = sourceText, allowStaleResults = false, userOpName=userOpName) 
                // it's crucial to not return duplicated or overlapping `ClassifiedSpan`s because Find Usages service crashes.
                let targetRange = RoslynHelpers.TextSpanToFSharpRange(document.FilePath, textSpan, sourceText)
                let classificationData = checkResults.GetSemanticClassification (Some targetRange) |> Array.distinctBy fst
                
                for (range, classificationType) in classificationData do
                    match RoslynHelpers.TryFSharpRangeToTextSpan(sourceText, range) with
                    | None -> ()
                    | Some span -> 
                        let span = 
                            match classificationType with
                            | SemanticClassificationType.Printf -> span
                            | _ -> Tokenizer.fixupSpan(sourceText, span)
                        result.Add(ClassifiedSpan(span, FSharpClassificationTypes.getClassificationTypeName(classificationType)))
            } 
            |> Async.Ignore |> RoslynHelpers.StartAsyncUnitAsTask cancellationToken

        // Do not perform classification if we don't have project options (#defines matter)
        member __.AdjustStaleClassification(_: SourceText, classifiedSpan: ClassifiedSpan) : ClassifiedSpan = classifiedSpan


