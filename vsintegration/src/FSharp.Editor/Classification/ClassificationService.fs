// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

namespace Microsoft.VisualStudio.FSharp.Editor

open System
open System.Composition
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

open System.Runtime.Caching
open FSharp.Compiler.SourceCodeServices

type SemanticClassificationData = (struct(FSharp.Compiler.Range.range * SemanticClassificationType)[])
type SemanticClassificationCacheValue = (VersionStamp * struct(FSharp.Compiler.Range.range * SemanticClassificationType)[])

[<Export(typeof<IFSharpClassificationService>)>]
type internal FSharpClassificationService
    [<ImportingConstructor>]
    (
        checkerProvider: FSharpCheckerProvider,
        projectInfoManager: FSharpProjectOptionsManager
    ) =
    static let userOpName = "SemanticColorization"

    static let addSemanticClassification classificationData sourceText (result: List<ClassifiedSpan>) (targetSpan: TextSpan) =
        for struct(range, classificationType) in classificationData do
            match RoslynHelpers.TryFSharpRangeToTextSpan(sourceText, range) with
            | None -> ()
            | Some span -> 
                let span = 
                    match classificationType with
                    | SemanticClassificationType.Printf -> span
                    | _ -> Tokenizer.fixupSpan(sourceText, span)
                if targetSpan.Contains span then
                    result.Add(ClassifiedSpan(span, FSharpClassificationTypes.getClassificationTypeName(classificationType)))

    static let toLookup (data: SemanticClassificationData) =
        let lookup = System.Collections.Generic.Dictionary()
        for i = 0 to data.Length - 1 do
            let (struct(r, _) as dataItem) = data.[i]
            lookup.[r.StartLine] <- dataItem
        System.Collections.ObjectModel.ReadOnlyDictionary lookup

    let semanticClassificationCache = new MemoryCache("semantic-classification")

    let cacheSemanticClassification (document: Document) (semanticClassification: SemanticClassificationCacheValue) =
        let policy = CacheItemPolicy()
        policy.Priority <- CacheItemPriority.Default
        policy.SlidingExpiration <- TimeSpan.FromSeconds 10.
        semanticClassificationCache.Set(document.Id.ToString(), semanticClassification, policy)

    let tryGetCachedSemanticClassification (document: Document) = async {
        let! ct = Async.CancellationToken
        let! currentVersion = document.GetTextVersionAsync ct |> Async.AwaitTask

        match semanticClassificationCache.Get(document.Id.Id.ToString()) with
        | null -> return None
        | :? SemanticClassificationCacheValue as semanticClassification ->
            match semanticClassification with
            | (version, _) when currentVersion = version ->
                return Some semanticClassification
            | _ ->
                return None
        | _ ->
            return None }

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
                let! currentVersion = document.GetTextVersionAsync(cancellationToken)
                if not (document.Project.Solution.Workspace.IsDocumentOpen document.Id) then
                    match! tryGetCachedSemanticClassification document |> liftAsync with
                    | Some(_, classificationData) ->
                        addSemanticClassification classificationData sourceText result textSpan
                    | _ ->
                        let! classificationData = checkerProvider.Checker.GetBackgroundSemanticClassificationForFile(document.FilePath, projectOptions, userOpName=userOpName) |> liftAsync
                        cacheSemanticClassification document (currentVersion, classificationData)
                        addSemanticClassification classificationData sourceText result textSpan
                else
                    let! _, _, checkResults = checkerProvider.Checker.ParseAndCheckDocument(document, projectOptions, sourceText = sourceText, allowStaleResults = false, userOpName=userOpName) 
                    // it's crucial to not return duplicated or overlapping `ClassifiedSpan`s because Find Usages service crashes.
                    let targetRange = RoslynHelpers.TextSpanToFSharpRange(document.FilePath, textSpan, sourceText)
                    let classificationData = checkResults.GetSemanticClassification (Some targetRange)             
                    addSemanticClassification classificationData sourceText result textSpan
            } 
            |> Async.Ignore |> RoslynHelpers.StartAsyncUnitAsTask cancellationToken

        // Do not perform classification if we don't have project options (#defines matter)
        member __.AdjustStaleClassification(_: SourceText, classifiedSpan: ClassifiedSpan) : ClassifiedSpan = classifiedSpan


