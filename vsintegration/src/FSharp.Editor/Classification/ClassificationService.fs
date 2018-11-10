﻿// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

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

open FSharp.Compiler.Server

// IEditorClassificationService is marked as Obsolete, but is still supported. The replacement (IClassificationService)
// is internal to Microsoft.CodeAnalysis.Workspaces which we don't have internals visible to. Rather than add yet another
// IVT, we'll maintain the status quo.
#nowarn "44"

open Microsoft.FSharp.Compiler.SourceCodeServices

[<ExportLanguageService(typeof<IEditorClassificationService>, FSharpConstants.FSharpLanguageName)>]
type internal FSharpClassificationService
    [<ImportingConstructor>]
    (
        checkerProvider: FSharpCheckerProvider,
        projectInfoManager: FSharpProjectOptionsManager
    ) =
    static let userOpName = "SemanticColorization"

    interface IEditorClassificationService with
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
                let! sourceText = document.GetTextAsync(cancellationToken)
                // it's crucial to not return duplicated or overlapping `ClassifiedSpan`s because Find Usages service crashes.
                let targetRange = RoslynHelpers.TextSpanToFSharpRange(document.FilePath, textSpan, sourceText)

                let! _, _, projectOptions = projectInfoManager.TryGetOptionsForDocumentOrProject(document)
                let! checkerData = document.GetCheckerData(cancellationToken, ProjectOptions.FromFSharpProjectOptions(projectOptions), "FSharpClassificationService.AddSemanticClassificationsAsync")
                let cmd =
                    {
                        CheckerData = checkerData
                        RangeToClassify = 
                            { 
                                StartLine = targetRange.StartLine
                                StartColumn = targetRange.StartColumn
                                EndLine = targetRange.EndLine
                                EndColumn = targetRange.EndColumn
                            }
                    }

                let! sc = checkerProvider.Server.GetSemanticClassificationAsync(cmd)
                
                for { Range = range; Type = classificationType } in sc.Items do
                    match RoslynHelpers.FSharpCompilerServerRangeToTextSpan(sourceText, range) with
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


