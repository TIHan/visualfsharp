// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

namespace Microsoft.VisualStudio.FSharp.Editor

open System
open System.Composition
open System.Collections.Generic
open System.Collections.Immutable
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

open FSharp.Compiler.Range
open FSharp.Compiler.SourceCodeServices
open FSharp.Compiler.SourceCodeServices.Lexer

[<Export(typeof<IFSharpClassificationService>)>]
type internal FSharpClassificationService
    [<ImportingConstructor>]
    (
        checkerProvider: FSharpCheckerProvider,
        projectInfoManager: FSharpProjectOptionsManager
    ) =
    static let userOpName = "SemanticColorization"

    static member GetLexicalClassifications(filePath: string, defines, text: SourceText, textSpan: TextSpan, ?ct) =
        let ct = defaultArg ct CancellationToken.None
        let result = ImmutableArray.CreateBuilder()
        let tokenCallback =
            let textRange = RoslynHelpers.TextSpanToFSharpRange(filePath, textSpan, text)
            fun (tok: FSharpSyntaxToken) ->
                if rangeContainsRange textRange tok.Range then
                    let spanKind =
                        if tok.IsKeyword then
                            ClassificationTypeNames.Keyword
                        elif tok.IsNumericLiteral then
                            ClassificationTypeNames.NumericLiteral
                        elif tok.IsCommentTrivia then
                            ClassificationTypeNames.Comment
                        elif tok.IsStringLiteral then
                            ClassificationTypeNames.StringLiteral
                        else
                            ClassificationTypeNames.Text

                    match RoslynHelpers.TryFSharpRangeToTextSpan(text, tok.Range) with
                    | Some span -> result.Add(ClassifiedSpan(spanKind, span))
                    | _ -> ()
                
        let flags = FSharpLexerFlags.Default &&& ~~~FSharpLexerFlags.SkipTrivia
        FSharpLexer.Lex(text.ToFSharpSourceText(), tokenCallback, langVersion = "preview", filePath = filePath, conditionalCompilationDefines = defines, flags = flags, ct = ct)

        result.ToImmutable()
        

    interface IFSharpClassificationService with
        // Do not perform classification if we don't have project options (#defines matter)
        member __.AddLexicalClassifications(_: SourceText, _: TextSpan, _: List<ClassifiedSpan>, _: CancellationToken) = ()
        
        member __.AddSyntacticClassificationsAsync(document: Document, textSpan: TextSpan, result: List<ClassifiedSpan>, cancellationToken: CancellationToken) =
            async {
                use _logBlock = Logger.LogBlock(LogEditorFunctionId.Classification_Syntactic)

                let defines = projectInfoManager.GetCompilationDefinesForEditingDocument(document)  
                let! sourceText = document.GetTextAsync(cancellationToken)  |> Async.AwaitTask
                result.AddRange(FSharpClassificationService.GetLexicalClassifications(document.FilePath, defines, sourceText, textSpan, ct = cancellationToken))
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


