// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

namespace Microsoft.VisualStudio.FSharp.Editor

open System
open System.Composition
open System.Collections.Generic
open System.Diagnostics
open System.Threading
open System.Runtime.CompilerServices

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.Classification
open Microsoft.CodeAnalysis.Editor
open Microsoft.CodeAnalysis.Host.Mef
open Microsoft.CodeAnalysis.Text
open Microsoft.CodeAnalysis.ExternalAccess.FSharp.Classification
open Microsoft.VisualStudio.FSharp.Editor.Utilities

// IEditorClassificationService is marked as Obsolete, but is still supported. The replacement (IClassificationService)
// is internal to Microsoft.CodeAnalysis.Workspaces which we don't have internals visible to. Rather than add yet another
// IVT, we'll maintain the status quo.
#nowarn "44"

open System.Runtime.Caching
open FSharp.Compiler.QuickLex
open FSharp.Compiler.SourceCodeServices

type SyntacticClassificationLookup = IReadOnlyDictionary<int, ResizeArray<struct(TokenKind * FSharp.Compiler.Range.range)>>
type SemanticClassificationData = (struct(FSharp.Compiler.Range.range * SemanticClassificationType)[])
type SemanticClassificationLookup = IReadOnlyDictionary<int, ResizeArray<struct(FSharp.Compiler.Range.range * SemanticClassificationType)>>

[<Sealed>]
type DocumentCache<'Value when 'Value : not struct>() =
    let cache = new MemoryCache("fsharp-cache")
    let policy = CacheItemPolicy(SlidingExpiration = TimeSpan.FromSeconds 2.)

    member _.TryGetValueAsync(doc: Document) = async {
        let! ct = Async.CancellationToken
        let! currentVersion = doc.GetTextVersionAsync ct |> Async.AwaitTask

        match cache.Get(doc.Id.ToString()) with
        | null -> return ValueNone
        | :? (Microsoft.CodeAnalysis.VersionStamp * 'Value) as value ->
            if fst value = currentVersion then
                return ValueSome(snd value)
            else
                return ValueNone
        | _ ->
            return ValueNone }

    member _.SetAsync(doc: Document, value: 'Value) = async {
            let! ct = Async.CancellationToken
            let! currentVersion = doc.GetTextVersionAsync ct |> Async.AwaitTask
            cache.Set(doc.Id.ToString(), (currentVersion, value), policy) }

    interface IDisposable with

        member _.Dispose() = cache.Dispose()

[<Export(typeof<IFSharpClassificationService>)>]
type internal FSharpClassificationService
    [<ImportingConstructor>]
    (
        checkerProvider: FSharpCheckerProvider,
        projectInfoManager: FSharpProjectOptionsManager
    ) =
    static let userOpName = "SemanticColorization"

    static let addSemanticClassification (lookup: SemanticClassificationLookup) sourceText (targetSpan: TextSpan) (outputResult: List<ClassifiedSpan>) =
        let r = RoslynHelpers.TextSpanToFSharpRange("", targetSpan, sourceText)
        for i = r.StartLine to r.EndLine do
            match lookup.TryGetValue i with
            | true, items ->
                for struct(range, classificationType) in items do
                    match RoslynHelpers.TryFSharpRangeToTextSpan(sourceText, range) with
                    | None -> ()
                    | Some span -> 
                        let span = 
                            match classificationType with
                            | SemanticClassificationType.Printf -> span
                            | _ -> Tokenizer.fixupSpan(sourceText, span)
                        if targetSpan.Contains span then
                            outputResult.Add(ClassifiedSpan(span, FSharpClassificationTypes.getClassificationTypeName(classificationType)))
            | _ ->
                ()

    static let toLookup (data: SemanticClassificationData) =
        let lookup = System.Collections.Generic.Dictionary<int, ResizeArray<struct(FSharp.Compiler.Range.range * SemanticClassificationType)>>()
        for i = 0 to data.Length - 1 do
            let (struct(r, _) as dataItem) = data.[i]
            let items =
                match lookup.TryGetValue r.StartLine with
                | true, items -> items
                | _ ->
                    let items = ResizeArray()
                    lookup.[r.StartLine] <- items
                    items
            items.Add dataItem
        System.Collections.ObjectModel.ReadOnlyDictionary lookup :> IReadOnlyDictionary<_, _>

    static let _createSyntacticLookup (text: SourceText) ct =
        let lookup = Dictionary()

        let getLineData i =
            match lookup.TryGetValue i with
            | true, data -> data
            | _ ->
                let data = ResizeArray()
                lookup.[i] <- data
                data

        let onToken = 
            fun kind (m: FSharp.Compiler.Range.range) -> 
                if m.StartLine <> m.EndLine then
                    for i = m.StartLine to m.EndLine do
                        let data = getLineData i
                        data.Add(struct(kind, m))
        Lexer.Lex(text.ToFSharpSourceText(), onToken, flags=LexerFlags.SkipTrivia, ct=ct)
        System.Collections.ObjectModel.ReadOnlyDictionary lookup

    static let _addSyntacticClassification (lookup: SyntacticClassificationLookup) sourceText (targetSpan: TextSpan) (outputResult: List<ClassifiedSpan>) =
        let r = RoslynHelpers.TextSpanToFSharpRange("", targetSpan, sourceText)
        for i = r.StartLine to r.EndLine do
            match lookup.TryGetValue i with
            | true, items ->
                for struct(kind, m) in items do
                    match RoslynHelpers.TryFSharpRangeToTextSpan(sourceText, m) with
                    | Some span when targetSpan.Contains span -> 
                        let typeName =
                            match kind with
                            | TokenKind.Keyword -> ClassificationTypeNames.Keyword
                            | TokenKind.Identifier -> ClassificationTypeNames.Identifier
                            | TokenKind.Text -> ClassificationTypeNames.Text
                            | TokenKind.StringLiteral -> ClassificationTypeNames.StringLiteral
                            | TokenKind.NumericLiteral -> ClassificationTypeNames.NumericLiteral
                            | TokenKind.Comment -> ClassificationTypeNames.Comment
                        ClassifiedSpan(span, typeName)
                        |> outputResult.Add
                    | _ ->
                        ()
            | _ ->  
                ()

   // static let syntacticTable = ConditionalWeakTable<SourceText, Lazy<SyntacticClassificationLookup>>()
   // static let _semanticTable = ConditionalWeakTable<SourceText, AsyncLazy<SemanticClassificationLookup>>()
    let _syntacticClassificationCache = new DocumentCache<SyntacticClassificationLookup>()
    let semanticClassificationCache = new DocumentCache<SemanticClassificationLookup>()

    interface IFSharpClassificationService with
        // Do not perform classification if we don't have project options (#defines matter)
        member __.AddLexicalClassifications(_: SourceText, _: TextSpan, _: List<ClassifiedSpan>, _: CancellationToken) = ()
        
        member __.AddSyntacticClassificationsAsync(document: Document, textSpan: TextSpan, result: List<ClassifiedSpan>, cancellationToken: CancellationToken) =
            async {
                use _logBlock = Logger.LogBlock(LogEditorFunctionId.Classification_Syntactic)

                let! sourceText = document.GetTextAsync(cancellationToken)  |> Async.AwaitTask
                if not (document.Project.Solution.Workspace.IsDocumentOpen document.Id) then
                    let onToken = 
                        fun kind (m: FSharp.Compiler.Range.range) -> 
                            match RoslynHelpers.TryFSharpRangeToTextSpan(sourceText, m) with
                            | Some span -> 
                                let typeName =
                                    match kind with
                                    | TokenKind.Keyword -> ClassificationTypeNames.Keyword
                                    | TokenKind.Identifier -> ClassificationTypeNames.Identifier
                                    | TokenKind.Text -> ClassificationTypeNames.Text
                                    | TokenKind.StringLiteral -> ClassificationTypeNames.StringLiteral
                                    | TokenKind.NumericLiteral -> ClassificationTypeNames.NumericLiteral
                                    | TokenKind.Comment -> ClassificationTypeNames.Comment
                                ClassifiedSpan(TextSpan(textSpan.Start + span.Start, span.Length), typeName)
                                |> result.Add
                            | _ ->
                                ()
                    Lexer.Lex(sourceText.GetSubText(textSpan).ToFSharpSourceText(), onToken, flags=LexerFlags.SkipTrivia, ct=cancellationToken)
                    //match! syntacticClassificationCache.TryGetValueAsync document with
                    //| ValueSome classificationData ->
                    //    addSyntacticClassification classificationData sourceText textSpan result
                    //| _ ->
                    //    let lookup = createSyntacticLookup sourceText cancellationToken :> IReadOnlyDictionary<_, _>
                  //  let lookup = syntacticTable.GetValue(sourceText, fun _ -> lazy (createSyntacticLookup sourceText cancellationToken :> IReadOnlyDictionary<_, _>)).Value
                      //  addSyntacticClassification lookup sourceText textSpan result

                else
                    let defines = projectInfoManager.GetCompilationDefinesForEditingDocument(document)  
                    let classifiedSpans = Tokenizer.getClassifiedSpans(document.Id, sourceText, textSpan, Some(document.FilePath), defines, cancellationToken)
                    result.AddRange classifiedSpans
            } |> RoslynHelpers.StartAsyncUnitAsTask cancellationToken

        member __.AddSemanticClassificationsAsync(document: Document, textSpan: TextSpan, result: List<ClassifiedSpan>, cancellationToken: CancellationToken) =
            asyncMaybe {
                use _logBlock = Logger.LogBlock(LogEditorFunctionId.Classification_Semantic) 

                let! _, _, projectOptions = projectInfoManager.TryGetOptionsForDocumentOrProject(document, cancellationToken)
                let! sourceText = document.GetTextAsync(cancellationToken)
                if not (document.Project.Solution.Workspace.IsDocumentOpen document.Id) then
                    match! semanticClassificationCache.TryGetValueAsync document |> liftAsync with
                    | ValueSome classificationData ->
                        addSemanticClassification classificationData sourceText textSpan result
                    | _ ->
                        let! classificationData = checkerProvider.Checker.GetBackgroundSemanticClassificationForFile(document.FilePath, projectOptions, userOpName=userOpName) |> liftAsync
                        let classificationData = toLookup classificationData
                        do! semanticClassificationCache.SetAsync(document, classificationData) |> liftAsync
                        addSemanticClassification classificationData sourceText textSpan result
                else
                    let! _, _, checkResults = checkerProvider.Checker.ParseAndCheckDocument(document, projectOptions, sourceText = sourceText, allowStaleResults = false, userOpName=userOpName) 
                    // it's crucial to not return duplicated or overlapping `ClassifiedSpan`s because Find Usages service crashes.
                    let targetRange = RoslynHelpers.TextSpanToFSharpRange(document.FilePath, textSpan, sourceText)
                    let classificationData = checkResults.GetSemanticClassification (Some targetRange)
                    let classificationData = toLookup classificationData
                    addSemanticClassification classificationData sourceText textSpan result
            } 
            |> Async.Ignore |> RoslynHelpers.StartAsyncUnitAsTask cancellationToken

        // Do not perform classification if we don't have project options (#defines matter)
        member __.AdjustStaleClassification(_: SourceText, classifiedSpan: ClassifiedSpan) : ClassifiedSpan = classifiedSpan


