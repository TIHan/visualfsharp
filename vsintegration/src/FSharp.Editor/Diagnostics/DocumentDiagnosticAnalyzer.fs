// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

namespace Microsoft.VisualStudio.FSharp.Editor

open System
open System.Collections.Immutable
open System.Collections.Generic
open System.Threading
open System.Threading.Tasks

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.Diagnostics
open Microsoft.CodeAnalysis.Text

open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.Server

[<DiagnosticAnalyzer(FSharpConstants.FSharpLanguageName)>]
type internal FSharpDocumentDiagnosticAnalyzer() =
    inherit DocumentDiagnosticAnalyzer()

    let getServer(document: Document) =
        document.Project.Solution.Workspace.Services.GetService<FSharpCheckerWorkspaceService>().Server

    let getProjectInfoManager(document: Document) =
        document.Project.Solution.Workspace.Services.GetService<FSharpCheckerWorkspaceService>().FSharpProjectOptionsManager

    static member GetDiagnostics(server: ICompilerServer, document: Document, parsingOptions, projectOptions, cmdType, cancellationToken) = 
        async {
            
            let! checkerOptions = document.GetCheckerOptions(cancellationToken, ProjectOptions.FromFSharpProjectOptions(projectOptions), "FSharpDocumentDiagnosticAnalyzer.GetDiagnostics")
            match checkerOptions with
            | Some(checkerOptions) ->
                let cmd = Command.GetErrorInfos.Create(ParsingOptions.FromFSharpParsingOptions(parsingOptions), checkerOptions, cmdType)
                match! server.GetErrorInfosAsync(cmd) with
                | Some(result) ->
                    let filePath = document.FilePath
                    let! sourceText = document.GetTextAsync(cancellationToken) |> Async.AwaitTask
                    let results = 
                        result.Items
                        |> Seq.choose(fun error ->
                            if error.StartLineAlternate = 0 || error.EndLineAlternate = 0 then
                                // F# error line numbers are one-based. Compiler returns 0 for global errors (reported by ProjectDiagnosticAnalyzer)
                                None
                            else
                                // Roslyn line numbers are zero-based
                                let linePositionSpan = LinePositionSpan(LinePosition(error.StartLineAlternate - 1, error.StartColumn), LinePosition(error.EndLineAlternate - 1, error.EndColumn))
                                let textSpan = sourceText.Lines.GetTextSpan(linePositionSpan)
                        
                                // F# compiler report errors at end of file if parsing fails. It should be corrected to match Roslyn boundaries
                                let correctedTextSpan =
                                    if textSpan.End <= sourceText.Length then 
                                        textSpan 
                                    else 
                                        let start =
                                            min textSpan.Start (sourceText.Length - 1)
                                            |> max 0

                                        TextSpan.FromBounds(start, sourceText.Length)
                        
                                let location = Location.Create(filePath, correctedTextSpan , linePositionSpan)
                                Some(RoslynHelpers.ConvertError(error, location)))
                        |> Seq.toImmutableArray
                    return results
                | _ ->
                    return ImmutableArray<Diagnostic>.Empty
            | _ -> 
                return ImmutableArray<Diagnostic>.Empty
        }

    override __.Priority = 10 // Default = 50

    override this.SupportedDiagnostics = RoslynHelpers.SupportedDiagnostics()

    override this.AnalyzeSyntaxAsync(document: Document, cancellationToken: CancellationToken): Task<ImmutableArray<Diagnostic>> =
        let projectInfoManager = getProjectInfoManager document
        asyncMaybe {
            let! parsingOptions, projectOptions = projectInfoManager.TryGetOptionsForEditingDocumentOrProject(document)
            return! 
                FSharpDocumentDiagnosticAnalyzer.GetDiagnostics(getServer document, document, parsingOptions, projectOptions, GetErrorInfosCommandType.Syntax, cancellationToken)
                |> liftAsync
        } 
        |> Async.map (Option.defaultValue ImmutableArray<Diagnostic>.Empty)
        |> RoslynHelpers.StartAsyncAsTask cancellationToken

    override this.AnalyzeSemanticsAsync(document: Document, cancellationToken: CancellationToken): Task<ImmutableArray<Diagnostic>> =
        let projectInfoManager = getProjectInfoManager document
        asyncMaybe {
            let! parsingOptions, _, projectOptions = projectInfoManager.TryGetOptionsForDocumentOrProject(document) 
            if document.Project.Name <> FSharpConstants.FSharpMiscellaneousFilesName || isScriptFile document.FilePath then
                return! 
                    FSharpDocumentDiagnosticAnalyzer.GetDiagnostics(getServer document, document, parsingOptions, projectOptions, GetErrorInfosCommandType.Semantic, cancellationToken)
                    |> liftAsync
            else
                return ImmutableArray<Diagnostic>.Empty
        }
        |> Async.map (Option.defaultValue ImmutableArray<Diagnostic>.Empty)
        |> RoslynHelpers.StartAsyncAsTask cancellationToken

    interface IBuiltInAnalyzer with
        member __.GetAnalyzerCategory() : DiagnosticAnalyzerCategory = DiagnosticAnalyzerCategory.SemanticDocumentAnalysis
        member __.OpenFileOnly _ = true

