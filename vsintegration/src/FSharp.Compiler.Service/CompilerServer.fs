namespace FSharp.Compiler.Server

open System

open Newtonsoft.Json

open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.SourceCodeServices

[<RequireQualifiedAccess>]
type CompilerCommand =
    | GetSemanticClassification of GetSemanticClassificationCommand
    | GetMatchBraces

[<RequireQualifiedAccess>]
type CompilerResult =
    | Error
    | GetSemanticClassification of GetSemanticClassificationResult option

[<Sealed>]
type internal CompilerServerOut(ipcClient: IpcMessageClient<CompilerCommand, CompilerResult>) =

    interface ICompilerServer with

        member __.GetSemanticClassificationAsync(cmd) = async {
            match! ipcClient.Send(CompilerCommand.GetSemanticClassification(cmd)) with
            | CompilerResult.GetSemanticClassification(result) -> return result
            | _ -> return None
        }

[<Sealed>]
type internal CompilerServerIn(checker: FSharpChecker) =

    interface ICompilerServer with 

        member __.GetSemanticClassificationAsync(cmd: GetSemanticClassificationCommand) = async {
                let data = cmd.CheckerData
                let! _, fileAnswer = checker.ParseAndCheckFileInProject(data.FilePath, data.TextVersionHash, data.SourceText, data.Options.ToFSharpProjectOptions(), data.UserOpName)
                match fileAnswer with
                | FSharpCheckFileAnswer.Aborted -> return None
                | FSharpCheckFileAnswer.Succeeded(fileResults) ->
                    let start = mkPos cmd.RangeToClassify.StartLine cmd.RangeToClassify.StartColumn
                    let en = mkPos cmd.RangeToClassify.EndLine cmd.RangeToClassify.EndColumn
                    let targetRange = mkRange cmd.CheckerData.FilePath start en
                    return
                        Some { Items =
                            fileResults.GetSemanticClassification(Some(targetRange))
                            |> Array.map (fun (m, t) -> { Range = { StartLine = m.StartLine; StartColumn = m.StartColumn; EndLine = m.EndLine; EndColumn = m.EndColumn }; Type = t })
                        } 
            }

module CompilerServer =

    let Run () =
        let checker = FSharpChecker.Create()
        let server = CompilerServerIn(checker) :> ICompilerServer

        let ipcServer = IpcMessageServer<CompilerCommand, CompilerResult>(fun cmd -> async {
                match cmd with
                | CompilerCommand.GetSemanticClassification(cmd) ->
                    let! result = server.GetSemanticClassificationAsync(cmd)
                    return CompilerResult.GetSemanticClassification(result)
        })
       
        ipcServer.Run()

    let CreateInProcess checker =
        CompilerServerIn(checker) :> ICompilerServer

    let CreateOutOfProcess () =
        let ipcClient = IpcMessageClient<CompilerCommand, CompilerResult>()
        ipcClient.Start()
        CompilerServerOut(ipcClient) :> ICompilerServer
//type CompilerServer(ls: CompilerListenServer, ipcServer: IpcMessageServer<CompilerCommand, CompilerResult>) =

//    member __.Run() =
//        ipcServer.S