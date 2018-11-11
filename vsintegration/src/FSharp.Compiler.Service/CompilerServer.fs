namespace FSharp.Compiler.Server

open System
open System.IO
open System.Reflection
open System.Diagnostics

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
type internal CompilerServerOut() =

    let mutable currentProcOpt : Process option = None
    let mutable isStarted = false

    let getAssemblyDirectory (asm: Assembly) =
        Path.GetDirectoryName(asm.Location)

    let startProcess () =
        match currentProcOpt with
        | Some(proc) -> try proc.Kill() with | _ -> ()
        | _ -> ()

        try
            let p = new Process()
            p.StartInfo.UseShellExecute <- true
            p.StartInfo.FileName <- Path.Combine(getAssemblyDirectory (Assembly.GetExecutingAssembly()), "FSharp.Compiler.Server.exe")

            p.Start() |> ignore
            currentProcOpt <- Some(p)
        with
        | ex ->
            printfn "failed: %s" ex.Message
            reraise()
        

    let ipcClient = IpcMessageClient<CompilerCommand, CompilerResult>()

    do
        ipcClient.Restarting.Add(fun () ->
            startProcess ()
        )

    member __.Start() =
        if not isStarted then
            startProcess ()
            ipcClient.Start ()
            isStarted <- true
        else
            failwith "FSharp Compiler Server Client already started."

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
                | _ -> return CompilerResult.GetSemanticClassification(None)
        })
       
        ipcServer.Run()

    let CreateInProcess checker =
        CompilerServerIn(checker) :> ICompilerServer

    let CreateOutOfProcess () =
        let client = CompilerServerOut()
        client.Start()
        client :> ICompilerServer