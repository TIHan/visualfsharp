namespace Microsoft.FSharp.Compiler.Server

open System
open System.IO
open System.Reflection
open System.Diagnostics
open System.Collections.Generic

open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.SourceCodeServices

[<Sealed>]
type internal CompilerServerOutOfProcess() =

    let mutable procOpt : Process option = None
    let mutable isStarted = false
    let mutable restartingHandler = Unchecked.defaultof<_>

    let getAssemblyDirectory (asm: Assembly) =
        Path.GetDirectoryName(asm.Location)

    let startProcess () =
        match procOpt with
        | Some(proc) -> 
            try proc.Kill() with | _ -> ()
            procOpt <- None
        | _ -> ()

        try
            let p = new Process()
            p.StartInfo.UseShellExecute <- true
            p.StartInfo.FileName <- Path.Combine(getAssemblyDirectory (Assembly.GetExecutingAssembly()), "FSharp.Compiler.Server.exe")

            procOpt <- Some(p)

            p.Start() |> ignore

        with
        | ex ->
            printfn "failed: %s" ex.Message
            reraise()
        

    let ipcClient = new IpcMessageClient<CompilerCommand, CompilerResult>()

    do
        restartingHandler <- ipcClient.Restarting.Subscribe(fun () ->
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

        member __.GetErrorInfosAsync(cmd) = async {
            match! ipcClient.Send(CompilerCommand.GetErrorInfos(cmd)) with
            | CompilerResult.GetErrorInfosResult(result) -> return result
            | _ -> return None
        }

    interface IDisposable with

        member __.Dispose() =
            restartingHandler.Dispose()
            (ipcClient :> IDisposable).Dispose()
            match procOpt with
            | Some(proc) -> try proc.Dispose() with | _ -> ()
            | _ -> ()