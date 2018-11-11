﻿namespace Microsoft.FSharp.Compiler.Server

open System
open System.IO
open System.Reflection
open System.Diagnostics
open System.Collections.Generic

open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.SourceCodeServices

[<RequireQualifiedAccess>]
module CompilerServer =

    let Run () =
        let checker = FSharpChecker.Create()
        let server = new CompilerServerInProcess(checker) :> ICompilerServer

        let ipcServer = IpcMessageServer<CompilerCommand, CompilerResult>(fun cmd -> async {
                match cmd with
                | CompilerCommand.GetSemanticClassification(cmd) ->
                    let! result = server.GetSemanticClassificationAsync(cmd)
                    return CompilerResult.GetSemanticClassification(result)

                | CompilerCommand.GetErrorInfos(cmd) ->
                    let! result = server.GetErrorInfosAsync(cmd)
                    return CompilerResult.GetErrorInfosResult(result)
        })
       
        ipcServer.Run()

    let CreateInProcess checker =
        new CompilerServerInProcess(checker) :> ICompilerServer

    let CreateOutOfProcess () =
        let client = new CompilerServerOutOfProcess()
        client.Start()
        client :> ICompilerServer