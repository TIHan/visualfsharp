open System

open FSharp.Compiler.Server

[<EntryPoint>]
let main _argv =
    CompilerServer.Run()
    0
