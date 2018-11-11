open System

open Microsoft.FSharp.Compiler.Server

[<EntryPoint>]
let main _argv =
    CompilerServer.Run()
    0
