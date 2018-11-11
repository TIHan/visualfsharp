open System

open Microsoft.FSharp.Compiler.Server

[<EntryPoint>]
let main argv =
    if argv.Length = 0 then
        failwith "FSharp Compiler Server requires a name."

    CompilerServer.Run argv.[0]
    0
