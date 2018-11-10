[<RequireQualifiedAccess>]
module FSharp.Compiler.Server.Constants

[<Literal>]
let IpcMegabyteSize = 10

let IpcBufferSize = (IpcMegabyteSize / 2) * 1024 * 1024
