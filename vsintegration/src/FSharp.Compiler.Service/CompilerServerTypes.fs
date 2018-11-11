namespace Microsoft.FSharp.Compiler.Server

[<RequireQualifiedAccess>]
type CompilerCommand =
    | GetSemanticClassification of Command.GetSemanticClassification
    | GetErrorInfos of Command.GetErrorInfos

[<RequireQualifiedAccess>]
type CompilerResult =
    | GetSemanticClassification of CommandResult.GetSemanticClassification option
    | GetErrorInfosResult of CommandResult.GetErrorInfos option
