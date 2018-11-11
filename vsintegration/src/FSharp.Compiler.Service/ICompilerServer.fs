namespace Microsoft.FSharp.Compiler.Server

open System

type GetErrorInfosCommandType =
    | Syntax = 0
    | Semantic = 1

[<RequireQualifiedAccess>]
module Command =

    type GetSemanticClassification =
        {
            CheckerOptions: CheckerOptions
            ClassifyRange: Range
        }

        static member Create(checkerOptions, classifyRange) =
            {
                CheckerOptions = checkerOptions
                ClassifyRange = classifyRange
            }

    type GetErrorInfos =
        {
            ParsingOptions: ParsingOptions
            CheckerOptions: CheckerOptions
            Type: GetErrorInfosCommandType
        }

        static member Create(parsingOptions, checkerOptions, typ) =
            {
                ParsingOptions = parsingOptions
                CheckerOptions = checkerOptions
                Type = typ
            }

[<RequireQualifiedAccess>]
module CommandResult =

    type GetSemanticClassification =
        {
            Items: SemanticClassificationItem []
        }

        static member Create(items) =
            {
                Items = items
            }

    type GetErrorInfos =
        {
            Items: ErrorInfo []
        }

        static member Create(items) =
            {
                Items = items
            }

type ICompilerServer =
    inherit IDisposable

    abstract GetSemanticClassificationAsync : Command.GetSemanticClassification -> Async<CommandResult.GetSemanticClassification option>

    abstract GetErrorInfosAsync : Command.GetErrorInfos -> Async<CommandResult.GetErrorInfos option>

