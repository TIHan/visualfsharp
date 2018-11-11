namespace FSharp.Compiler.Server

open System

type GetSemanticClassificationCommand =
    {
        CheckerData: CheckerData
        RangeToClassify: Range
    }

type GetSemanticClassificationResult =
    {
        Items: SemanticClassificationItem []
    }

type ICompilerServer =
    inherit IDisposable

    abstract GetSemanticClassificationAsync : GetSemanticClassificationCommand -> Async<GetSemanticClassificationResult option>

