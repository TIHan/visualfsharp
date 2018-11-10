namespace FSharp.Compiler.Server

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

    abstract GetSemanticClassificationAsync : GetSemanticClassificationCommand -> Async<GetSemanticClassificationResult option>

