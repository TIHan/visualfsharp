namespace FSharp.Compiler.Server

type ICompilerServer =

    abstract GetSemanticClassificationAsync : GetSemanticClassificationCommand -> Async<SemanticClassification option>

