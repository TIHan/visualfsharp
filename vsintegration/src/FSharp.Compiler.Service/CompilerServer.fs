namespace FSharp.Compiler.Server

open System

open Newtonsoft.Json

open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.SourceCodeServices

[<Sealed>]
type FSharpCompilerServer(checker: FSharpChecker) =
   
    let settings = JsonSerializerSettings()

    do
        settings.ObjectCreationHandling <- ObjectCreationHandling.Replace
        settings.ReferenceLoopHandling <- ReferenceLoopHandling.Ignore

    member this.GetSemanticClassificationAsync(cmd: GetSemanticClassificationCommand) = async {
            let data = cmd.CheckerData
            let! _, fileAnswer = checker.ParseAndCheckFileInProject(data.FilePath, data.TextVersionHash, data.SourceText, data.Options.ToFSharpProjectOptions(), data.UserOpName)
            match fileAnswer with
            | FSharpCheckFileAnswer.Aborted -> return None
            | FSharpCheckFileAnswer.Succeeded(fileResults) ->
                let start = mkPos cmd.RangeToClassify.StartLine cmd.RangeToClassify.StartColumn
                let en = mkPos cmd.RangeToClassify.EndLine cmd.RangeToClassify.EndColumn
                let targetRange = mkRange cmd.CheckerData.FilePath start en
                return
                    Some { Items =
                        fileResults.GetSemanticClassification(Some(targetRange))
                        |> Array.map (fun (m, t) -> { Range = { StartLine = m.StartLine; StartColumn = m.StartColumn; EndLine = m.EndLine; EndColumn = m.EndColumn }; Type = t })
                    } 
        }

    interface ICompilerServer with

        member this.GetSemanticClassificationAsync(cmd: GetSemanticClassificationCommand) = async {
                try
                    let serializedCmd = JsonConvert.SerializeObject(cmd, settings)
                    let cmd = JsonConvert.DeserializeObject<GetSemanticClassificationCommand>(serializedCmd, settings)
                    let! result = this.GetSemanticClassificationAsync(cmd)
                    let serializedResult = JsonConvert.SerializeObject(result, settings)
                    return JsonConvert.DeserializeObject<SemanticClassification option>(serializedResult, settings)
                with
                | ex ->
                    printfn "%A" ex
                    return None
            }

