namespace FSharp.Compiler.Server

open System

open Newtonsoft.Json

open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.SourceCodeServices

type Range =
    {
        StartLine: int
        StartColumn: int
        EndLine: int
        EndColumn: int
    }

type ProjectOptions =
    {
      ProjectFileName: string
      ProjectId: string option
      SourceFiles: string[]
      OtherOptions: string[]
      ReferencedProjects: (string * ProjectOptions)[]
      IsIncompleteTypeCheckEnvironment : bool
      UseScriptResolutionRules : bool      
      LoadTime : DateTime
      Stamp : int64 option
    }

    static member FromFSharpProjectOptions(options: FSharpProjectOptions) =
        {
            ProjectFileName = options.ProjectFileName
            ProjectId = options.ProjectId
            SourceFiles = options.SourceFiles
            OtherOptions = options.OtherOptions
            ReferencedProjects = options.ReferencedProjects |> Array.map (fun (x, o) -> (x, ProjectOptions.FromFSharpProjectOptions(o)))
            IsIncompleteTypeCheckEnvironment = options.IsIncompleteTypeCheckEnvironment
            UseScriptResolutionRules = options.UseScriptResolutionRules
            LoadTime = options.LoadTime
            Stamp = options.Stamp
        }

    member this.ToFSharpProjectOptions() =
        {
            ProjectFileName = this.ProjectFileName
            ProjectId = this.ProjectId
            SourceFiles = this.SourceFiles
            OtherOptions = this.OtherOptions
            ReferencedProjects = this.ReferencedProjects |> Array.map (fun (x, o) -> (x, o.ToFSharpProjectOptions()))
            IsIncompleteTypeCheckEnvironment = this.IsIncompleteTypeCheckEnvironment
            UseScriptResolutionRules = this.UseScriptResolutionRules
            LoadTime = this.LoadTime
            UnresolvedReferences = None
            OriginalLoadReferences = []
            ExtraProjectInfo = None
            Stamp = this.Stamp
        }

type SemanticClassificationItem =
    {
        Range: Range
        Type: SemanticClassificationType
    }

type SemanticClassification =
    {
        Items: SemanticClassificationItem []
    }

type CheckerData =
    {
        FilePath: string
        TextVersionHash: int
        SourceText: string
        Options: ProjectOptions
        UserOpName: string option
    }

type GetSemanticClassificationCommand =
    {
        CheckerData: CheckerData
        RangeToClassify: Range
    }

[<Sealed>]
type FSharpCompilerServer(checker: FSharpChecker) =
   
    let settings = JsonSerializerSettings()

    do
        settings.ObjectCreationHandling <- ObjectCreationHandling.Replace
        settings.ReferenceLoopHandling <- ReferenceLoopHandling.Ignore

    member private this.GetSemanticClassificationInternal(cmd: GetSemanticClassificationCommand) = async {
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

    member this.GetSemanticClassification(cmd: GetSemanticClassificationCommand) = async {
            try
                let serializedCmd = JsonConvert.SerializeObject(cmd, settings)
                let cmd = JsonConvert.DeserializeObject<GetSemanticClassificationCommand>(serializedCmd, settings)
                let! result = this.GetSemanticClassificationInternal(cmd)
                let serializedResult = JsonConvert.SerializeObject(result, settings)
                return JsonConvert.DeserializeObject<SemanticClassification option>(serializedResult, settings)
            with
            | ex ->
                printfn "%A" ex
                return None
        }

