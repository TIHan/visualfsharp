namespace Microsoft.FSharp.Compiler.Server

open System
open System.IO
open System.Reflection
open System.Diagnostics
open System.Collections.Generic

open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.SourceCodeServices

[<Sealed>]
type internal CompilerServerInProcess(checker: FSharpChecker) =

    interface ICompilerServer with 

        member __.GetSemanticClassificationAsync(cmd) = async {
            let data = cmd.CheckerOptions
            let! _, fileAnswer = checker.ParseAndCheckFileInProject(data.FilePath, data.TextVersionHash, data.SourceText, data.ProjectOptions.ToFSharpProjectOptions(), data.UserOpName)
            match fileAnswer with
            | FSharpCheckFileAnswer.Aborted -> return None
            | FSharpCheckFileAnswer.Succeeded(fileResults) ->
                let targetRange = cmd.ClassifyRange.ToFSharpRange(cmd.CheckerOptions.FilePath)
                return
                    Some { Items =
                        fileResults.GetSemanticClassification(Some(targetRange))
                        |> Array.map (fun (m, t) -> SemanticClassificationItem.FromFSharp(m, t))
                    } 
        }

        member __.GetErrorInfosAsync(cmd) = async {
            let! result = GetErrorInfos.Execute checker cmd
            return Some(result)
        }

    interface IDisposable with

        member __.Dispose() = ()