module FSharp.Compiler.UnitTests.FsiTests

open System.IO
open FSharp.Compiler
open FSharp.Compiler.Interactive.Shell
open FSharp.Compiler.SourceCodeServices
open NUnit.Framework

let createFsiSession () =
    // Intialize output and input streams
    let inStream = new StringReader("")
    let outStream = new CompilerOutputStream()
    let errStream = new CompilerOutputStream()

    // Build command line arguments & start FSI session
    let argv = [| "C:\\fsi.exe" |]
    let allArgs = Array.append argv [|"--noninteractive"|]

    #if NETCOREAPP
    let fsiConfig = FsiEvaluationSession.GetDefaultConfiguration()
    #else
    let fsiConfig = FsiEvaluationSession.GetDefaultConfiguration()
    #endif
    FsiEvaluationSession.Create(fsiConfig, allArgs, inStream, new StreamWriter(outStream), new StreamWriter(errStream), collectible = true)

[<Test>]
let ``Test get variables`` () =
    use fsiSession = createFsiSession ()

    for i = 0 to 1000 do
        fsiSession.EvalInteraction("let x = 1")
        fsiSession.EvalInteraction("let x = 2")
        fsiSession.EvalInteraction("let y = 2")
        fsiSession.EvalInteraction("let z = 2")
        fsiSession.EvalInteraction("let b = 2")
        fsiSession.EvalInteraction("let a = 2")
        fsiSession.EvalInteraction("5")

    let values = fsiSession.GetBoundValues()
    let value = values |> List.exactlyOne

    ()