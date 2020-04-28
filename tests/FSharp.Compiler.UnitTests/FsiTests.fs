module FSharp.Compiler.UnitTests.FsiTests

open System.IO
open FSharp.Compiler.Interactive.Shell
open NUnit.Framework

let createFsiSession () =
    // Intialize output and input streams
    let inStream = new StringReader("")
    let outStream = new CompilerOutputStream()
    let errStream = new CompilerOutputStream()

    // Build command line arguments & start FSI session
    let argv = [| "C:\\fsi.exe" |]
    let allArgs = Array.append argv [|"--noninteractive"|]

    let fsiConfig = FsiEvaluationSession.GetDefaultConfiguration()
    FsiEvaluationSession.Create(fsiConfig, allArgs, inStream, new StreamWriter(outStream), new StreamWriter(errStream), collectible = true)

[<Test>]
let ``GetBoundValues: No bound values at the start of FSI session`` () =
    use fsiSession = createFsiSession ()
    let values = fsiSession.GetBoundValues()
    Assert.IsEmpty values

[<Test>]
let ``GetBoundValues: Bound value has correct name`` () =
    use fsiSession = createFsiSession ()

    fsiSession.EvalInteraction("let x = 1")

    let values = fsiSession.GetBoundValues()
    let valueName, _ = values |> List.exactlyOne

    Assert.AreEqual("x", valueName)

[<Test>]
let ``GetBoundValues: Bound value has correct value`` () =
    use fsiSession = createFsiSession ()

    fsiSession.EvalInteraction("let y = 2")

    let values = fsiSession.GetBoundValues()
    let _, fsiValue = values |> List.exactlyOne

    Assert.AreEqual(2, fsiValue.ReflectionValue)

[<Test>]
let ``GetBoundValues: Bound value has correct type`` () =
    use fsiSession = createFsiSession ()

    fsiSession.EvalInteraction("let z = 3")

    let values = fsiSession.GetBoundValues()
    let _, fsiValue = values |> List.exactlyOne

    Assert.AreEqual(typeof<int>, fsiValue.ReflectionType)