module FSharp.Compiler.UnitTests.FsiTests

open System.IO
open FSharp.Compiler.Interactive.Shell
open NUnit.Framework

type Test = { x: int }

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
    let name, _ = values |> List.exactlyOne

    Assert.AreEqual("x", name)

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

[<Test>]
let ``AddBoundValue: Creating a bound value will result in retrieving the correct name`` () =
    use fsiSession = createFsiSession ()

    fsiSession.AddBoundValue("w", [123])

    let values = fsiSession.GetBoundValues()
    let name, _ = values |> List.exactlyOne

    Assert.AreEqual("w", name)

[<Test>]
let ``AddBoundValue: Creating a bound value will result in retrieving the correct value`` () =
    use fsiSession = createFsiSession ()

    fsiSession.AddBoundValue("w", [123])

    let values = fsiSession.GetBoundValues()
    let _, fsiValue = values |> List.exactlyOne

    Assert.AreEqual([123], fsiValue.ReflectionValue)

[<Test>]
let ``AddBoundValue: Creating a bound value will result in retrieving the correct type`` () =
    use fsiSession = createFsiSession ()

    fsiSession.AddBoundValue("w", [123])

    let values = fsiSession.GetBoundValues()
    let _, fsiValue = values |> List.exactlyOne

    Assert.AreEqual(typeof<int list>, fsiValue.ReflectionType)

[<Test>]
let ``AddBoundValue: Creating a bound value will result in retrieving the correct record value`` () =
    use fsiSession = createFsiSession ()

    fsiSession.EvalInteraction("""#r "FSharp.Compiler.UnitTests.dll" """)
    fsiSession.EvalInteraction("""open FSharp.Compiler.UnitTests.FsiTests""")
    fsiSession.AddBoundValue("r", { x = 1 })
    fsiSession.EvalInteraction("let x = { r with x = 5 }")

    let values = fsiSession.GetBoundValues()
    let _, fsiValue = values |> List.exactlyOne

    Assert.AreEqual(typeof<int list>, fsiValue.ReflectionType)