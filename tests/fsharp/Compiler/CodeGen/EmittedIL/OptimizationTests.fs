// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

namespace FSharp.Compiler.UnitTests.CodeGen.EmittedIL

open System.IO
open System.Reflection
open FSharp.Compiler.UnitTests
open NUnit.Framework

#if NETCOREAPP
[<TestFixture>]
module OptimizationTests =

    [<Test>]
    let ``Inline List.iter optimization``() =
        let source =
            """
module Module1

open System
open System.Runtime.CompilerServices
    
let checkIt (a: int) (b: int) (c: int) (d: int) (e: int) (x: string) = if x = "test" then printf "test"

let check1 a b c d e (xs: string list) =
    xs |> List.iter (checkIt a b c d e)
            """

        CompilerAssert.CompileLibraryAndVerifyILWithOptions [|"--optimize+"|] source
            (fun verifier ->
                verifier.VerifyIL
                    [
                        "IL_0000:  newobj     instance void Module1/check1@9::.ctor()"
                    ]
            )
#endif