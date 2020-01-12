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
open System.Text.RegularExpressions
open FSharp.Quotations
open FSharp.Quotations.DerivedPatterns
open FSharp.Quotations.ExprShape
open FSharp.Quotations.Patterns

let test() = 
    let xs = [|1;2;3|]
    let q = <@ xs.[0..^1] @>
    
    let expectEqualWithoutWhitespace s1 s2 = 
        let a = Regex.Replace(s1, "\s", "") 
        let b = Regex.Replace(s2, "\s", "")
        if a <> b then failwithf "Expected '%s', but got\n'%s'" a b
        ()
    
    let expected = "Call(None, GetArraySlice, [ValueWithName([|1;2;3|], xs), NewUnionCase(Some, Value(0)), NewUnionCase(Some, Call(None, []`1.GetReverseIndex, [ValueWithName([|1;2;3|], xs), Value(0), Value(1)]))])"
    expectEqualWithoutWhitespace expected (q.ToString())
    
test()
            """

        CompilerAssert.CompileLibraryAndVerifyILWithOptions [|"--optimize+";"--langversion:preview"|] source
            (fun verifier ->
                verifier.VerifyIL
                    [
                        "IL_0000:  newobj     instance void Module1/check"
                    ]
            )
#endif