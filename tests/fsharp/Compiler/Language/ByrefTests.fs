// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

namespace FSharp.Compiler.UnitTests

open NUnit.Framework
open FSharp.Compiler.SourceCodeServices

[<TestFixture>]
module ByrefTests =

    [<Test>]
    let ``No defensive copy on .NET struct`` () =
        CompilerAssert.Pass
            """
open System
open System.Runtime.CompilerServices

let f (x: DateTime) = x.ToLocalTime()
let f2 () =
    let x = DateTime.Now
    x.ToLocalTime()

[<Extension; AbstractClass; Sealed>]
type Extensions =

    [<Extension>]
    static member Test(x: inref<DateTime>) = &x

    [<Extension>]
    static member Test2(x: byref<DateTime>) = &x

let test (x: inref<DateTime>) =
    x.Test()

let test2 (x: byref<DateTime>) =
    x.Test2()

let test3 (x: byref<DateTime>) =
    x.Test()

let test4 () =
    DateTime.Now.Test()

let test5 (x: inref<DateTime>) =
    &x.Test()

let test6 () =
    DateTime.Now.Test().Test().Test()
            """

    [<Test>]
    let ``Extension method scope errors`` () =
        CompilerAssert.TypeCheckWithErrors
            """
open System
open System.Runtime.CompilerServices

[<Extension; AbstractClass; Sealed>]
type Extensions =

    [<Extension>]
    static member Test(x: inref<DateTime>) = &x

let f1 () =
    &DateTime.Now.Test()

let f2 () =
    let result =
        let dt = DateTime.Now
        &dt.Test()
    result

let f3 () =
    Extensions.Test(let dt = DateTime.Now in &dt)

let f4 () =
    let dt = DateTime.Now
    &Extensions.Test(&dt)

let f5 () =
    &Extensions.Test(let dt = DateTime.Now in &dt)
            """
            [|
                (
                    FSharpErrorSeverity.Error,
                    3228,
                    (12, 6, 12, 25),
                    "The address of a value returned from the expression cannot be used at this point. This is to ensure the address of the local value does not escape its scope."
                )
                (
                    FSharpErrorSeverity.Error,
                    3228,
                    (17, 10, 17, 19),
                    "The address of a value returned from the expression cannot be used at this point. This is to ensure the address of the local value does not escape its scope."
                )
                (
                    FSharpErrorSeverity.Error,
                    3228,
                    (21, 5, 21, 50),
                    "The address of a value returned from the expression cannot be used at this point. This is to ensure the address of the local value does not escape its scope."
                )
                (
                    FSharpErrorSeverity.Error,
                    3228,
                    (25, 6, 25, 26),
                    "The address of a value returned from the expression cannot be used at this point. This is to ensure the address of the local value does not escape its scope."
                )
                (
                    FSharpErrorSeverity.Error,
                    3228,
                    (28, 6, 28, 51),
                    "The address of a value returned from the expression cannot be used at this point. This is to ensure the address of the local value does not escape its scope."
                )
            |]

// TODO: A better way to test the ones below are to use a custom struct in C# code that contains explicit use of their "readonly" keyword.
#if NETCOREAPP
    // NETCORE makes DateTime a readonly struct; therefore, it should not error.
    [<Test>]
    let ``No defensive copy on .NET struct - netcore`` () =
        CompilerAssert.Pass
            """
open System
let f (x: inref<DateTime>) = x.ToLocalTime()
let f2 () =
    let x = DateTime.Now
    let y = &x
    y.ToLocalTime()
let f3 (x: inref<DateTime>) = &x
let f4 (x: inref<DateTime>) =
    (f3 &x).ToLocalTime()

open System.Runtime.CompilerServices
[<Extension; AbstractClass; Sealed>]
type Extensions =

    [<Extension>]
    static member Test(x: inref<DateTime>) = &x

let test1 () =
    DateTime.Now.Test().Date

let test2 () =
    DateTime.Now.Test().Test().Date.Test().Test().Date.Test()
            """

    [<Test>]
    let ``Dereference return optimization``() =
        let source =
            """
module Test

open System.Runtime.CompilerServices

[<Struct>]
type S =

    val X : int

[<Extension; AbstractClass; Sealed>]
type Extensions =

    [<Extension; MethodImpl(MethodImplOptions.NoInlining)>]
    static member Test(x: inref<S>) = &x

let doot (dt: inref<S>) =
    dt.Test().Test()

[<MethodImpl(MethodImplOptions.NoInlining)>]
let test (dt: S) = dt

let test2 (dt: inref<S>) =
    let z = dt.Test().Test()
    test z

let stress () =
    let x = S()
    for i = 1 to 1000000 do
        test2 &x |> ignore
            """

        CompilerAssert.CompileLibraryAndVerifyIL source
            (fun verifier ->
                verifier.VerifyIL
                            [
                            """.method public static valuetype Test/S 
              doot([in] valuetype Test/S& dt) cil managed
      {
        .param [1]
        .custom instance void [System.Runtime]System.Runtime.CompilerServices.IsReadOnlyAttribute::.ctor() = ( 01 00 00 00 ) 
        
        .maxstack  8
        IL_0000:  ldarg.0
        IL_0001:  call       valuetype Test/S& modreq([System.Runtime]System.Runtime.InteropServices.InAttribute) Test/Extensions::Test(valuetype Test/S&)
        IL_0006:  call       valuetype Test/S& modreq([System.Runtime]System.Runtime.InteropServices.InAttribute) Test/Extensions::Test(valuetype Test/S&)
        IL_000b:  ldobj      Test/S
        IL_0010:  ret
      }"""
                            ]
            )
#else
    // Note: Currently this is assuming NET472. That may change which might break these tests. Consider using custom C# code.
    [<Test>]
    let ``Defensive copy on .NET struct for inref`` () =
        CompilerAssert.TypeCheckWithErrors
            """
open System
let f (x: inref<DateTime>) = x.ToLocalTime()
let f2 () =
    let x = DateTime.Now
    let y = &x
    y.ToLocalTime()
let f3 (x: inref<DateTime>) = &x
let f4 (x: inref<DateTime>) =
    (f3 &x).ToLocalTime()

open System.Runtime.CompilerServices
[<Extension; AbstractClass; Sealed>]
type Extensions =

    [<Extension>]
    static member Test(x: inref<DateTime>) = &x

let test1 () =
    DateTime.Now.Test().Date
            """
            [|
                (
                    FSharpErrorSeverity.Warning,
                    52,
                    (3, 30, 3, 45),
                    "The value has been copied to ensure the original is not mutated by this operation or because the copy is implicit when returning a struct from a member and another member is then accessed"
                )
                (
                    FSharpErrorSeverity.Warning,
                    52,
                    (7, 5, 7, 20),
                    "The value has been copied to ensure the original is not mutated by this operation or because the copy is implicit when returning a struct from a member and another member is then accessed"
                )
                (
                    FSharpErrorSeverity.Warning,
                    52,
                    (10, 5, 10, 26),
                    "The value has been copied to ensure the original is not mutated by this operation or because the copy is implicit when returning a struct from a member and another member is then accessed"
                )
                (
                    FSharpErrorSeverity.Warning,
                    52,
                    (20, 5, 20, 29),
                    "The value has been copied to ensure the original is not mutated by this operation or because the copy is implicit when returning a struct from a member and another member is then accessed"
                )
            |]
#endif