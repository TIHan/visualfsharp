// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

namespace FSharp.Compiler.UnitTests

open NUnit.Framework
open FSharp.Compiler.SourceCodeServices

[<TestFixture>]
module RecursiveBindingTests =

    [<Test>]
    let ``Generic recursive function should typecheck without issue`` () =
        CompilerAssert.Pass
            """
let rec f() = g<obj>()
and g<'T>() = 42
            """

    [<Test>]
    let ``Generic recursive function with constraint should typecheck without issue`` () =
        CompilerAssert.Pass
            """
let rec f() = g<obj>()
and g<'T when 'T : not struct>() = 42
            """

    [<Test>]
    let ``Generic recursive function with constraint should typecheck with an error`` () =
        CompilerAssert.TypeCheckSingleError
            """
let rec f() = g<int>()
and g<'T when 'T : not struct>() = 42
            """
            FSharpErrorSeverity.Error
            193
            (2, 15, 2, 16)
            "A generic construct requires that the type 'int' have reference semantics, but it does not, i.e. it is a struct"

    [<Test>]
    let ``Generic recursive function should typecheck with a warning about the type argument being less generic`` () =
        CompilerAssert.TypeCheckSingleError
            """
let rec f() = g<'T>()
and g<'T>() = 42
            """
            FSharpErrorSeverity.Warning
            64
            (2, 17, 2, 19)
            "This construct causes code to be less generic than indicated by the type annotations. The type variable 'T has been constrained to be type 'obj'."

    [<Test>]
    let ``Generic recursive function with constraint should typecheck with a warning about the type argument being less generic`` () =
        // Note: The range on this warning is not on the type argument, but on the function itself.
        CompilerAssert.TypeCheckSingleError
            """
let rec f() = g<'T>()
and g<'T when 'T : struct>() = 42
            """
            FSharpErrorSeverity.Warning
            64
            (2, 15, 2, 16)
            "This construct causes code to be less generic than indicated by the type annotations. The type variable 'T has been constrained to be type 'int'."

    [<Test>]
    let ``Generic static member method should typecheck without issue when it is a recursive definition`` () =
        CompilerAssert.Pass
            """
type C() =
    static member F() = C.G<obj>()
    static member G<'T>() = 42
            """