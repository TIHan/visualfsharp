namespace FSharp.Compiler.UnitTests

open System
open System.IO
open FSharp.Compiler.Interactive.Shell
open Xunit
open FSharp.Test.Utilities
open FSharp.CodeAnalysis.SyntaxTree

#nowarn "57"

[<Collection("SingleThreaded")>]
module CodeAnalysisSyntaxTreeTests =

    [<Fact>]
    let ``Module identifier`` () =
        let syntaxTree = FSharpSyntaxTree.Create(CompilerAssert.TryGetParsedSource("""module Test""").Value)
        let syntaxTree = syntaxTree :?> FSharpSyntaxTreeImplementation
        let name = syntaxTree.Implementation.ModuleOrNamespaces.[0].Name :?> FSharpSyntaxIdentifier
        Assert.Equal("Test", name.IdentifierToken.Text)

    [<Fact>]
    let ``Module qualified name`` () =
        let syntaxTree = FSharpSyntaxTree.Create(CompilerAssert.TryGetParsedSource("""module SomeNamespace.Test""").Value)
        let syntaxTree = syntaxTree :?> FSharpSyntaxTreeImplementation
        let name = syntaxTree.Implementation.ModuleOrNamespaces.[0].Name :?> FSharpSyntaxQualifiedName
        Assert.Equal("Test", name.Right.IdentifierToken.Text)
        Assert.Equal("SomeNamespace", (name.Left :?> FSharpSyntaxIdentifier).IdentifierToken.Text)

    [<Fact>]
    let ``Module qualified name with more names`` () =
        let syntaxTree = FSharpSyntaxTree.Create(CompilerAssert.TryGetParsedSource("""module SomeNamespace.AnotherNamespace.Test""").Value)
        let syntaxTree = syntaxTree :?> FSharpSyntaxTreeImplementation
        let name = syntaxTree.Implementation.ModuleOrNamespaces.[0].Name :?> FSharpSyntaxQualifiedName
        Assert.Equal("Test", name.Right.IdentifierToken.Text)
        Assert.Equal("AnotherNamespace", (name.Left :?> FSharpSyntaxQualifiedName).Right.IdentifierToken.Text)
        Assert.Equal("SomeNamespace", ((name.Left :?> FSharpSyntaxQualifiedName).Left :?> FSharpSyntaxIdentifier).IdentifierToken.Text)

    [<Fact>]
    let ``Module qualified name with more names - correct tokens`` () =
        let syntaxTree = FSharpSyntaxTree.Create(CompilerAssert.TryGetParsedSource("""module SomeNamespace.AnotherNamespace.Test""").Value)
        let tokens = 
            syntaxTree.GetAllTokens() 
            |> Seq.sortBy (fun x -> struct(x.Range.StartLine, x.Range.StartColumn))
            |> Array.ofSeq
        Assert.Equal(tokens.[0].Kind, FSharpSyntaxTokenKind.Module)
        Assert.Equal(tokens.[1].Kind, FSharpSyntaxTokenKind.Identifier)
        Assert.Equal(tokens.[2].Kind, FSharpSyntaxTokenKind.Dot)
        Assert.Equal(tokens.[3].Kind, FSharpSyntaxTokenKind.Identifier)
        Assert.Equal(tokens.[4].Kind, FSharpSyntaxTokenKind.Dot)
        Assert.Equal(tokens.[5].Kind, FSharpSyntaxTokenKind.Identifier)