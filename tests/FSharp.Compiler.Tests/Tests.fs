namespace FSharp.Compiler.Compilation.Tests

open System
open System.IO
open System.Collections.Immutable
open System.Collections.Generic
open System.Threading
open FSharp.Compiler.Compilation
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.Text
open NUnit.Framework



[<AutoOpen>]
module Helpers =

    let createTestModules name amount =
        [
            for i = 1 to amount do
                yield
                    sprintf
                    """
module TestModule%i =

    type %s () =

                    member val X = 1

                    member val Y = 2

                    member val Z = 3
                    
    let testFunction (x: %s) =
                    x.X + x.Y + x.Z
                    """ i name name
        ]
        |> List.reduce (+)

    // TODO: Unfortunately, we have to do this. F# requires a primary assembly. We should probably do the work to not have a primary assembly so we never have to do this.
    let defaultArgs = ["--targetprofile:netcore_private"]

    let createSource name amount =
        (sprintf """namespace Test.%s""" name) + createTestModules name amount

    let getMetadataReferences () =
        let currentReferencedAssemblies =
            let asmLocations =
                AppDomain.CurrentDomain.GetAssemblies()
                |> Array.choose (fun asm ->
                    if not asm.IsDynamic then
                        Some asm.Location
                    else
                        None
                )
            HashSet(asmLocations, StringComparer.OrdinalIgnoreCase)

        let metadataReferences =
            Directory.EnumerateFiles(Path.GetDirectoryName typeof<System.Object>.Assembly.Location)
            |> Seq.choose (fun filePath ->
                // This is indeed, quite hacky, but it works for now.
                if String.Equals (Path.GetExtension filePath, ".dll", StringComparison.OrdinalIgnoreCase) && 
                   ((Path.GetFileNameWithoutExtension filePath).StartsWith("System.Runtime.Numerics") || 
                    (Path.GetFileNameWithoutExtension filePath).StartsWith("System.Net.Requests") || 
                    (Path.GetFileNameWithoutExtension filePath).StartsWith("System.Net.WebClient") ||
                    currentReferencedAssemblies.Contains filePath) then
                    Some (PortableExecutableReference.CreateFromFile filePath)
                else
                    None
            )
            |> Seq.map (fun peReference -> FSharpMetadataReference.PortableExecutable peReference)
            |> ImmutableArray.CreateRange

        let fsharpCoreMetadataReference =
            PortableExecutableReference.CreateFromFile typeof<int list>.Assembly.Location
            |> FSharpMetadataReference.PortableExecutable

        metadataReferences.Add fsharpCoreMetadataReference

    let getSemanticModel (text: string) =
        let src = FSharpSource.FromText (text, "c:\\test1.fs")
        let c = FSharpCompilation.Create ("test", ImmutableArray.Create src, getMetadataReferences(), defaultArgs)
        c.GetSemanticModel src

    let createScriptAux (text: string) =
        let src = FSharpSource.FromText (text, "c:\\test1.fsx")
        FSharpCompilation.CreateScript ("test", src, getMetadataReferences(), defaultArgs), src

    let semanticModelScript text =
        let c, src = createScriptAux text
        c.GetSemanticModel src

    let runScriptAux (sm: FSharpSemanticModel) =
        let c = sm.Compilation

        use peStream = new MemoryStream()
        match c.Emit (peStream) with
        | Result.Ok emitResult ->
            
            let asm = System.Reflection.Assembly.Load(peStream.ToArray())
            asm.EntryPoint.Invoke(null, [||]) |> ignore

            let itNodeOpt =
                sm.SyntaxTree.GetRootNode().GetChildren()
                |> Seq.filter (fun node ->
                    match node.Kind with
                    | FSharpSyntaxNodeKind.ModuleOrNamespace _ -> true
                    | _ -> false
                )
                |> Seq.tryExactlyOne

            match itNodeOpt with
            | Some itNode ->
                let it = sm.GetSymbolInfo(itNode).Symbol.Value
                let itCompiledName = it.TryGetCompiledName().Value
                let parentTypeInfo = it.TryGetParentTypeInfo().Value

                let asm = System.Reflection.Assembly.Load(peStream.ToArray())
                asm.EntryPoint.Invoke(null, [||]) |> ignore
                let parentRuntimeType = asm.GetType(parentTypeInfo.CompiledName)
                let itRuntimeProperty = parentRuntimeType.GetProperty(itCompiledName)
                let value = itRuntimeProperty.GetMethod.Invoke(null, [||])

                Result.Ok (emitResult, value)
            | _ ->
                Result.Ok (emitResult, null)
        | Result.Error diags ->
            Result.Error diags

    let runScript (text: string) =
        semanticModelScript text
        |> runScriptAux

    let runScriptAndContinue (text1: string) (text2: string) =
        let c, src = createScriptAux text1

        let res =
            c.GetSemanticModel src
            |> runScriptAux

        let emitResult =
            match res with
            | Ok (er, _) -> er
            | _ -> failwith "no emit result"

        let src2 = FSharpSource.FromText (text2, "c:\\test2.fsx")
        let c2 = FSharpCompilation.CreateScript (emitResult, src2)
        c2.GetSemanticModel src2
        |> runScriptAux

[<TestFixture>]
type CompilationTests () =

    [<Test>]
    member __.``Find Symbol - Basic`` () =
        let textString = """
module TestModuleCompilationTest

type CompiltationTest<'T> () =

                member val X = 1

                member val Y = 2

                member val Z = 3
                    
let testFunction (x: CompilationTest) =
    x.X + x.Y + x.Z"""

        let semanticModel = getSemanticModel textString

        let position = textString.IndexOf("""CompiltationTest<'T> ()""")
        let symbol = semanticModel.TryGetEnclosingSymbol (position, CancellationToken.None)
        Assert.True (symbol.IsSome)

        let diagnostics = semanticModel.SyntaxTree.GetDiagnostics ()
        Assert.True (diagnostics.IsEmpty)

    [<Test>]
    member __.``Find Symbol - Basic - Speculative`` () =
        let textString = """
module TestModuleCompilationTest =

    type CompiltationTest<'T> () =

                    member val X = 1

                    member val Y = 2

                    member val Z = 3
                    
    let testFunction (x: CompilationTest<'T>) =
        x.X + x.Y + x.Z"""

        let semanticModel = getSemanticModel textString

        let position = textString.IndexOf("""x.X + x.Y + x.Z""")
        let token = (semanticModel.SyntaxTree.GetRootNode ()).FindToken position

        Assert.False (token.IsNone)
        Assert.True (token.IsIdentifier)

        let node = token.GetParentNode ()
        let symbol = semanticModel.TryGetEnclosingSymbol (position, CancellationToken.None)
        let speculativeSymbolInfo = semanticModel.GetSpeculativeSymbolInfo (position, node, CancellationToken.None)

        Assert.True (symbol.IsSome)
        Assert.True (speculativeSymbolInfo.Symbol.IsSome)

    [<Test>]
    member __.``Get Completion Symbols - Open Declaration`` () =
        let semanticModel = 
            getSemanticModel """
module CompilationTest.Test
open System.Collections
open System. 

let beef = obj ()
let x = beef.
let yopac = 1"""

        let symbols = semanticModel.GetCompletionSymbolsAsync (4, 13) |> Async.RunSynchronously
        Assert.False (symbols.IsEmpty)
        let symbols = semanticModel.GetCompletionSymbolsAsync (7, 14) |> Async.RunSynchronously
        Assert.False (symbols.IsEmpty)

    [<Test>]
    member __.``Syntax Tree - Find Token`` () =
        let textString = """
namespace Test
        
/// Doc comment
type Class1 (* inside comment *) () =
            
    // normal comment
    member val X = 1
        
    member val Y = 1
        
    member val Z = 1
        
"""         
        let semanticModel = getSemanticModel textString
        let syntaxTree = semanticModel.SyntaxTree
        let rootNode = semanticModel.SyntaxTree.GetRootNode ()

        let position = textString.IndexOf("Test")
        let token = rootNode.FindToken position

        Assert.False (token.IsNone)
        Assert.True (token.IsIdentifier)
        Assert.AreEqual ("Test", token.Value.Value)

        let token2 = rootNode.FindToken (position + 4)

        Assert.True token2.IsIdentifier
        Assert.AreEqual ("Test", token2.Value.Value)

        //

        let position = textString.IndexOf("Class1")
        let token3 = rootNode.FindToken position

        Assert.True token3.IsIdentifier
        Assert.AreEqual ("Class1", token3.Value.Value)

    [<Test>]
    member __.``Syntax Tree - String Token`` () =
        let textString = """
namespace Test
        
module App =

    let x = "hello
there
    "

    let y = 1
"""         
        let semanticModel = getSemanticModel textString

        let text = "hello"
        let position = textString.IndexOf(text)
        let syntaxTree = semanticModel.SyntaxTree

        let rootNode = syntaxTree.GetRootNode ()
        let token = rootNode.FindToken position

        Assert.IsTrue token.IsString

    [<Test>]
    member __.``Script Test - Simple`` () =
        let text = """
let x = 1 + 1
        """

        let c = semanticModelScript text
        let diags = c.GetDiagnostics()
        Assert.True (diags.IsEmpty, sprintf "%A" diags)

    [<Test>]
    member __.``Script Test - Reference Script`` () =

        let tmpPath = Path.GetTempFileName()
        let tmpFsx = Path.ChangeExtension(tmpPath, ".fsx")
        try
            let refText = """
type FromAnotherScript () = class end
"""
            File.WriteAllText(tmpFsx, refText)
            let text = sprintf """
#load @"%s"

let x = FromAnotherScript ()
                               """ tmpFsx

            let c = semanticModelScript text
            let diags = c.Compilation.GetSyntaxAndSemanticDiagnostics()
            Assert.True (diags.IsEmpty, sprintf "%A" diags)
        finally
            try File.Delete tmpPath with | _ -> ()
            try File.Delete tmpFsx with | _ -> ()

    [<Test>]
    member __.``Script Test - Simple Emit`` () =
        let text = """
let x = 1 + 1
        """

        let c = semanticModelScript text
        
        use peStream = new MemoryStream()
        match c.Compilation.Emit (peStream) with
        | Ok _ -> ()
        | Error diags -> Assert.Fail (sprintf "%A" diags)

    [<Test>]
    member __.``Script Test - Simple Evaluation`` () =
        match runScript "1 + 1" with
        | Ok (_, value) -> Assert.AreEqual (2, value)
        | Error diags -> Assert.Fail (sprintf "%A" diags)


    [<Test>]
    member __.``Script Test - Simple Evaluation - 2`` () =
        let res =
            runScript
                """
1 + 1
5 + 5
                """
        match res with
        | Ok (_, value) -> Assert.AreEqual (10, value)
        | Error diags -> Assert.Fail (sprintf "%A" diags)

    [<Test>]
    member __.``Script Test - Simple Evaluation - 3`` () =
        let res =
            runScript
                """
let it = 1 + 1
5 + 5
                """
        match res with
        | Ok (_, value) -> Assert.AreEqual (10, value)
        | Error diags -> Assert.Fail (sprintf "%A" diags)

    [<Test>]
    member __.``Script Test - Simple Evaluation - 4`` () =
        let res =
            runScript
                """
let it = 1 + 1
                """
        match res with
        | Ok (_, value) -> Assert.AreEqual (null, value)
        | Error diags -> Assert.Fail (sprintf "%A" diags)

    [<Test>]
    member __.``Script Test - Simple Evaluation - 5`` () =
        let res =
            runScript
                """
1 + 1
type C () = class end
                """
        match res with
        | Ok (_, value) -> Assert.AreEqual (null, value)
        | Error diags -> Assert.Fail (sprintf "%A" diags)

    [<Test>]
    member __.``Script Test - Evaluation with continue`` () =
        let res =
            runScriptAndContinue
                """
module Doot123

let y = 1 + 1
                """
                """
open Doot123
y
                """
        match res with
        | Ok (_, value) -> Assert.AreEqual (2, value)
        | Error diags -> Assert.Fail (sprintf "%A" diags)

//[<TestFixture>]
//type UtilitiesTest () =

//    [<Test>]
//    member __.``Lru Cache Validation`` () =
//        let lru = LruCache<int, obj> (5, Collections.Generic.EqualityComparer.Default)

//        Assert.Throws<ArgumentNullException> (fun () -> lru.Set (1, null)) |> ignore

//        let o1 = obj ()
//        lru.Set (1, o1)

//        Assert.AreEqual (1, lru.Count)

//        match lru.TryGetValue 1 with
//        | ValueSome o -> Assert.AreSame (o1, o)
//        | _ -> failwith "couldn't find object in Lru"

//        lru.Set (1, obj ())

//        match lru.TryGetValue 1 with
//        | ValueSome o -> Assert.AreNotSame (o1, o)
//        | _ -> failwith "couldn't find object in Lru"

//        lru.Set (2, obj ())
//        lru.Set (3, obj ())
//        lru.Set (4, obj ())
//        lru.Set (5, obj ())

//        Assert.AreEqual (5, lru.Count)

//        lru.Set (6, obj ())

//        Assert.AreEqual (5, lru.Count)

//        Assert.True ((lru.TryGetValue 1).IsNone)

//        lru.TryGetValue 2 |> ignore

//        lru.Set (7, obj ())

//        // Because we tried to acess 2 before setting 7, it put 3 at the back.
//        Assert.True ((lru.TryGetValue 3).IsNone)

//        Assert.True ((lru.TryGetValue 2).IsSome)

//        Assert.AreEqual (5, lru.Count)

//        Assert.True ((lru.TryGetValue 1).IsNone)
//        Assert.True ((lru.TryGetValue 2).IsSome)
//        Assert.True ((lru.TryGetValue 3).IsNone)
//        Assert.True ((lru.TryGetValue 4).IsSome)
//        Assert.True ((lru.TryGetValue 5).IsSome)
//        Assert.True ((lru.TryGetValue 6).IsSome)

//    [<Test>]
//    member __.``Mru Weak Cache Validation`` () =
//        let mru = MruWeakCache<int, obj> (5, 10, Collections.Generic.EqualityComparer.Default)

//        Assert.Throws<ArgumentNullException> (fun () -> mru.Set (1, null)) |> ignore

//        let stackF () =
//            let o1 = obj ()
//            mru.Set (1, o1)

//            Assert.AreEqual (1, mru.Count)
//            Assert.AreEqual (0, mru.WeakReferenceCount)

//            match mru.TryGetValue 1 with
//            | ValueSome o -> Assert.AreSame (o1, o)
//            | _ -> failwith "couldn't find object in mru"

//            mru.Set (2, obj ())
//            mru.Set (3, obj ())
//            mru.Set (4, obj ())

//            let o5 = obj ()
//            mru.Set (5, o5)

//            Assert.AreEqual (5, mru.Count)
//            Assert.AreEqual (0, mru.WeakReferenceCount)

//            let o6 = obj ()
//            mru.Set (6, o6)

//            Assert.AreEqual (5, mru.Count)
//            Assert.AreEqual (1, mru.WeakReferenceCount)

//            Assert.True ((mru.TryGetValue 5) = ValueSome o5)

//            // trying to get 6, evicts 5 out of the cache and into the weak reference cache.
//            Assert.True ((mru.TryGetValue 6) = ValueSome o6)

//            Assert.AreEqual (5, mru.Count)
//            Assert.AreEqual (1, mru.WeakReferenceCount)

//        stackF ()
//        GC.Collect ()

//        Assert.AreEqual (5, mru.Count)
//        Assert.AreEqual (1, mru.WeakReferenceCount)

//        Assert.True ((mru.TryGetValue 5).IsNone)
//        Assert.True ((mru.TryGetValue 6).IsSome)
//        Assert.True ((mru.TryGetValue 4).IsSome)
//        Assert.True ((mru.TryGetValue 3).IsSome)
//        Assert.True ((mru.TryGetValue 2).IsSome)
//        Assert.True ((mru.TryGetValue 1).IsSome)

//        Assert.AreEqual (5, mru.Count)
//        Assert.AreEqual (0, mru.WeakReferenceCount)
//        ()
