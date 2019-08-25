namespace FSharp.Compiler.Compilation

open System.IO
open System.Threading
open System.Collections.Immutable
open Microsoft.CodeAnalysis

type [<RequireQualifiedAccess>] FSharpMetadataReference =
    | PortableExecutable of PortableExecutableReference
    | FSharpCompilation of FSharpCompilation
    
and [<NoEquality;NoComparison;Sealed>] FSharpEmitResult

and [<Sealed>] FSharpCompilation =

    member ReplaceSource: oldSrc: FSharpSource * newSrc: FSharpSource -> FSharpCompilation

    member GetSemanticModel: FSharpSource -> FSharpSemanticModel

    member GetSyntaxTree: FSharpSource -> FSharpSyntaxTree

    member GetSyntaxAndSemanticDiagnostics: ?ct: CancellationToken -> ImmutableArray<Diagnostic>

    member Emit: peStream: Stream * ?pdbStream: Stream * ?ct: CancellationToken -> Result<FSharpEmitResult, ImmutableArray<Diagnostic>>

    static member Create: assemblyName: string * srcs: ImmutableArray<FSharpSource> * metadataReferences: ImmutableArray<FSharpMetadataReference> * ?args: string list -> FSharpCompilation

    static member CreateScript: assemblyName: string * scriptSnapshot: FSharpSource * metadataReferences: ImmutableArray<FSharpMetadataReference> * ?args: string list -> FSharpCompilation

    static member CreateScript: emitResult: FSharpEmitResult * scriptSnapshot: FSharpSource * ?additionalMetadataReferences: ImmutableArray<FSharpMetadataReference> -> FSharpCompilation

[<AutoOpen>]
module FSharpSemanticModelExtensions =

    type FSharpSemanticModel with

        member Compilation: FSharpCompilation
