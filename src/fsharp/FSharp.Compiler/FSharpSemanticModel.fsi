﻿namespace FSharp.Compiler.Compilation

open System.Threading
open System.Collections.Immutable
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.Text
open FSharp.Compiler.Compilation.Utilities
open FSharp.Compiler.Compilation.IncrementalChecker
open FSharp.Compiler.NameResolution

[<AbstractClass>]
type FSharpSymbol =

    abstract Name: string

[<AbstractClass>]
type TypeSymbol =
    inherit FSharpSymbol

    abstract IsValueType: bool

[<Sealed;Class>]
type NamedTypeSymbol =
    inherit TypeSymbol

    member FullName: string

[<AbstractClass>]
type BindingSymbol =
    inherit FSharpSymbol

    member IsMutable: bool

    member Type: TypeSymbol

[<Sealed;Class>]
type LocalValueSymbol =
    inherit BindingSymbol

[<Sealed;Class>]
type ModuleValueSymbol =
    inherit BindingSymbol

[<Sealed>]
type FSharpSymbolInfo =

    /// The symbol that was referred to by the syntax node, if any. If None is returned, it may
    /// still be that case that we have one or more "best guesses" as to what symbol was
    /// intended. These best guesses are available via the CandidateSymbols property.
    member Symbol: FSharpSymbol option

    member CandidateSymbols: ImmutableArray<FSharpSymbol>

    member GetAllSymbols: unit -> ImmutableArray<FSharpSymbol>

[<Sealed>]
type FSharpSemanticModel =

    internal new: FSharpSource * CancellableLazy<IncrementalChecker> * compilationObj: obj -> FSharpSemanticModel

    member internal GetToolTipTextAsync: line: int * column: int -> Async<FSharp.Compiler.SourceCodeServices.FSharpToolTipText<FSharp.Compiler.SourceCodeServices.Layout> option>

    member GetCompletionSymbolsAsync: line: int * column: int -> Async<InternalFSharpSymbol list>

    member GetSymbolInfo: node: FSharpSyntaxNode * ?ct: CancellationToken -> FSharpSymbolInfo

    member TryGetEnclosingSymbol: position: int * ?ct: CancellationToken -> FSharpSymbol option

    member GetSpeculativeSymbolInfo: position: int * node: FSharpSyntaxNode * ?ct: CancellationToken -> FSharpSymbolInfo

    member LookupSymbols: position: int * ct: CancellationToken -> ImmutableArray<FSharpSymbol>

    member SyntaxTree: FSharpSyntaxTree

    member internal CompilationObj: obj

    member GetSyntaxDiagnostics: ?ct: CancellationToken -> ImmutableArray<Diagnostic>

    member GetSemanticDiagnostics: ?ct: CancellationToken -> ImmutableArray<Diagnostic>

    member GetDiagnostics: ?ct: CancellationToken -> ImmutableArray<Diagnostic>