namespace Microsoft.VisualStudio.FSharp.Editor

open System.Threading
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.Text
open FSharp.Compiler.Ast
open FSharp.Compiler.Range
open FSharp.Compiler.SourceCodeServices

[<Sealed>]
type internal FSharpProjectOptionsManager =

    member GetCompilationDefines : document: Document -> string list

    member GetMatchingBraces : document: Document * cancellationToken : CancellationToken -> Async<(range * range) list>

    member TryParseAndCheckDocument : document: Document * cancellationToken: CancellationToken -> Async<(FSharpParseFileResults * ParsedInput * FSharpCheckFileResults * SourceText) option>
