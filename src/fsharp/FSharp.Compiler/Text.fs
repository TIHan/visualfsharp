module FSharp.Compiler.TextNew

open System
open System.IO
open System.Threading
open System.Linq
open System.Collections.Immutable
open FSharp.Compiler.Internal.ArrayExtensions

[<Struct>]
type FSharpTextSpan(start: int, length: int) =

    member _.Start = start

    member _.Length = length

    member this.End = this.Start + this.Length

    member this.Contains(span: FSharpTextSpan) =
        span.Start >= this.Start && span.End <= this.End

    member this.Contains(position: int) =
        uint (position - this.Start) < uint this.Length 

    member this.OverlapsWith(span: FSharpTextSpan) =
        max this.Start span.Start < min this.End span.End

    member this.IntersectsWith(span: FSharpTextSpan) =
        span.Start <= this.End && span.End >= this.Start

    member this.IntersectsWith(position: int) =
        uint (position - this.Start) <= uint this.Length

    member this.TryOverlap(span: FSharpTextSpan, result: outref<FSharpTextSpan>) =
        let overlapStart = max this.Start span.Start
        let overlapEnd = min this.End span.End
        if overlapStart < overlapEnd then
            result <- FSharpTextSpan(overlapStart, overlapEnd)
            true
        else
            result <- FSharpTextSpan()
            false

    member inline this.TryOverlap(span: FSharpTextSpan) =
        let mutable result = Unchecked.defaultof<_>
        if this.TryOverlap(span, &result) then
            Some result
        else
            None

    member this.TryIntersect(span: FSharpTextSpan, result: outref<FSharpTextSpan>) =
        let intersectStart = max this.Start span.Start
        let intersectEnd = min this.End span.End
        if intersectStart <= intersectEnd then
            result <- FSharpTextSpan(intersectStart, intersectEnd)
            true
        else
            result <- FSharpTextSpan()
            false

    member inline this.TryIntersect(span: FSharpTextSpan) =
        let mutable result = Unchecked.defaultof<_>
        if this.TryIntersect(span, &result) then
            Some result
        else
            None

    member this.IsEmpty = this.Length = 0

[<AbstractClass>]
type FSharpSourceText internal () = 

    abstract Item : int -> char with get

    abstract Lines : FSharpTextLineCollection

    abstract GetSubText : start: int * length: int -> FSharpSourceText

    abstract Length : int

    abstract ContentEquals : sourceText: FSharpSourceText -> bool

    abstract CopyTo : sourceIndex: int * destination: char[] * destinationIndex: int * count: int -> unit

and [<Struct>] FSharpTextLine internal (text: FSharpSourceText, span: FSharpTextSpan) =
    
    member _.LineNumber = text.Lines.IndexOf span.Start

    member _.Start = span.Start

    member _.End = span.End
    
and [<Sealed>] FSharpTextLineCollection internal (text: FSharpSourceText, lineStarts: int []) =

    member _.Count = lineStarts.Length

    member _.Item with get index =
        let start = lineStarts.[index]
        let endIncludingLineBreak =
            if index = lineStarts.Length - 1 then
                text.Length - 1
            else
                lineStarts.[index + 1]             
        FSharpTextLine(text, FSharpTextSpan(start, endIncludingLineBreak - start))

    member _.IndexOf(position: int) =
        if position < 0 || position > text.Length then
            ArgumentOutOfRangeException "position"
            |> raise

        // Binary search to find the right line
        // if no lines start exactly at position, round to the left
        // EoF position will map to the last line.
        let lineNumber = lineStarts.BinarySearch(position);
        if lineNumber < 0 then
            (~~~lineNumber) - 1
        else
            lineNumber

[<Sealed>]
type private StringText (str: ReadOnlyMemory<char>) as this =
    inherit FSharpSourceText()

    static let getLineStarts (str: ReadOnlyMemory<char>) =
        [|
            let mutable position = 0
            for i = 0 to str.Length - 1 do
                let c = str.Span.[i]
                if c = '\n' then
                    yield position
                position <- position + 1
        |]

    let mutable lazyLines = Unchecked.defaultof<FSharpTextLineCollection>
    let getLines () =
        FSharpTextLineCollection(this, getLineStarts str)

    member _.String = str

    override _.Item with get index = str.Span.[index]

    override _.Lines =
        if lazyLines = Unchecked.defaultof<FSharpTextLineCollection> then
            Interlocked.CompareExchange(&lazyLines, getLines (), Unchecked.defaultof<_>)
        else
            lazyLines

    override _.GetSubText(start, length) = 
        StringText(str.Slice(start, length)) :> FSharpSourceText      

    override _.Length = str.Length

    override this.ContentEquals(sourceText) =
        match sourceText with
        | :? StringText as sourceText when sourceText = this || sourceText.String.Equals str -> true
        | _ -> false

    override _.CopyTo(sourceIndex, destination, destinationIndex, count) =
        str.Slice(sourceIndex).CopyTo(Memory(destination, destinationIndex, count))


let charBufferSize = 32 * 1024
let charBufferCount = 5
let largeObjectHeapLimitInChars = 40 * 1024 // 40KB

let private readChunks (reader: TextReader) =
    let chunks = ImmutableArray.CreateBuilder()
    while reader.Peek() <> -1 do
        let mutable chunk = Array.zeroCreate<char> largeObjectHeapLimitInChars

        let charsRead = reader.ReadBlock(chunk, 0, chunk.Length)
        if charsRead = 0 then ()
        else

        if charsRead < chunk.Length then
            Array.Resize(&chunk, charsRead)

        chunks.Add chunk

[<Sealed>]
type private LargeText (chunks: ImmutableArray<char>) =

    static let largeObjectHeapLimitInChars = 40 * 1024 // 40KB
    
    static let readChunks (reader: TextReader) =
        let chunks = ImmutableArray.CreateBuilder()
        while reader.Peek() <> -1 do
            let mutable chunk = Array.zeroCreate<char> largeObjectHeapLimitInChars
    
            let charsRead = reader.ReadBlock(chunk, 0, chunk.Length)
            if charsRead = 0 then ()
            else
    
            if charsRead < chunk.Length then
                Array.Resize(&chunk, charsRead)
    
            chunks.Add chunk

    static member Create()