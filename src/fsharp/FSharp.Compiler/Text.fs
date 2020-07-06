module FSharp.Compiler.TextNew

open System
open System.IO

[<Struct>]
type FSharpTextSpan(start: int, length: int) =

    member _.Start = start

    member _.Length = length

type FSharpTextSpan with

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

    abstract GetLineString : lineIndex: int -> string

    abstract GetLineCount : unit -> int

    abstract GetLastCharacterPosition : unit -> int * int

    abstract GetSubTextString : start: int * length: int -> string

    abstract SubTextEquals : target: string * startIndex: int -> bool

    abstract Length : int

    abstract ContentEquals : sourceText: FSharpSourceText -> bool

    abstract CopyTo : sourceIndex: int * destination: char [] * destinationIndex: int * count: int -> unit

and FSharpSubText (text: FSharpSourceText, span: FSharpTextSpan) =

    do
        if 

    member _.Text = text

    member _.Span = span
    

[<Sealed>]
type StringText internal (str: string) =
    inherit FSharpSourceText()

    let getLines (str: string) =
        use reader = new StringReader(str)
        [|
        let mutable line = reader.ReadLine()
        while not (isNull line) do
            yield line
            line <- reader.ReadLine()
        if str.EndsWith("\n", StringComparison.Ordinal) then
            // last trailing space not returned
            // http://stackoverflow.com/questions/19365404/stringreader-omits-trailing-linebreak
            yield String.Empty
        |]

    let getLines =
        // This requires allocating and getting all the lines.
        // However, likely whoever is calling it is using a different implementation of ISourceText
        // So, it's ok that we do this for now.
        lazy getLines str

    member _.String = str

    override _.Item with get index = str.[index]

    override _.GetLastCharacterPosition() =
        let lines = getLines.Value
        if lines.Length > 0 then
            (lines.Length, lines.[lines.Length - 1].Length)
        else
            (0, 0)

    override _.GetLineString(lineIndex) = 
        getLines.Value.[lineIndex]

    override _.GetLineCount() = getLines.Value.Length

    override _.GetSubTextString(start, length) = 
        str.Substring(start, length)

    override _.SubTextEquals(target, startIndex) =
        if startIndex < 0 || startIndex >= str.Length then
            invalidArg "startIndex" "Out of range."

        if String.IsNullOrEmpty(target) then
            invalidArg "target" "Is null or empty."

        let lastIndex = startIndex + target.Length
        if lastIndex <= startIndex || lastIndex >= str.Length then
            invalidArg "target" "Too big."

        str.IndexOf(target, startIndex, target.Length) <> -1              

    override _.Length = str.Length

    override this.ContentEquals(sourceText) =
        match sourceText with
        | :? StringText as sourceText when sourceText = this || sourceText.String = str -> true
        | _ -> false

    override _.CopyTo(sourceIndex, destination, destinationIndex, count) =
            str.CopyTo(sourceIndex, destination, destinationIndex, count)

