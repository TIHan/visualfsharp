// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

module internal FSharp.Compiler.Lexhelp

open System
open System.Text

open Internal.Utilities
open Internal.Utilities.Text.Lexing

open FSharp.Compiler
open FSharp.Compiler.AbstractIL.Internal
open FSharp.Compiler.AbstractIL.Internal.Library
open FSharp.Compiler.ErrorLogger
open FSharp.Compiler.Lib
open FSharp.Compiler.ParseHelpers
open FSharp.Compiler.Parser
open FSharp.Compiler.PrettyNaming
open FSharp.Compiler.Range
open FSharp.Compiler.SyntaxTree
open FSharp.Compiler.XmlDoc

/// The "mock" filename used by fsi.exe when reading from stdin.
/// Has special treatment by the lexer, i.e. __SOURCE_DIRECTORY__ becomes GetCurrentDirectory()
let stdinMockFilename = "stdin" 

/// Lexer args: status of #light processing.  Mutated when a #light
/// directive is processed. This alters the behaviour of the lexfilter.
[<Sealed>]
type LightSyntaxStatus(initial:bool,warn:bool) = 
    let mutable status = None
    member x.Status 
       with get() = match status with None -> initial | Some v -> v
       and  set v = status <- Some(v)
    member x.ExplicitlySet = status.IsSome
    member x.WarnOnMultipleTokens = warn
    

/// Manage lexer resources (string interning)
[<Sealed>]
type LexResourceManager(?capacity: int) =
    let strings = new System.Collections.Generic.Dictionary<string, Parser.token>(defaultArg capacity 1024)
    member x.InternIdentifierToken(s) = 
        match strings.TryGetValue s with
        | true, res -> res
        | _ ->
            let res = IDENT s
            strings.[s] <- res
            res

/// Lexer parameters 
type lexargs =  
    { defines: string list
      mutable ifdefStack: LexerIfdefStack
      resourceManager: LexResourceManager
      lightSyntaxStatus : LightSyntaxStatus
      errorLogger: ErrorLogger
      applyLineDirectives: bool
      pathMap: PathMap }

/// possible results of lexing a long Unicode escape sequence in a string literal, e.g. "\U0001F47D",
/// "\U000000E7", or "\UDEADBEEF" returning SurrogatePair, SingleChar, or Invalid, respectively
type LongUnicodeLexResult =
    | SurrogatePair of uint16 * uint16
    | SingleChar of uint16
    | Invalid

let mkLexargs (_filename, defines, lightSyntaxStatus, resourceManager, ifdefStack, errorLogger, pathMap:PathMap) =
    { defines = defines
      ifdefStack= ifdefStack
      lightSyntaxStatus=lightSyntaxStatus
      resourceManager=resourceManager
      errorLogger=errorLogger
      applyLineDirectives=true
      pathMap=pathMap }

/// Register the lexbuf and call the given function
let reusingLexbufForParsing lexbuf f = 
    use unwindBuildPhase = PushThreadBuildPhaseUntilUnwind BuildPhase.Parse
    LexbufLocalXmlDocStore.ClearXmlDoc lexbuf
    try
      f () 
    with e ->
      raise (WrappedError(e, (try lexbuf.LexemeRange with _ -> range0)))

let resetLexbufPos filename (lexbuf: UnicodeLexing.Lexbuf) = 
    lexbuf.EndPos <- Position.FirstLine (fileIndexOfFile filename)

/// Reset the lexbuf, configure the initial position with the given filename and call the given function
let usingLexbufForParsing (lexbuf:UnicodeLexing.Lexbuf, filename) f =
    resetLexbufPos filename lexbuf
    reusingLexbufForParsing lexbuf (fun () -> f lexbuf)

//------------------------------------------------------------------------
// Functions to manipulate lexer transient state
//-----------------------------------------------------------------------

let defaultStringFinisher = (fun _endm _b s -> STRING (Encoding.Unicode.GetString(s, 0, s.Length))) 

let callStringFinisher fin (buf: ByteBuffer) endm b = fin endm b (buf.Close())

let addUnicodeString (buf: ByteBuffer) (x:string) = buf.EmitBytes (Encoding.Unicode.GetBytes x)

let addIntChar (buf: ByteBuffer) c = 
    buf.EmitIntAsByte (c % 256)
    buf.EmitIntAsByte (c / 256)

let addUnicodeChar buf c = addIntChar buf (int c)
let addByteChar buf (c:char) = addIntChar buf (int32 c % 256)

let stringBufferAsString (buf: byte[]) =
    if buf.Length % 2 <> 0 then failwith "Expected even number of bytes"
    let chars : char[] = Array.zeroCreate (buf.Length/2)
    for i = 0 to (buf.Length/2) - 1 do
        let hi = buf.[i*2+1]
        let lo = buf.[i*2]
        let c = char (((int hi) * 256) + (int lo))
        chars.[i] <- c
    System.String(chars)

/// When lexing bytearrays we don't expect to see any unicode stuff. 
/// Likewise when lexing string constants we shouldn't see any trigraphs > 127 
/// So to turn the bytes collected in the string buffer back into a bytearray 
/// we just take every second byte we stored.  Note all bytes > 127 should have been 
/// stored using addIntChar 
let stringBufferAsBytes (buf: ByteBuffer) = 
    let bytes = buf.Close()
    Array.init (bytes.Length / 2) (fun i -> bytes.[i*2]) 

/// Sanity check that high bytes are zeros. Further check each low byte <= 127 
let stringBufferIsBytes (buf: ByteBuffer) = 
    let bytes = buf.Close()
    let mutable ok = true 
    for i = 0 to bytes.Length / 2-1 do
        if bytes.[i*2+1] <> 0uy then ok <- false
    ok

let newline (lexbuf:LexBuffer<_>) = 
    lexbuf.EndPos <- lexbuf.EndPos.NextLine

let trigraph c1 c2 c3 =
    let digit (c:char) = int c - int '0' 
    char (digit c1 * 100 + digit c2 * 10 + digit c3)

let digit d = 
    if d >= '0' && d <= '9' then int32 d - int32 '0'   
    else failwith "digit" 

let hexdigit d = 
    if d >= '0' && d <= '9' then digit d 
    elif d >= 'a' && d <= 'f' then int32 d - int32 'a' + 10
    elif d >= 'A' && d <= 'F' then int32 d - int32 'A' + 10
    else failwith "hexdigit" 

let unicodeGraphShort (s:string) =
    if s.Length <> 4 then failwith "unicodegraph"
    uint16 (hexdigit s.[0] * 4096 + hexdigit s.[1] * 256 + hexdigit s.[2] * 16 + hexdigit s.[3])

let hexGraphShort (s:string) =
    if s.Length <> 2 then failwith "hexgraph"
    uint16 (hexdigit s.[0] * 16 + hexdigit s.[1])

let unicodeGraphLong (s:string) =
    if s.Length <> 8 then failwith "unicodeGraphLong"
    let high = hexdigit s.[0] * 4096 + hexdigit s.[1] * 256 + hexdigit s.[2] * 16 + hexdigit s.[3] in 
    let low = hexdigit s.[4] * 4096 + hexdigit s.[5] * 256 + hexdigit s.[6] * 16 + hexdigit s.[7] in 
    // not a surrogate pair
    if high = 0 then SingleChar(uint16 low)
    // invalid encoding
    elif high > 0x10 then Invalid
    // valid supplementary character: code points U+10000 to U+10FFFF
    // valid surrogate pair: see http://www.unicode.org/versions/latest/ch03.pdf , "Surrogates" section
    // high-surrogate code point (U+D800 to U+DBFF) followed by low-surrogate code point (U+DC00 to U+DFFF)
    else
      let codepoint = high * 0x10000 + low
      let hiSurr = uint16 (0xD800 + ((codepoint - 0x10000) / 0x400))
      let loSurr = uint16 (0xDC00 + ((codepoint - 0x10000) % 0x400))
      SurrogatePair(hiSurr, loSurr)

let escape c = 
    match c with
    | '\\' -> '\\'
    | '\'' -> '\''
    | 'a' -> char 7
    | 'f' -> char 12
    | 'v' -> char 11
    | 'n' -> '\n'
    | 't' -> '\t'
    | 'b' -> '\b'
    | 'r' -> '\r'
    | c -> c

//------------------------------------------------------------------------
// Keyword table
//-----------------------------------------------------------------------   

exception ReservedKeyword of string * range
exception IndentationProblem of string * range

module Keywords = 
    type private compatibilityMode =
        | ALWAYS  (* keyword *)
        | FSHARP  (* keyword, but an identifier under --ml-compatibility mode *)

    let private keywordList = 
     [ FSHARP, "abstract", ABSTRACT
       ALWAYS, "and"        ,AND
       ALWAYS, "as"         ,AS
       ALWAYS, "assert"     ,ASSERT
       ALWAYS, "asr"        ,INFIX_STAR_STAR_OP "asr"
       ALWAYS, "base"       ,BASE
       ALWAYS, "begin"      ,BEGIN
       ALWAYS, "class"      ,CLASS
       FSHARP, "const"      ,CONST
       FSHARP, "default"    ,DEFAULT
       FSHARP, "delegate"   ,DELEGATE
       ALWAYS, "do"         ,DO
       ALWAYS, "done"       ,DONE
       FSHARP, "downcast"   ,DOWNCAST
       ALWAYS, "downto"     ,DOWNTO
       FSHARP, "elif"       ,ELIF
       ALWAYS, "else"       ,ELSE
       ALWAYS, "end"        ,END
       ALWAYS, "exception"  ,EXCEPTION
       FSHARP, "extern"     ,EXTERN
       ALWAYS, "false"      ,FALSE
       ALWAYS, "finally"    ,FINALLY
       FSHARP, "fixed"      ,FIXED
       ALWAYS, "for"        ,FOR
       ALWAYS, "fun"        ,FUN
       ALWAYS, "function"   ,FUNCTION
       FSHARP, "global"     ,GLOBAL
       ALWAYS, "if"         ,IF
       ALWAYS, "in"         ,IN
       ALWAYS, "inherit"    ,INHERIT
       FSHARP, "inline"     ,INLINE
       FSHARP, "interface"  ,INTERFACE
       FSHARP, "internal"   ,INTERNAL
       ALWAYS, "land"       ,INFIX_STAR_DIV_MOD_OP "land"
       ALWAYS, "lazy"       ,LAZY
       ALWAYS, "let"        ,LET(false)
       ALWAYS, "lor"        ,INFIX_STAR_DIV_MOD_OP "lor"
       ALWAYS, "lsl"        ,INFIX_STAR_STAR_OP "lsl"
       ALWAYS, "lsr"        ,INFIX_STAR_STAR_OP "lsr"
       ALWAYS, "lxor"       ,INFIX_STAR_DIV_MOD_OP "lxor"
       ALWAYS, "match"      ,MATCH
       FSHARP, "member"     ,MEMBER
       ALWAYS, "mod"        ,INFIX_STAR_DIV_MOD_OP "mod"
       ALWAYS, "module"     ,MODULE
       ALWAYS, "mutable"    ,MUTABLE
       FSHARP, "namespace"  ,NAMESPACE
       ALWAYS, "new"        ,NEW
       FSHARP, "null"       ,NULL
       ALWAYS, "of"         ,OF
       ALWAYS, "open"       ,OPEN
       ALWAYS, "or"         ,OR
       FSHARP, "override"   ,OVERRIDE
       ALWAYS, "private"    ,PRIVATE  
       FSHARP, "public"     ,PUBLIC
       ALWAYS, "rec"        ,REC
       FSHARP, "return"      ,YIELD(false)
       ALWAYS, "sig"        ,SIG
       FSHARP, "static"     ,STATIC
       ALWAYS, "struct"     ,STRUCT
       ALWAYS, "then"       ,THEN
       ALWAYS, "to"         ,TO
       ALWAYS, "true"       ,TRUE
       ALWAYS, "try"        ,TRY
       ALWAYS, "type"       ,TYPE
       FSHARP, "upcast"     ,UPCAST
       FSHARP, "use"        ,LET(true)
       ALWAYS, "val"        ,VAL
       FSHARP, "void"       ,VOID
       ALWAYS, "when"       ,WHEN
       ALWAYS, "while"      ,WHILE
       ALWAYS, "with"       ,WITH
       FSHARP, "yield"      ,YIELD(true)
       ALWAYS, "_"          ,UNDERSCORE
     (*------- for prototyping and explaining offside rule *)
       FSHARP, "__token_OBLOCKSEP" ,OBLOCKSEP
       FSHARP, "__token_OWITH"     ,OWITH
       FSHARP, "__token_ODECLEND"  ,ODECLEND
       FSHARP, "__token_OTHEN"     ,OTHEN
       FSHARP, "__token_OELSE"     ,OELSE
       FSHARP, "__token_OEND"      ,OEND
       FSHARP, "__token_ODO"       ,ODO
       FSHARP, "__token_OLET"      ,OLET(true)
       FSHARP, "__token_constraint",CONSTRAINT
      ]
    (*------- reserved keywords which are ml-compatibility ids *) 
    @ List.map (fun s -> (FSHARP,s,RESERVED)) 
        [ "break"; "checked"; "component"; "constraint"; "continue"
          "fori";  "include";  "mixin"
          "parallel"; "params";  "process"; "protected"; "pure"
          "sealed"; "trait";  "tailcall"; "virtual" ]

    let private unreserveWords = 
        keywordList |> List.choose (function (mode, keyword, _) -> if mode = FSHARP then Some keyword else None) 

    //------------------------------------------------------------------------
    // Keywords
    //-----------------------------------------------------------------------

    let keywordNames = 
        keywordList |> List.map (fun (_, w, _) -> w) 

    let keywordTable = 
        let tab = System.Collections.Generic.Dictionary<string, token>(100)
        for _, keyword, token in keywordList do 
            tab.Add(keyword, token)
        tab
        
    let KeywordToken s = keywordTable.[s]

    let IdentifierToken args (lexbuf:UnicodeLexing.Lexbuf) (s:string) =
        if IsCompilerGeneratedName s then 
            warning(Error(FSComp.SR.lexhlpIdentifiersContainingAtSymbolReserved(), lexbuf.LexemeRange))
        args.resourceManager.InternIdentifierToken s

    let KeywordOrIdentifierToken args (lexbuf:UnicodeLexing.Lexbuf) s =
        match keywordTable.TryGetValue s with
        | true, v ->
            match v with 
            | RESERVED ->
                warning(ReservedKeyword(FSComp.SR.lexhlpIdentifierReserved(s), lexbuf.LexemeRange))
                IdentifierToken args lexbuf s
            | _ -> v
        | _ ->
            match s with 
            | "__SOURCE_DIRECTORY__" ->
                let filename = fileOfFileIndex lexbuf.StartPos.FileIndex
                let dirname =
                    if String.IsNullOrWhiteSpace(filename) then
                        String.Empty
                    else if filename = stdinMockFilename then
                        System.IO.Directory.GetCurrentDirectory()
                    else
                        filename
                        |> FileSystem.GetFullPathShim (* asserts that path is already absolute *)
                        |> System.IO.Path.GetDirectoryName

                if String.IsNullOrEmpty dirname then dirname
                else PathMap.applyDir args.pathMap dirname
                |> KEYWORD_STRING
            | "__SOURCE_FILE__" -> 
                KEYWORD_STRING (System.IO.Path.GetFileName((fileOfFileIndex lexbuf.StartPos.FileIndex))) 
            | "__LINE__" -> 
                KEYWORD_STRING (string lexbuf.StartPos.Line)
            | _ -> 
                IdentifierToken args lexbuf s

    let DoesIdentifierNeedQuotation (s : string) : bool =
        not (String.forall IsIdentifierPartCharacter s)              // if it has funky chars
        || s.Length > 0 && (not(IsIdentifierFirstCharacter s.[0]))  // or if it starts with a non-(letter-or-underscore)
        || keywordTable.ContainsKey s                               // or if it's a language keyword like "type"

    /// A utility to help determine if an identifier needs to be quoted 
    let QuoteIdentifierIfNeeded (s : string) : string =
        if DoesIdentifierNeedQuotation s then "``" + s + "``" else s

    /// Quote identifier with double backticks if needed, remove unnecessary double backticks quotation.
    let NormalizeIdentifierBackticks (s : string) : string =
        let s =
            if s.StartsWithOrdinal("``") && s.EndsWithOrdinal("``") then
                s.[2..s.Length - 3]
            else s
        QuoteIdentifierIfNeeded s

    /// Keywords paired with their descriptions. Used in completion and quick info.
    let keywordsWithDescription : (string * string) list =
        [ "abstract",  FSComp.SR.keywordDescriptionAbstract()
          "and",       FSComp.SR.keyworkDescriptionAnd()
          "as",        FSComp.SR.keywordDescriptionAs()
          "assert",    FSComp.SR.keywordDescriptionAssert()
          "base",      FSComp.SR.keywordDescriptionBase()
          "begin",     FSComp.SR.keywordDescriptionBegin()
          "class",     FSComp.SR.keywordDescriptionClass()
          "default",   FSComp.SR.keywordDescriptionDefault()
          "delegate",  FSComp.SR.keywordDescriptionDelegate()
          "do",        FSComp.SR.keywordDescriptionDo()
          "done",      FSComp.SR.keywordDescriptionDone()
          "downcast",  FSComp.SR.keywordDescriptionDowncast()
          "downto",    FSComp.SR.keywordDescriptionDownto()
          "elif",      FSComp.SR.keywordDescriptionElif()
          "else",      FSComp.SR.keywordDescriptionElse()
          "end",       FSComp.SR.keywordDescriptionEnd()
          "exception", FSComp.SR.keywordDescriptionException()
          "extern",    FSComp.SR.keywordDescriptionExtern()
          "false",     FSComp.SR.keywordDescriptionTrueFalse()
          "finally",   FSComp.SR.keywordDescriptionFinally()
          "for",       FSComp.SR.keywordDescriptionFor()
          "fun",       FSComp.SR.keywordDescriptionFun()
          "function",  FSComp.SR.keywordDescriptionFunction()
          "global",    FSComp.SR.keywordDescriptionGlobal()
          "if",        FSComp.SR.keywordDescriptionIf()
          "in",        FSComp.SR.keywordDescriptionIn()
          "inherit",   FSComp.SR.keywordDescriptionInherit()
          "inline",    FSComp.SR.keywordDescriptionInline()
          "interface", FSComp.SR.keywordDescriptionInterface()
          "internal",  FSComp.SR.keywordDescriptionInternal()
          "lazy",      FSComp.SR.keywordDescriptionLazy()
          "let",       FSComp.SR.keywordDescriptionLet()
          "let!",      FSComp.SR.keywordDescriptionLetBang()
          "match",     FSComp.SR.keywordDescriptionMatch()
          "match!",    FSComp.SR.keywordDescriptionMatchBang()
          "member",    FSComp.SR.keywordDescriptionMember()
          "module",    FSComp.SR.keywordDescriptionModule()
          "mutable",   FSComp.SR.keywordDescriptionMutable()
          "namespace", FSComp.SR.keywordDescriptionNamespace()
          "new",       FSComp.SR.keywordDescriptionNew()
          "not",       FSComp.SR.keywordDescriptionNot()
          "null",      FSComp.SR.keywordDescriptionNull()
          "of",        FSComp.SR.keywordDescriptionOf()
          "open",      FSComp.SR.keywordDescriptionOpen()
          "or",        FSComp.SR.keywordDescriptionOr()
          "override",  FSComp.SR.keywordDescriptionOverride()
          "private",   FSComp.SR.keywordDescriptionPrivate()
          "public",    FSComp.SR.keywordDescriptionPublic()
          "rec",       FSComp.SR.keywordDescriptionRec()
          "return",    FSComp.SR.keywordDescriptionReturn()
          "return!",   FSComp.SR.keywordDescriptionReturnBang()
          "select",    FSComp.SR.keywordDescriptionSelect()
          "static",    FSComp.SR.keywordDescriptionStatic()
          "struct",    FSComp.SR.keywordDescriptionStruct()
          "then",      FSComp.SR.keywordDescriptionThen()
          "to",        FSComp.SR.keywordDescriptionTo()
          "true",      FSComp.SR.keywordDescriptionTrueFalse()
          "try",       FSComp.SR.keywordDescriptionTry()
          "type",      FSComp.SR.keywordDescriptionType()
          "upcast",    FSComp.SR.keywordDescriptionUpcast()
          "use",       FSComp.SR.keywordDescriptionUse()
          "use!",      FSComp.SR.keywordDescriptionUseBang()
          "val",       FSComp.SR.keywordDescriptionVal()
          "void",      FSComp.SR.keywordDescriptionVoid()
          "when",      FSComp.SR.keywordDescriptionWhen()
          "while",     FSComp.SR.keywordDescriptionWhile()
          "with",      FSComp.SR.keywordDescriptionWith()
          "yield",     FSComp.SR.keywordDescriptionYield()
          "yield!",    FSComp.SR.keywordDescriptionYieldBang()
          "->",        FSComp.SR.keywordDescriptionRightArrow()
          "<-",        FSComp.SR.keywordDescriptionLeftArrow()
          ":>",        FSComp.SR.keywordDescriptionCast()
          ":?>",       FSComp.SR.keywordDescriptionDynamicCast()
          "<@",        FSComp.SR.keywordDescriptionTypedQuotation()
          "@>",        FSComp.SR.keywordDescriptionTypedQuotation()
          "<@@",       FSComp.SR.keywordDescriptionUntypedQuotation()
          "@@>",       FSComp.SR.keywordDescriptionUntypedQuotation() ]

module Lexer =

    open System.Globalization
    open System.Collections.Generic
    open FSharp.Compiler.Text

    let invalidCharacter = Char.MaxValue

    //--------------------------
    // Integer parsing
    
    // Parsing integers is common in bootstrap runs (parsing
    // the parser tables, no doubt). So this is an optimized
    // version of the F# core library parsing code with the call to "Trim"
    // removed, which appears in profiling runs as a small but significant cost.
    
    let getSign32 (s:string) (p:byref<int>) l = 
        if (l >= p + 1 && s.[p] = '-') 
        then p <- p + 1; -1 
        else 1 
    
    let isOXB c = 
        let c = Char.ToLowerInvariant c
        c = 'x' || c = 'o' || c = 'b'
    
    let is0OXB (s:string) p l = 
        l >= p + 2 && s.[p] = '0' && isOXB s.[p+1]
    
    let get0OXB (s:string) (p:byref<int>)  l = 
        if is0OXB s p l
        then let r = Char.ToLowerInvariant s.[p+1] in p <- p + 2; r
        else 'd' 
    
    let formatError() = raise (new System.FormatException(SR.GetString("bad format string")))
    
    let parseBinaryUInt64 (s:string) = 
        Convert.ToUInt64(s, 2)
    
    let parseOctalUInt64 (s:string) =
        Convert.ToUInt64(s, 8)
    
    let removeUnderscores (s:string) =
        match s with
        | null -> null
        | s -> s.Replace("_", "")
    
    let parseInt32 (s:string) = 
        let s = removeUnderscores s
        let l = s.Length 
        let mutable p = 0 
        let sign = getSign32 s &p l 
        let specifier = get0OXB s &p l 
        match Char.ToLower(specifier,CultureInfo.InvariantCulture) with 
        | 'x' -> sign * (int32 (Convert.ToUInt32(UInt64.Parse(s.Substring(p), NumberStyles.AllowHexSpecifier,CultureInfo.InvariantCulture))))
        | 'b' -> sign * (int32 (Convert.ToUInt32(parseBinaryUInt64 (s.Substring(p)))))
        | 'o' -> sign * (int32 (Convert.ToUInt32(parseOctalUInt64  (s.Substring(p)))))
        | _ -> Int32.Parse(s, NumberStyles.AllowLeadingSign, CultureInfo.InvariantCulture)

    [<Sealed>]
    type SlidingWindow (text: ISourceText) =

        let window = Array.zeroCreate<char> 2048
        let mutable offset = window.Length
        let mutable absoluteBlockPosition = 0

        let tryFill isInit =
            if offset >= window.Length then
                let lengthToCopy = 
                    if window.Length > text.Length - absoluteBlockPosition then
                        text.Length - absoluteBlockPosition
                    else
                        window.Length

                if lengthToCopy > 0 then
                    text.CopyTo(absoluteBlockPosition, window, 0, lengthToCopy)
                    if not isInit then
                        absoluteBlockPosition <- absoluteBlockPosition + lengthToCopy
                    offset <- offset - window.Length

        do
            tryFill true

        member _.PeekChar() =
            tryFill false
            if absoluteBlockPosition + offset > text.Length then
                invalidCharacter
            else
                window.[offset]

        member this.PeekCharAhead(amount: int) =
            if amount < 0 then
                invalidArg "amount" "Amount must be greater than or equal to zero."

            if amount = 0 then
                this.PeekChar()
            else
                tryFill false
                let i = absoluteBlockPosition + offset + amount
                if i > text.Length then
                    invalidCharacter
                else
                    text.[i]

        member _.AdvanceChar() =
            offset <- offset + 1

        member _.Lexeme() =
            String(window, 0, offset)

    type LexNumericLiteralIntegerKind =
        | NormalInteger
        | HexInteger
        | BinaryInteger

    [<Sealed>]
    type Lexer (args: lexargs, text: ISourceText) =  
        let mutable lexemeStartLine = 0
        let mutable lexemeStartColumn = 0

        let mutable line = 0
        let mutable column = 0

        let newLine () = 
            line <- line + 1
            column <- 0

        let window = SlidingWindow text 
        let peek () = window.PeekChar()
        let peekAhead amount = window.PeekCharAhead amount
        let advance () =
            column <- column + 1
            window.AdvanceChar()

        let lexeme () = window.Lexeme()
        let lexemeRange () = mkFileIndexRange 0 (mkPos lexemeStartLine lexemeStartColumn) (mkPos line column)
        
        let trimBoth (s:string) n m = s.Substring(n, s.Length - (n+m))
        
        let lexemeTrimBoth n m = trimBoth (lexeme ()) n m
        
        let lexemeTrimRight n = lexemeTrimBoth 0 n
        
        let _lexemeTrimLeft n = lexemeTrimBoth n 0

        let fail args msg dflt =
            let m = lexemeRange ()
            args.errorLogger.ErrorR(Error(msg,m))
            dflt

        let _lexemeTrimRightToInt32 args n = 
            try parseInt32 (lexemeTrimRight n)
            with _ -> fail args (FSComp.SR.lexOutsideIntegerRange()) 0

        let _evalFloat args =
            try
                float32(removeUnderscores (lexemeTrimRight 1))
            with _ ->
                fail args (FSComp.SR.lexInvalidFloat()) 0.0f

        let rec scanWhitespace () =
            match peek () with
            | ' ' ->
                advance ()
                scanWhitespace ()
            | _ ->
                WHITESPACE (LexerWhitespaceContinuation.Token(LexerIfdefStackEntries.Empty))

        let rec scanNumericLiteralInteger count kind =
            match peek () with
            | '0'             
            | '1' ->
                advance ()
                scanNumericLiteralInteger (count + 1) kind

            | '2'
            | '3'
            | '4'
            | '5'
            | '6'
            | '7' when not (kind = BinaryInteger) ->
                advance ()
                scanNumericLiteralInteger (count + 1) kind
            | '8'
            | '9' when not (kind = BinaryInteger || kind = HexInteger) ->
                advance ()
                scanNumericLiteralInteger (count + 1) kind

            | 'u' when count > 0 ->
                advance ()
                match peek () with
                | 'l' ->
                    advance ()
                | _ ->
                    ()

                let s = removeUnderscores (lexemeTrimRight 1)
                let n = 
                    try int64 s with _ ->  fail args (FSComp.SR.lexOutsideThirtyTwoBitUnsigned()) 0L
                if n > 0xFFFFFFFFL || n < 0L then fail args (FSComp.SR.lexOutsideThirtyTwoBitUnsigned()) (UINT32(0u)) else
                UINT32(uint32 (uint64 n))

            | _ when count > 0 ->
                let s = removeUnderscores (lexemeTrimRight 1)
                // Allow <max_int+1> to parse as min_int.  Allowed only because we parse '-' as an operator. 
                if s = "2147483648" then INT32(-2147483648, true) else
                let n = 
                    try int32 s with _ -> fail args (FSComp.SR.lexOutsideThirtyTwoBitSigned()) 0
                INT32(n,false)

            | _ ->
                fail args (FSComp.SR.lexInvalidNumericLiteral()) (INT32(0, false))
      
        let rec scanNumericLiteral () =
            match peek () with
            | '0' ->
                advance ()
                match peek () with
                | 'b'
                | 'B' ->
                    advance ()
                    scanNumericLiteralInteger 0 BinaryInteger
                | 'x'
                | 'X' ->
                    advance ()
                    scanNumericLiteralInteger 0 HexInteger
                | _ ->
                    scanNumericLiteralInteger 0 NormalInteger
            | _ ->
                scanNumericLiteralInteger 0 NormalInteger
            

        //| "@>." { RQUOTE_DOT ("<@ @>",false) }

        //| "@@>." { RQUOTE_DOT ("<@@ @@>",true) }

        //| ">|]" { GREATER_BAR_RBRACK }

        //| ":?>" { COLON_QMARK_GREATER }

        //| ":?" { COLON_QMARK }

        //| ":=" { COLON_EQUALS }

        //| ";;" { SEMICOLON_SEMICOLON }

        //| ";" { SEMICOLON }

        //| "<-" { LARROW }

        //| "=" { EQUALS }

        //| "[" { LBRACK }

        //| "[|" { LBRACK_BAR }

        //| "{|" { LBRACE_BAR }

        //| "<" { LESS false }

        //| ">" { GREATER false }

        //| "[<" { LBRACK_LESS }

        //| "]" { RBRACK }

        //| "|]" { BAR_RBRACK }

        //| "|}" { BAR_RBRACE }

        //| ">]" { GREATER_RBRACK }

        //| "{" { LBRACE }

        //| "|" { BAR }

        //| "}" { RBRACE }

        //| "$" { DOLLAR }

        //| "%" { PERCENT_OP("%") }

        //| "%%" { PERCENT_OP("%%") }

        //| "-" { MINUS }

        //| "~" { RESERVED }

        //| "`" { RESERVED }

        let scanToken () =
            lexemeStartLine <- line
            lexemeStartColumn <- column
            match peek () with
            | '#' ->
                advance ()
                HASH

            | '&' ->
                advance ()
                match peek () with
                | '&' ->
                    advance ()
                    AMP_AMP
                | _ ->
                   AMP

            | '|' ->
                advance ()
                match peek () with
                | '|' ->
                    advance ()
                    BAR
                | _ ->
                    BAR_BAR

            | '\'' ->
                advance ()
                QUOTE

            | '(' ->
                advance ()
                LPAREN

            | ')' ->
                advance ()
                RPAREN

            | '*' ->
                advance ()
                STAR

            | ',' ->
                advance ()
                COMMA

            | '-' ->
                advance ()
                match peek () with
                | '>' -> 
                    advance ()
                    RARROW
                | _ ->
                    MINUS

            | '>' ->
                advance ()
                match peek (), peekAhead 1 with
                | '|', ']' ->
                    advance ()
                    advance ()
                    GREATER_BAR_RBRACK
                | ']', _ ->
                    advance ()
                    GREATER_RBRACK
                | _ ->
                    GREATER false

            | ';' -> 
                advance ()
                match peek () with
                | ';' ->
                    advance ()
                    SEMICOLON_SEMICOLON
                | _ ->
                    SEMICOLON

            | '=' ->
                advance ()
                EQUALS

            | '[' -> LBRACK

            | ']' -> RBRACK

            | '<' -> LESS false

            | '{' -> LBRACE

            | '}' -> RBRACE

            | '$' -> DOLLAR

            | '%' -> MINUS

            | '~' -> RESERVED

            | '`' -> RESERVED

            | '.' ->
                advance ()
                match peek () with
                | '.' ->
                    advance ()
                    match peek () with
                    | '^' ->
                        // TODO: Split
                        advance ()
                        DOT_DOT_HAT
                    | _ ->
                        DOT_DOT
                | _ ->
                    DOT

            | ':' ->
                advance ()
                match peek () with
                | ':' ->
                    advance ()
                    COLON_COLON
                | '>' ->
                    advance ()
                    COLON_GREATER
                | _ ->
                    COLON

            | '?' ->
                advance ()
                match peek () with
                | '?' ->
                    advance ()
                    QMARK_QMARK
                | _ ->
                    QMARK
                   
            | '\n' ->
                advance ()
                newLine ()
                WHITESPACE (LexerWhitespaceContinuation.Token(LexerIfdefStackEntries.Empty))

            | '\r' ->
                advance ()
                match peek () with
                | '\n' ->
                    advance ()
                | _ ->
                    ()
                newLine ()
                WHITESPACE (LexerWhitespaceContinuation.Token(LexerIfdefStackEntries.Empty))

            | ' ' ->
                scanWhitespace ()

            | '0'
            | '1'
            | '2'
            | '3'
            | '4'
            | '5'
            | '6'
            | '7'
            | '8'
            | '9' ->
                scanNumericLiteral ()

            | _ ->
                EOF(LexerWhitespaceContinuation.Token(LexerIfdefStackEntries.Empty))

        member _.LexemeRange = lexemeRange ()

        member _.ScanToken() = scanToken ()

        static member Create(text) = Lexer(text)