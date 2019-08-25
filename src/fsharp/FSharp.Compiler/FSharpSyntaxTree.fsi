﻿namespace FSharp.Compiler.Compilation

open System
open System.Threading
open System.Collections.Generic
open System.Collections.Immutable
open System.Runtime.CompilerServices
open Microsoft.CodeAnalysis.Text
open Microsoft.CodeAnalysis.Host
open FSharp.Compiler.AbstractIL.Internal.Library
open FSharp.Compiler.Compilation.Utilities
open FSharp.Compiler.CompileOps
open FSharp.Compiler
open FSharp.Compiler.Ast
open FSharp.Compiler.Range
open Microsoft.CodeAnalysis

[<CustomEquality;NoComparison;RequireQualifiedAccess>]
type FSharpSyntaxNodeKind =
    | ParsedInput of ParsedInput
    | ModuleOrNamespace of SynModuleOrNamespace
    | ModuleDecl of SynModuleDecl
    | LongIdentWithDots of LongIdentWithDots
    | Ident of index: int * Ident
    | ComponentInfo of SynComponentInfo
    | TypeConstraint of SynTypeConstraint
    | MemberSig of SynMemberSig
    | TypeDefnSig of SynTypeDefnSig
    | TypeDefnSigRepr of SynTypeDefnSigRepr
    | ExceptionDefnRepr of SynExceptionDefnRepr
    | UnionCase of SynUnionCase
    | UnionCaseType of SynUnionCaseType
    | ArgInfo of SynArgInfo
    | TypeDefnSimpleRepr of SynTypeDefnSimpleRepr
    | SimplePat of SynSimplePat
    | EnumCase of SynEnumCase
    | Const of SynConst
    | Measure of SynMeasure
    | RationalConst of SynRationalConst
    | TypeDefnKind of SynTypeDefnKind
    | Field of SynField
    | ValSig of SynValSig
    | ValTyparDecls of SynValTyparDecls
    | Type of SynType
    | SimplePats of SynSimplePats
    | Typar of SynTypar
    | TyparDecl of SynTyparDecl
    | Binding of SynBinding
    | ValData of SynValData
    | ValInfo of SynValInfo
    | Pat of SynPat
    | ConstructorArgs of SynConstructorArgs
    | BindingReturnInfo of SynBindingReturnInfo
    | Expr of SynExpr
    | StaticOptimizationConstraint of SynStaticOptimizationConstraint
    | IndexerArg of SynIndexerArg
    | SimplePatAlternativeIdInfo of SynSimplePatAlternativeIdInfo
    | MatchClause of SynMatchClause
    | InterfaceImpl of SynInterfaceImpl
    | TypeDefn of SynTypeDefn
    | TypeDefnRepr of SynTypeDefnRepr
    | MemberDefn of SynMemberDefn
    | ExceptionDefn of SynExceptionDefn
    | ParsedHashDirective of ParsedHashDirective
    | AttributeList of SynAttributeList
    | Attribute of SynAttribute

[<Struct;NoEquality;NoComparison>]
type FSharpSyntaxToken =

    member IsNone: bool

    /// Gets the parent node that owns the token.
    /// Does a full parse of the syntax tree if neccessary.
    /// Throws an exception if the token is None.
    member GetParentNode: ?ct: CancellationToken -> FSharpSyntaxNode

    member Span: TextSpan

    member IsKeyword: bool

    member IsIdentifier: bool

    member IsString: bool

    member Value: obj voption

    member ValueText: string voption

    static member None: FSharpSyntaxToken

and [<Sealed>] FSharpSyntaxNode =

    member Parent: FSharpSyntaxNode option

    member SyntaxTree: FSharpSyntaxTree

    member Kind: FSharpSyntaxNodeKind

    member Span: TextSpan

    member GetAncestors: unit -> FSharpSyntaxNode seq

    member GetAncestorsAndSelf: unit -> FSharpSyntaxNode seq

    member GetDescendantTokens: unit -> FSharpSyntaxToken seq

    /// Get tokens whose parent is the current node.
    member GetChildTokens: unit -> FSharpSyntaxToken seq

    member GetDescendants: span: TextSpan -> FSharpSyntaxNode seq

    member GetDescendants: unit -> FSharpSyntaxNode seq

    /// Get nodes whose parent is the current node.
    member GetChildren: span: TextSpan -> FSharpSyntaxNode seq

    member GetChildren: unit -> FSharpSyntaxNode seq

    member GetRoot: unit -> FSharpSyntaxNode

    member FindToken: position: int -> FSharpSyntaxToken

    member TryFindNode: span: TextSpan -> FSharpSyntaxNode option

and [<Sealed>] FSharpSyntaxTree =

    /// The file that was parsed to form a syntax tree.
    /// Will be empty if there is no file associated with the syntax tree.
    /// Will never be null.
    member FilePath: string

    member internal Source: FSharpSource

    /// TODO: Make this public when we have a better way to handling ParsingInfo, perhaps have a better ParsingOptions?
    member internal ParsingConfig: ParsingConfig

    member internal GetParseResult: CancellationToken -> ParseResult

    member internal ConvertSpanToRange: TextSpan -> range

    /// Gets all the tokens by the given span.
    /// Does not require a full parse, therefore use this when you want lexical information without a full parse.
    member GetTokens: span: TextSpan * ?ct: CancellationToken -> FSharpSyntaxToken seq

    /// Gets all the tokens.
    /// The same result as getting descendant tokens from the root node.
    /// Does not require a full parse, therefore use this when you want lexical information without a full parse.
    member GetTokens: ?ct: CancellationToken -> FSharpSyntaxToken seq

    /// Get the root node.
    /// Does a full parse.
    member GetRootNode: ?ct: CancellationToken -> FSharpSyntaxNode

    /// Get the text associated with the syntax tree.
    member GetText: ?ct: CancellationToken -> SourceText

    /// Creates a new syntax tree with the given source.
    member WithChangedSource: newSrc: FSharpSource -> FSharpSyntaxTree

    /// Get diagnostics.
    member GetDiagnostics: ?ct: CancellationToken -> ImmutableArray<Diagnostic>

    static member internal Create: ParsingConfig * FSharpSource -> FSharpSyntaxTree