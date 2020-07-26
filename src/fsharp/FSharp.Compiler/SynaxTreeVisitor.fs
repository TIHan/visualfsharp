module rec FSharp.CodeAnalysis.SyntaxTree
        
open System
open System.Collections.Immutable
open FSharp.Compiler.SyntaxTree
open FSharp.CodeAnalysis.Internal.SyntaxTreeExtendedRanges

[<Struct>]
type FSharpSourceRange internal (range: FSharp.Compiler.Range.range) =

    member _.StartLine = range.StartLine

    member _.StartColumn = range.StartColumn

    member _.EndLine = range.EndLine

    member _.EndColumn = range.EndColumn

    member internal _.InternalRange = range

    static member Combine (range1: FSharpSourceRange, range2: FSharpSourceRange) =
        FSharpSourceRange(FSharp.Compiler.Range.unionRanges range1.InternalRange range2.InternalRange)

[<AbstractClass>]
type FSharpSyntaxNode internal () =
    
    abstract Range : FSharpSourceRange

    abstract Parent : FSharpSyntaxNode

    abstract ChildrenCount : int

    abstract GetChildSlot : index: int -> FSharpSyntaxNode

let failSyntax () = failwith "invalid syntax"

let inline getSyntax<'T when 'T :> FSharpSyntaxNode> (node: byref<'T>) f =
    match box node with
    | null ->
        node <- f ()
    | _ ->
        ()
    node


[<Sealed>]
type FSharpSyntaxConstBool (parent: FSharpSyntaxNode, internalNode: SynConst) =
    inherit FSharpSyntaxConst (parent, internalNode)

    member _.Value =
        match internalNode with
        | SynConst.Bool value -> value
        | _ -> failSyntax ()

[<Sealed>]
type FSharpSyntaxConstMeasure (parent: FSharpSyntaxNode, internalNode: SynConst) =
    inherit FSharpSyntaxConst (parent, internalNode)

    let mutable innerConst = Unchecked.defaultof<FSharpSyntaxConst>
    let mutable innerMeasure = Unchecked.defaultof<FSharpSyntaxMeasure>

    member this.InnerConst =
        getSyntax &innerConst (fun () ->
            match this.InternalNode with
            | SynConst.Measure (internalInner, _) ->
                FSharpSyntaxConst.InternalCreate (this, internalInner)
            | _ ->
                failSyntax ()
        )

    member this.InnerMeasure =
        getSyntax &innerMeasure (fun () ->
            match this.InternalNode with
            | SynConst.Measure (_, internalMeasure) ->
                FSharpSyntaxMeasure.InternalCreate (this, internalMeasure)
            | _ ->
                failSyntax ()
        )


    

[<AbstractClass>]
type FSharpSyntaxConst (parent: FSharpSyntaxNode, internalNode: SynConst) =
    inherit FSharpSyntaxNode()

    let mutable children = ImmutableArray.Empty

    member internal _.InternalNode = internalNode
    
    override this.Range = FSharpSourceRange(this.InternalConst.Range FSharp.Compiler.Range.range0)

    override _.Parent = parent

    static member internal Create(parent: FSharpSyntaxNode, internalNode: SynConst) =
        match internalNode with
        | SynConst.Bool _ -> 
        | SynConst.Byte _
        | SynConst.Bytes _
        | SynConst.Char _
        | SynConst.Decimal _
        | SynConst.Double _
        | SynConst.Int16 _
        | SynConst.Int32 _
        | SynConst.Int64 _
        | SynConst.IntPtr _
        | SynConst.SByte _
        | SynConst.Single _
        | SynConst.String _
        | SynConst.UInt16 _
        | SynConst.UInt16s _
        | SynConst.UInt32 _
        | SynConst.UInt64 _
        | SynConst.UIntPtr _
        | SynConst.Unit
        | SynConst.UserNum _ -> children
        | SynConst.Measure (internalConst, internalMeasure) ->
            if children.IsEmpty then
                let builder = ImmutableArray.CreateBuilder<FSharpSyntaxNode>()
                builder.Add(FSharpSyntaxConst(this, internalConst)) // 0
                builder.Add(FSharpSyntaxMeasure(this, internalMeasure)) // 1
                children <- builder.ToImmutable()
                children
            else
                children

    override this.Children =
        match this.InternalConst with
        | SynConst.Bool _
        | SynConst.Byte _
        | SynConst.Bytes _
        | SynConst.Char _
        | SynConst.Decimal _
        | SynConst.Double _
        | SynConst.Int16 _
        | SynConst.Int32 _
        | SynConst.Int64 _
        | SynConst.IntPtr _
        | SynConst.SByte _
        | SynConst.Single _
        | SynConst.String _
        | SynConst.UInt16 _
        | SynConst.UInt16s _
        | SynConst.UInt32 _
        | SynConst.UInt64 _
        | SynConst.UIntPtr _
        | SynConst.Unit
        | SynConst.UserNum _ -> children
        | SynConst.Measure (internalConst, internalMeasure) ->
            if children.IsEmpty then
                let builder = ImmutableArray.CreateBuilder<FSharpSyntaxNode>()
                builder.Add(FSharpSyntaxConst(this, internalConst)) // 0
                builder.Add(FSharpSyntaxMeasure(this, internalMeasure)) // 1
                children <- builder.ToImmutable()
                children
            else
                children

[<Sealed>]
type FSharpSyntaxIdent internal (parent: FSharpSyntaxNode, internalNode: LongIdent) =
    inherit FSharpSyntaxNode()

    member internal _.InternalIdentifier = internalNode

    override this.Range = FSharpSourceRange(longIdentRange this.InternalIdentifier)

    override _.Parent = parent

    override _.Children = ImmutableArray.Empty

let (|TyparNode|) (node: FSharpSyntaxNode) =
    match node with
    | :? FSharpSyntaxTypar as synTypar -> Some(synTypar)
    | _ -> None

let (|Typar|) (synTypar: FSharpSyntaxTypar) =
    match synTypar.InternalTypar with
    | SynTypar.Typar (_, _, _) ->
        let children = synTypar.Children
        Some(children.[0] :?> FSharpSyntaxIdent)

[<Sealed>]
type FSharpSyntaxTypar internal (parent: FSharpSyntaxNode, internalNode: SynTypar) =
    inherit FSharpSyntaxNode()

    let mutable children = ImmutableArray.Empty

    member internal _.InternalTypar = internalNode

    override this.Range = FSharpSourceRange(this.InternalTypar.Range)

    override _.Parent = parent

    override this.Children =
        match this.InternalTypar with
        | SynTypar.Typar (id, _, _) ->
            if children.IsEmpty then
                let builder = ImmutableArray.CreateBuilder<FSharpSyntaxNode>()
                builder.Add(FSharpSyntaxIdent(this, [id]))
                children <- builder.ToImmutable()
                children
            else
                children

[<RequireQualifiedAccess>]
module Const =

    let (|ConstNode|) (node: FSharpSyntaxNode) =
        match node with
        | :? FSharpSyntaxConst as synConst -> Some(synConst)
        | _ -> None

    let (|ConstBool|) (synConst: FSharpSyntaxConst) =
        match synConst.InternalConst with
        | SynConst.Bool value -> Some value
        | _ -> None
    
    let (|ConstByte|) (synConst: FSharpSyntaxConst) =
        match synConst.InternalConst with
        | SynConst.Byte value -> Some value
        | _ -> None
    
    let (|ConstBytes|) (synConst: FSharpSyntaxConst) =
        match synConst.InternalConst with
        | SynConst.Bytes (value, _) -> Some (ReadOnlyMemory value)
        | _ -> None
    
    let (|ConstChar|) (synConst: FSharpSyntaxConst) =
        match synConst.InternalConst with
        | SynConst.Char value -> Some value
        | _ -> None

    let (|ConstDecimal|) (synConst: FSharpSyntaxConst) =
        match synConst.InternalConst with
        | SynConst.Decimal value -> Some value
        | _ -> None
    
    let (|ConstDouble|) (synConst: FSharpSyntaxConst) =
        match synConst.InternalConst with
        | SynConst.Double value -> Some value
        | _ -> None
    
    let (|ConstInt16|) (synConst: FSharpSyntaxConst) =
        match synConst.InternalConst with
        | SynConst.Int16 value -> Some value
        | _ -> None
    
    let (|ConstInt32|) (synConst: FSharpSyntaxConst) =
        match synConst.InternalConst with
        | SynConst.Int32 value -> Some value
        | _ -> None
    
    let (|ConstInt64|) (synConst: FSharpSyntaxConst) =
        match synConst.InternalConst with
        | SynConst.Int64 value -> Some value
        | _ -> None
    
    let (|ConstIntPtr|) (synConst: FSharpSyntaxConst) =
        match synConst.InternalConst with
        | SynConst.IntPtr value -> Some value
        | _ -> None
    
    let (|ConstSByte|) (synConst: FSharpSyntaxConst) =
        match synConst.InternalConst with
        | SynConst.SByte value -> Some value
        | _ -> None
    
    let (|ConstSingle|) (synConst: FSharpSyntaxConst) =
        match synConst.InternalConst with
        | SynConst.Single value -> Some value
        | _ -> None
    
    let (|ConstString|) (synConst: FSharpSyntaxConst) =
        match synConst.InternalConst with
        | SynConst.String (value, _) -> Some value
        | _ -> None

    let (|ConstUInt16|) (synConst: FSharpSyntaxConst) =
        match synConst.InternalConst with
        | SynConst.UInt16 value -> Some value
        | _ -> None
    
    let (|ConstUInt16s|) (synConst: FSharpSyntaxConst) =
        match synConst.InternalConst with
        | SynConst.UInt16s value -> Some(ReadOnlyMemory value)
        | _ -> None
    
    let (|ConstUInt32|) (synConst: FSharpSyntaxConst) =
        match synConst.InternalConst with
        | SynConst.UInt32 value -> Some value
        | _ -> None
    
    let (|ConstUInt64|) (synConst: FSharpSyntaxConst) =
        match synConst.InternalConst with
        | SynConst.UInt64 value -> Some value
        | _ -> None
    
    let (|ConstUIntPtr|) (synConst: FSharpSyntaxConst) =
        match synConst.InternalConst with
        | SynConst.UIntPtr value -> Some value
        | _ -> None
    
    let (|ConstUnit|) (synConst: FSharpSyntaxConst) =
        match synConst.InternalConst with
        | SynConst.Unit -> Some()
        | _ -> None
    
    let (|ConstUserNumber|) (synConst: FSharpSyntaxConst) =
        match synConst.InternalConst with
        | SynConst.UserNum (value, suffix) -> Some(value, suffix)
        | _ -> None
    
    let (|ConstMeasure|) (synConst: FSharpSyntaxConst) =
        match synConst.InternalConst with
        | SynConst.Measure _ ->
            let children = synConst.Children
            Some(children.[0] :?> FSharpSyntaxConst, children.[1] :?> FSharpSyntaxMeasure)
        | _ -> 
            None

[<Sealed>]
type FSharpSyntaxMeasure internal (parent: FSharpSyntaxNode, internalNode: SynMeasure) =
    inherit FSharpSyntaxNode()

    let mutable children = ImmutableArray.Empty

    member internal _.InternalMeasure = internalNode

    override this.Range = FSharpSourceRange(this.InternalMeasure.Range)

    override _.Parent = parent

    static member internal InternalCreate (parent: FSharpSyntaxNode, internalNode: SynMeasure) =
        match internalNode with
        | SynMeasure.Anon _
        | SynMeasure.One _ -> children
        | SynMeasure.Divide (internalMeasure1, internalMeasure2, _) 
        | SynMeasure.Product (internalMeasure1, internalMeasure2, _) ->
            if children.IsEmpty then
                let builder = ImmutableArray.CreateBuilder<FSharpSyntaxNode>()
                builder.Add(FSharpSyntaxMeasure(this, internalMeasure1)) // 0
                builder.Add(FSharpSyntaxMeasure(this, internalMeasure2)) // 1
                children <- builder.ToImmutable()
                children
            else
                children
        | SynMeasure.Power (internalMeasure1, _, _) ->
            if children.IsEmpty then
                let builder = ImmutableArray.CreateBuilder<FSharpSyntaxNode>()
                builder.Add(FSharpSyntaxMeasure(this, internalMeasure1)) // 0
                children <- builder.ToImmutable()
                children
            else
                children
        | SynMeasure.Named (lid, _) ->
            if children.IsEmpty then
                let builder = ImmutableArray.CreateBuilder<FSharpSyntaxNode>()
                builder.Add(FSharpSyntaxIdent(this, lid)) // 0
                children <- builder.ToImmutable()
                children
            else
                children
        | SynMeasure.Seq (internalMeasures, _) ->
            if children.IsEmpty then
                let builder = ImmutableArray.CreateBuilder<FSharpSyntaxNode>()
                builder.Add(FSharpSyntaxList(this, internalMeasures |> Seq.map (fun x -> FSharpSyntaxMeasure(this, x) :> FSharpSyntaxNode))) // 0
                children <- builder.ToImmutable()
                children
            else
                children
        | SynMeasure.Var (internalTypar, _) ->
            if children.IsEmpty then
                let builder = ImmutableArray.CreateBuilder<FSharpSyntaxNode>()
                builder.Add(FSharpSyntaxTypar(this, internalTypar)) // 0
                children <- builder.ToImmutable()
                children
            else
                children

let (|MeasureNode|) (node: FSharpSyntaxNode) =
    match node with
    | :? FSharpSyntaxMeasure as synMeasure -> Some(synMeasure)
    | _ -> None

let (|MeasureAnon|) (synMeasure: FSharpSyntaxMeasure) =
    match synMeasure.InternalMeasure with
    | SynMeasure.Anon _ -> Some()
    | _ -> None

let (|MeasureOne|) (synMeasure: FSharpSyntaxMeasure) =
    match synMeasure.InternalMeasure with
    | SynMeasure.One _ -> Some()
    | _ -> None

let (|MeasureDivide|) (synMeasure: FSharpSyntaxMeasure) =
    match synMeasure.InternalMeasure with
    | SynMeasure.Divide (internalMeasure1, internalMeasure2, _) -> 
        let children = synMeasure.Children
        Some(children.[0] :?> FSharpSyntaxMeasure, children.[1] :?> FSharpSyntaxMeasure)
    | _ -> 
        None

let (|MeasureProduct|) (synMeasure: FSharpSyntaxMeasure) =
    match synMeasure.InternalMeasure with
    | SynMeasure.Product _ -> 
        let children = synMeasure.Children
        Some(children.[0] :?> FSharpSyntaxMeasure, children.[1] :?> FSharpSyntaxMeasure)
    | _ -> 
        None

let (|MeasurePower|) (synMeasure: FSharpSyntaxMeasure) =
    match synMeasure.InternalMeasure with
    | SynMeasure.Power _ -> 
        let children = synMeasure.Children
        Some(children.[0] :?> FSharpSyntaxMeasure)
    | _ -> 
        None

let (|MeasureNamed|) (synMeasure: FSharpSyntaxMeasure) =
    match synMeasure.InternalMeasure with
    | SynMeasure.Named _ -> 
        let children = synMeasure.Children
        Some(children.[0] :?> FSharpSyntaxIdent)
    | _ -> 
        None

let (|MeasureSeq|) (synMeasure: FSharpSyntaxMeasure) =
    match synMeasure.InternalMeasure with
    | SynMeasure.Seq _ -> 
        let children = synMeasure.Children
        Some(children.[0] :?> FSharpSyntaxList<FSharpSyntaxMeasure>)
    | _ -> 
        None

let (|MeasureVar|) (synMeasure: FSharpSyntaxMeasure) =
    match synMeasure.InternalMeasure with
    | SynMeasure.Var _ -> 
        let children = synMeasure.Children
        Some(children.[0] :?> FSharpSyntaxList<FSharpSyntaxTypar>)
    | _ -> 
        None

[<Sealed>]
type FSharpSyntaxBinding internal (parent: FSharpSyntaxNode, internalNode: SynBinding) =
    inherit FSharpSyntaxNode()

    let mutable children = ImmutableArray.Empty

    member _.InternalNode = internalNode

    override _.Parent = parent

    override this.Range = FSharpSourceRange this.InternalNode.Range

    override this.Children =
        if children.IsEmpty then
            children <-
                match this.InternalNode with
                | SynBinding.Binding (_, _, _, _, internalAttribs, _, _, _, _, _, _, _) ->
                    ImmutableArray.Empty
        children

[<Sealed>]
type FSharpSyntaxModuleDecl internal (parent: FSharpSyntaxNode, internalNode: SynModuleDecl) =
    inherit FSharpSyntaxNode()

    let mutable children = ImmutableArray.Empty

    member _.InternalNode = internalNode

    override _.Parent = parent

    override this.Range = FSharpSourceRange this.InternalNode.Range

    override this.Children =
        if children.IsEmpty then
            children <-
                match this.InternalNode with
                | SynModuleDecl.ModuleAbbrev (id, lid, _) ->
                    let builder = ImmutableArray.CreateBuilder<FSharpSyntaxNode>()
                    builder.Add(FSharpSyntaxIdent(this, [id])) // 0
                    builder.Add(FSharpSyntaxIdent(this, lid)) // 1
                    builder.ToImmutable()

                | SynModuleDecl.NestedModule (_, _, internalDecls, _, _) ->
                    let builder = ImmutableArray.CreateBuilder<FSharpSyntaxNode>()
                    builder.Add(FSharpSyntaxList<FSharpSyntaxModuleDecl>(this, internalDecls |> Seq.map (fun x -> FSharpSyntaxModuleDecl(this, x)))) // 0
                    builder.ToImmutable()

                | SynModuleDecl.Let (_, bindings, _) ->
                    let builder = ImmutableArray.CreateBuilder<FSharpSyntaxNode>()

                    builder.ToImmutable()

                | _ ->
                    failwith "nope"
        children
            

let (|ModuleDecl|) (node: FSharpSyntaxNode) =
    match node with
    | :? FSharpSyntaxModuleDecl as node -> Some node
    | _ -> None

[<RequireQualifiedAccess>]
module ModuleDecl =   

    let (|ModuleAbbrev|) (node: FSharpSyntaxModuleDecl) =
        match node.InternalNode with
        | SynModuleDecl.ModuleAbbrev _ ->
            let children = node.Children
            Some(children.[0] :?> FSharpSyntaxIdent, children.[1] :?> FSharpSyntaxIdent)
        | _ ->
            None

    let (|NestedModule|) (node: FSharpSyntaxModuleDecl) =
        match node.InternalNode with
        | SynModuleDecl.NestedModule _ ->
            let children = node.Children
            Some(children.[0] :?> FSharpSyntaxList<FSharpSyntaxModuleDecl>)
        | _ ->
            None

[<Sealed>]
type FSharpSyntaxModuleOrNamespace internal (parent: FSharpSyntaxNode, internalNode: SynModuleOrNamespace) =
    inherit FSharpSyntaxNode()

    let mutable children = ImmutableArray.Empty

    member _.InternalModuleOrNamespace = internalNode

    override _.Parent = parent

    override this.Range = FSharpSourceRange this.InternalModuleOrNamespace.Range

    override this.Children =
        match this.InternalModuleOrNamespace with
        | SynModuleOrNamespace (longId, _, _, internalDecls, _, internalAttribs, internalAccessibility, _) ->
            if children.IsEmpty then
                let builder = ImmutableArray.CreateBuilder<FSharpSyntaxNode>()
                builder.Add(FSharpSyntaxIdent(this, longId))
                children <- builder.ToImmutable()
            children

let (|ModuleOrNamespaceNode|) (node: FSharpSyntaxNode) =
    match node with
    | :? FSharpSyntaxModuleOrNamespace as node -> Some node
    | _ -> None

let (|ModuleOrNamespace|) (synModuleOrNamespace: FSharpSyntaxModuleOrNamespace) =
    match synModuleOrNamespace.InternalModuleOrNamespace with
    | SynModuleOrNamespace.SynModuleOrNamespace _ ->
        let children = synModuleOrNamespace.Children
        Some(children.[0] :?> FSharpSyntaxIdent)
    
[<Sealed>]
type FSharpSyntaxHashDirective (parent: FSharpSyntaxNode, name: string, args: ImmutableArray<string>, range: FSharpSourceRange) =
    inherit FSharpSyntaxNode()

    override _.Parent = parent

    override _.Children = ImmutableArray.Empty

    override _.Range = range

    member _.Name = name
    
    member _.Arguments = args

[<Sealed>]
type FSharpSyntaxTree internal (implFile: ParsedImplFileInput) =
    inherit FSharpSyntaxNode()

    let mutable children = ImmutableArray.Empty
    let mutable hashDirectives = ImmutableArray<FSharpSyntaxHashDirective>.Empty
    let mutable modules = ImmutableArray<FSharpSyntaxModuleOrNamespace>.Empty

    member _.IsScript =
        match implFile with
        | ParsedImplFileInput (isScript=isScript) -> isScript

    member _.FileName =
        match implFile with
        | ParsedImplFileInput (fileName=fileName) -> fileName

    member this.HashedDirectives =
        match implFile with
        | ParsedImplFileInput (hashDirectives=internalHashDirectives) ->
            if hashDirectives.IsEmpty then
                let builder = ImmutableArray.CreateBuilder<FSharpSyntaxHashDirective>()
                for ParsedHashDirective (name, args, m) in internalHashDirectives do
                    builder.Add(FSharpSyntaxHashDirective(this, name, ImmutableArray.CreateRange args, FSharpSourceRange m))
                hashDirectives <- builder.ToImmutable()
            hashDirectives

    member this.ModuleOrNamespaces =
        if modules.IsEmpty then
            modules <-
                match implFile with
                | ParsedImplFileInput (modules=modules) ->
                    let builder = ImmutableArray.CreateBuilder<FSharpSyntaxModuleOrNamespace>()
                    for x in modules do
                        builder.Add(FSharpSyntaxModuleOrNamespace(this, x))
                    builder.ToImmutable()
        modules

    override _.Parent = Unchecked.defaultof<_>

    override _.Range = FSharpSourceRange implFile.Range

    override this.Children =
        if children.IsEmpty then
            children <-
                seq {
                    yield! (this.HashedDirectives |> Seq.map (fun x -> x :> FSharpSyntaxNode))
                    yield! (this.ModuleOrNamespaces |> Seq.map (fun x -> x :> FSharpSyntaxNode)) }
                |> ImmutableArray.CreateRange
        children
            
let (|RootNode|) (node: FSharpSyntaxNode) =
    match node with
    | :? FSharpSyntaxTree as node -> Some node
    | _ -> None
