module rec FSharp.CodeAnalysis.SyntaxTree
        
open FSharp.Compiler.Range
open FSharp.Compiler.AbstractIL.Internal.Library
open FSharp.Compiler.SyntaxTree
open FSharp.CodeAnalysis.Internal.SyntaxTreeExtendedRanges

let private failSyntax () = failwith "invalid syntax"
let private failIndex () = raise (System.IndexOutOfRangeException())

[<Struct>]
type FSharpSourceRange internal (range: range) =

    member _.StartLine = range.StartLine

    member _.StartColumn = range.StartColumn

    member _.EndLine = range.EndLine

    member _.EndColumn = range.EndColumn

    member internal _.InternalRange = range

    static member Combine (range1: FSharpSourceRange, range2: FSharpSourceRange) =
        FSharpSourceRange(FSharp.Compiler.Range.unionRanges range1.InternalRange range2.InternalRange)

[<Sealed>]
type FSharpSyntaxNodeList<'T when 'T :> FSharpSyntaxNode> (parent: FSharpSyntaxNode, nodes: 'T list) =
    inherit FSharpSyntaxNode(parent)

    override _.Range = FSharpSourceRange range0

    override _.GetChild index = nodes.[index] :> FSharpSyntaxNode

    override _.GetChildrenCount() = nodes.Length

    member _.Nodes = nodes

[<AbstractClass>]
type FSharpSyntaxNode internal (parent: FSharpSyntaxNode) =
    
    abstract Range : FSharpSourceRange

    abstract GetChild : index: int -> FSharpSyntaxNode

    abstract GetChildrenCount : unit -> int

    member _.Parent = parent

[<AbstractClass>]
type FSharpSyntaxTree internal (parent: FSharpSyntaxNode, internalNode: ParsedInput) =
    inherit FSharpSyntaxNode (parent)

    member _.InternalNode = internalNode

    override _.Range = FSharpSourceRange internalNode.Range

    static member internal InternalCreate(internalNode: ParsedInput) =
        match internalNode with
        | ParsedInput.ImplFile _ -> FSharpSyntaxTreeImplementation(Unchecked.defaultof<_>, internalNode) :> FSharpSyntaxTree
        | ParsedInput.SigFile _ -> FSharpSyntaxTreeSignature(Unchecked.defaultof<_>, internalNode) :> FSharpSyntaxTree

[<Sealed>]
type FSharpSyntaxTreeImplementation internal (parent: FSharpSyntaxNode, internalNode: ParsedInput) =
    inherit FSharpSyntaxTree(parent, internalNode)

    let mutable implementation : FSharpSyntaxImplementation = Unchecked.defaultof<_>

    member this.Implementation =
        match box implementation with
        | null ->
            implementation <-
                match this.InternalNode with
                | ParsedInput.ImplFile implFile ->
                    FSharpSyntaxImplementation(this, implFile)
                | _ -> failSyntax ()
        | _ ->
            ()
        implementation

    override _.GetChild index =
        match index with
        | 0 -> implementation :> FSharpSyntaxNode
        | _ -> failIndex ()

    override _.GetChildrenCount() = 1
            
[<Sealed>]
type FSharpSyntaxTreeSignature internal (parent: FSharpSyntaxNode, internalNode: ParsedInput) =
    inherit FSharpSyntaxTree(parent, internalNode)

    let mutable signature : FSharpSyntaxSignature = Unchecked.defaultof<_>

    member this.Signature =
        match box signature with
        | null ->
            signature <-
                match this.InternalNode with
                | ParsedInput.SigFile sigFile ->
                    FSharpSyntaxSignature(this, sigFile)
                | _ -> failSyntax ()
        | _ ->
            ()
        signature

    override _.GetChild index =
        match index with
        | 0 -> signature :> FSharpSyntaxNode
        | _ -> failIndex ()

    override _.GetChildrenCount() = 1
            
[<Sealed>]
type FSharpSyntaxImplementation internal (parent: FSharpSyntaxNode, internalNode: ParsedImplFileInput) =
    inherit FSharpSyntaxNode (parent)

    let mutable hashDirectives : FSharpSyntaxHashDirective list = Unchecked.defaultof<_>
    let mutable moduleOrNamespaces : FSharpSyntaxModuleOrNamespace list = Unchecked.defaultof<_>

    member _.InternalNode = internalNode

    override _.Range = FSharpSourceRange internalNode.Range

    member this.HashDirectives =
        match box hashDirectives with
        | null ->
            hashDirectives <-
                match this.InternalNode with
                | ParsedImplFileInput.ParsedImplFileInput (_, _, _, _, hashDirectives, _, _) ->
                    hashDirectives |> List.map (fun x -> FSharpSyntaxHashDirective(this, x))
                | _ -> failSyntax ()
        | _ ->
            ()
        hashDirectives

    member this.item5 =
        match box moduleOrNamespaces with
        | null ->
            moduleOrNamespaces <-
                match this.InternalNode with
                | ParsedImplFileInput.ParsedImplFileInput (_, _, _, _, _, modules, _) ->
                    modules |> List.map (fun x -> FSharpSyntaxModuleOrNamespace(this, x))


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxSignature internal (parent: FSharpSyntaxNode, internalNode: ParsedSigFileInput) =
    inherit FSharpSyntaxNode (parent)

    let mutable __item3 : FSharpSyntaxNodeList<FSharpSyntaxParsedHashDirective> = Unchecked.defaultof<_>
    let mutable __item4 : FSharpSyntaxNodeList<FSharpSyntaxModuleOrNamespaceSig> = Unchecked.defaultof<_>

    member _.InternalNode = internalNode

    override _.Range = FSharpSourceRange internalNode.Range

    member this.item3 =
            match this.InternalNode with
            | ParsedSigFileInput.ParsedSigFileInput (_, _, _, item3, _) ->
                FSharpSyntaxNodeList<FSharpSyntaxParsedHashDirective>(this, item3)
            | _ -> failwith "invalid syntax" 

    member this.item4 =
            match this.InternalNode with
            | ParsedSigFileInput.ParsedSigFileInput (_, _, _, _, item4) ->
                FSharpSyntaxNodeList<FSharpSyntaxModuleOrNamespaceSig>(this, item4)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxHashDirective internal (parent: FSharpSyntaxNode, internalNode: ParsedHashDirective) =
    inherit FSharpSyntaxNode (parent)

    member _.InternalNode = internalNode

    override _.Range = FSharpSourceRange internalNode.Range

    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0


[<Sealed>]
type FSharpSyntaxParsedHashDirectiveParsedHashDirective internal (parent: FSharpSyntaxNode, internalNode: ParsedHashDirective) =
    inherit FSharpSyntaxParsedHashDirective(parent, internalNode)






    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<AbstractClass>]
type FSharpSyntaxModuleOrNamespace internal (parent: FSharpSyntaxNode, internalNode: SynModuleOrNamespace) =
    inherit FSharpSyntaxNode (parent)

    member _.InternalNode = internalNode

    override _.Range = FSharpSourceRange internalNode.Range


[<Sealed>]
type FSharpSyntaxModuleOrNamespaceSynModuleOrNamespace internal (parent: FSharpSyntaxNode, internalNode: SynModuleOrNamespace) =
    inherit FSharpSyntaxModuleOrNamespace(parent, internalNode)

    let mutable __item0 : FSharpSyntaxNodeList<FSharpSyntaxIdent> = Unchecked.defaultof<_>
    let mutable __item2 : FSharpSyntaxModuleOrNamespaceKind = Unchecked.defaultof<_>
    let mutable __item3 : FSharpSyntaxNodeList<FSharpSyntaxModuleDecl> = Unchecked.defaultof<_>
    let mutable __item5 : FSharpSyntaxNodeList<FSharpSyntaxAttributeList> = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynModuleOrNamespace.SynModuleOrNamespace (item0, _, _, _, _, _, _, _) ->
                FSharpSyntaxNodeList<FSharpSyntaxIdent>(this, item0)
            | _ -> failwith "invalid syntax" 

    member this.item2 =
            match this.InternalNode with
            | SynModuleOrNamespace.SynModuleOrNamespace (_, _, item2, _, _, _, _, _) ->
                FSharpSyntaxModuleOrNamespaceKind(this, item2)
            | _ -> failwith "invalid syntax" 

    member this.item3 =
            match this.InternalNode with
            | SynModuleOrNamespace.SynModuleOrNamespace (_, _, _, item3, _, _, _, _) ->
                FSharpSyntaxNodeList<FSharpSyntaxModuleDecl>(this, item3)
            | _ -> failwith "invalid syntax" 

    member this.item5 =
            match this.InternalNode with
            | SynModuleOrNamespace.SynModuleOrNamespace (_, _, _, _, _, item5, _, _) ->
                FSharpSyntaxNodeList<FSharpSyntaxAttributeList>(this, item5)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<AbstractClass>]
type FSharpSyntaxModuleOrNamespaceSig internal (parent: FSharpSyntaxNode, internalNode: SynModuleOrNamespaceSig) =
    inherit FSharpSyntaxNode (parent)

    member _.InternalNode = internalNode

    override _.Range = FSharpSourceRange internalNode.Range


[<Sealed>]
type FSharpSyntaxModuleOrNamespaceSigSynModuleOrNamespaceSig internal (parent: FSharpSyntaxNode, internalNode: SynModuleOrNamespaceSig) =
    inherit FSharpSyntaxModuleOrNamespaceSig(parent, internalNode)

    let mutable __item0 : FSharpSyntaxNodeList<FSharpSyntaxIdent> = Unchecked.defaultof<_>
    let mutable __item2 : FSharpSyntaxModuleOrNamespaceKind = Unchecked.defaultof<_>
    let mutable __item3 : FSharpSyntaxNodeList<FSharpSyntaxModuleSigDecl> = Unchecked.defaultof<_>
    let mutable __item5 : FSharpSyntaxNodeList<FSharpSyntaxAttributeList> = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynModuleOrNamespaceSig.SynModuleOrNamespaceSig (item0, _, _, _, _, _, _, _) ->
                FSharpSyntaxNodeList<FSharpSyntaxIdent>(this, item0)
            | _ -> failwith "invalid syntax" 

    member this.item2 =
            match this.InternalNode with
            | SynModuleOrNamespaceSig.SynModuleOrNamespaceSig (_, _, item2, _, _, _, _, _) ->
                FSharpSyntaxModuleOrNamespaceKind(this, item2)
            | _ -> failwith "invalid syntax" 

    member this.item3 =
            match this.InternalNode with
            | SynModuleOrNamespaceSig.SynModuleOrNamespaceSig (_, _, _, item3, _, _, _, _) ->
                FSharpSyntaxNodeList<FSharpSyntaxModuleSigDecl>(this, item3)
            | _ -> failwith "invalid syntax" 

    member this.item5 =
            match this.InternalNode with
            | SynModuleOrNamespaceSig.SynModuleOrNamespaceSig (_, _, _, _, _, item5, _, _) ->
                FSharpSyntaxNodeList<FSharpSyntaxAttributeList>(this, item5)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<AbstractClass>]
type FSharpSyntaxIdent internal (parent: FSharpSyntaxNode, internalNode: Ident) =
    inherit FSharpSyntaxNode (parent)

    member _.InternalNode = internalNode

    override _.Range = FSharpSourceRange internalNode.Range


[<AbstractClass>]
type FSharpSyntaxModuleOrNamespaceKind internal (parent: FSharpSyntaxNode, internalNode: SynModuleOrNamespaceKind) =
    inherit FSharpSyntaxNode (parent)

    member _.InternalNode = internalNode

    override _.Range = FSharpSourceRange internalNode.Range


[<Sealed>]
type FSharpSyntaxModuleOrNamespaceKindNamedModule internal (parent: FSharpSyntaxNode, internalNode: SynModuleOrNamespaceKind) =
    inherit FSharpSyntaxModuleOrNamespaceKind(parent, internalNode)


[<Sealed>]
type FSharpSyntaxModuleOrNamespaceKindAnonModule internal (parent: FSharpSyntaxNode, internalNode: SynModuleOrNamespaceKind) =
    inherit FSharpSyntaxModuleOrNamespaceKind(parent, internalNode)


[<Sealed>]
type FSharpSyntaxModuleOrNamespaceKindDeclaredNamespace internal (parent: FSharpSyntaxNode, internalNode: SynModuleOrNamespaceKind) =
    inherit FSharpSyntaxModuleOrNamespaceKind(parent, internalNode)


[<Sealed>]
type FSharpSyntaxModuleOrNamespaceKindGlobalNamespace internal (parent: FSharpSyntaxNode, internalNode: SynModuleOrNamespaceKind) =
    inherit FSharpSyntaxModuleOrNamespaceKind(parent, internalNode)


[<AbstractClass>]
type FSharpSyntaxModuleDecl internal (parent: FSharpSyntaxNode, internalNode: SynModuleDecl) =
    inherit FSharpSyntaxNode (parent)

    member _.InternalNode = internalNode

    override _.Range = FSharpSourceRange internalNode.Range


[<Sealed>]
type FSharpSyntaxModuleDeclModuleAbbrev internal (parent: FSharpSyntaxNode, internalNode: SynModuleDecl) =
    inherit FSharpSyntaxModuleDecl(parent, internalNode)

    let mutable __item0 : FSharpSyntaxIdent = Unchecked.defaultof<_>
    let mutable __item1 : FSharpSyntaxNodeList<FSharpSyntaxIdent> = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynModuleDecl.ModuleAbbrev (item0, _, _) ->
                FSharpSyntaxIdent(this, item0)
            | _ -> failwith "invalid syntax" 

    member this.item1 =
            match this.InternalNode with
            | SynModuleDecl.ModuleAbbrev (_, item1, _) ->
                FSharpSyntaxNodeList<FSharpSyntaxIdent>(this, item1)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxModuleDeclNestedModule internal (parent: FSharpSyntaxNode, internalNode: SynModuleDecl) =
    inherit FSharpSyntaxModuleDecl(parent, internalNode)

    let mutable __item0 : FSharpSyntaxComponentInfo = Unchecked.defaultof<_>
    let mutable __item2 : FSharpSyntaxNodeList<FSharpSyntaxModuleDecl> = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynModuleDecl.NestedModule (item0, _, _, _, _) ->
                FSharpSyntaxComponentInfo(this, item0)
            | _ -> failwith "invalid syntax" 

    member this.item2 =
            match this.InternalNode with
            | SynModuleDecl.NestedModule (_, _, item2, _, _) ->
                FSharpSyntaxNodeList<FSharpSyntaxModuleDecl>(this, item2)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxModuleDeclLet internal (parent: FSharpSyntaxNode, internalNode: SynModuleDecl) =
    inherit FSharpSyntaxModuleDecl(parent, internalNode)

    let mutable __item1 : FSharpSyntaxNodeList<FSharpSyntaxBinding> = Unchecked.defaultof<_>

    member this.item1 =
            match this.InternalNode with
            | SynModuleDecl.Let (_, item1, _) ->
                FSharpSyntaxNodeList<FSharpSyntaxBinding>(this, item1)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxModuleDeclDoExpr internal (parent: FSharpSyntaxNode, internalNode: SynModuleDecl) =
    inherit FSharpSyntaxModuleDecl(parent, internalNode)

    let mutable __item1 : FSharpSyntaxExpr = Unchecked.defaultof<_>

    member this.item1 =
            match this.InternalNode with
            | SynModuleDecl.DoExpr (_, item1, _) ->
                FSharpSyntaxExpr(this, item1)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxModuleDeclTypes internal (parent: FSharpSyntaxNode, internalNode: SynModuleDecl) =
    inherit FSharpSyntaxModuleDecl(parent, internalNode)

    let mutable __item0 : FSharpSyntaxNodeList<FSharpSyntaxTypeDefn> = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynModuleDecl.Types (item0, _) ->
                FSharpSyntaxNodeList<FSharpSyntaxTypeDefn>(this, item0)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxModuleDeclException internal (parent: FSharpSyntaxNode, internalNode: SynModuleDecl) =
    inherit FSharpSyntaxModuleDecl(parent, internalNode)

    let mutable __item0 : FSharpSyntaxExceptionDefn = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynModuleDecl.Exception (item0, _) ->
                FSharpSyntaxExceptionDefn(this, item0)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxModuleDeclOpen internal (parent: FSharpSyntaxNode, internalNode: SynModuleDecl) =
    inherit FSharpSyntaxModuleDecl(parent, internalNode)

    let mutable __item0 : FSharpSyntaxLongIdentWithDots = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynModuleDecl.Open (item0, _) ->
                FSharpSyntaxLongIdentWithDots(this, item0)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxModuleDeclAttributes internal (parent: FSharpSyntaxNode, internalNode: SynModuleDecl) =
    inherit FSharpSyntaxModuleDecl(parent, internalNode)

    let mutable __item0 : FSharpSyntaxNodeList<FSharpSyntaxAttributeList> = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynModuleDecl.Attributes (item0, _) ->
                FSharpSyntaxNodeList<FSharpSyntaxAttributeList>(this, item0)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxModuleDeclHashDirective internal (parent: FSharpSyntaxNode, internalNode: SynModuleDecl) =
    inherit FSharpSyntaxModuleDecl(parent, internalNode)

    let mutable __item0 : FSharpSyntaxParsedHashDirective = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynModuleDecl.HashDirective (item0, _) ->
                FSharpSyntaxParsedHashDirective(this, item0)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxModuleDeclNamespaceFragment internal (parent: FSharpSyntaxNode, internalNode: SynModuleDecl) =
    inherit FSharpSyntaxModuleDecl(parent, internalNode)

    let mutable __item0 : FSharpSyntaxModuleOrNamespace = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynModuleDecl.NamespaceFragment (item0) ->
                FSharpSyntaxModuleOrNamespace(this, item0)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<AbstractClass>]
type FSharpSyntaxAttributeList internal (parent: FSharpSyntaxNode, internalNode: SynAttributeList) =
    inherit FSharpSyntaxNode (parent)

    member _.InternalNode = internalNode

    override _.Range = FSharpSourceRange internalNode.Range


[<AbstractClass>]
type FSharpSyntaxModuleSigDecl internal (parent: FSharpSyntaxNode, internalNode: SynModuleSigDecl) =
    inherit FSharpSyntaxNode (parent)

    member _.InternalNode = internalNode

    override _.Range = FSharpSourceRange internalNode.Range


[<Sealed>]
type FSharpSyntaxModuleSigDeclModuleAbbrev internal (parent: FSharpSyntaxNode, internalNode: SynModuleSigDecl) =
    inherit FSharpSyntaxModuleSigDecl(parent, internalNode)

    let mutable __item0 : FSharpSyntaxIdent = Unchecked.defaultof<_>
    let mutable __item1 : FSharpSyntaxNodeList<FSharpSyntaxIdent> = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynModuleSigDecl.ModuleAbbrev (item0, _, _) ->
                FSharpSyntaxIdent(this, item0)
            | _ -> failwith "invalid syntax" 

    member this.item1 =
            match this.InternalNode with
            | SynModuleSigDecl.ModuleAbbrev (_, item1, _) ->
                FSharpSyntaxNodeList<FSharpSyntaxIdent>(this, item1)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxModuleSigDeclNestedModule internal (parent: FSharpSyntaxNode, internalNode: SynModuleSigDecl) =
    inherit FSharpSyntaxModuleSigDecl(parent, internalNode)

    let mutable __item0 : FSharpSyntaxComponentInfo = Unchecked.defaultof<_>
    let mutable __item2 : FSharpSyntaxNodeList<FSharpSyntaxModuleSigDecl> = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynModuleSigDecl.NestedModule (item0, _, _, _) ->
                FSharpSyntaxComponentInfo(this, item0)
            | _ -> failwith "invalid syntax" 

    member this.item2 =
            match this.InternalNode with
            | SynModuleSigDecl.NestedModule (_, _, item2, _) ->
                FSharpSyntaxNodeList<FSharpSyntaxModuleSigDecl>(this, item2)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxModuleSigDeclVal internal (parent: FSharpSyntaxNode, internalNode: SynModuleSigDecl) =
    inherit FSharpSyntaxModuleSigDecl(parent, internalNode)

    let mutable __item0 : FSharpSyntaxValSig = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynModuleSigDecl.Val (item0, _) ->
                FSharpSyntaxValSig(this, item0)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxModuleSigDeclTypes internal (parent: FSharpSyntaxNode, internalNode: SynModuleSigDecl) =
    inherit FSharpSyntaxModuleSigDecl(parent, internalNode)

    let mutable __item0 : FSharpSyntaxNodeList<FSharpSyntaxTypeDefnSig> = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynModuleSigDecl.Types (item0, _) ->
                FSharpSyntaxNodeList<FSharpSyntaxTypeDefnSig>(this, item0)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxModuleSigDeclException internal (parent: FSharpSyntaxNode, internalNode: SynModuleSigDecl) =
    inherit FSharpSyntaxModuleSigDecl(parent, internalNode)

    let mutable __item0 : FSharpSyntaxExceptionSig = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynModuleSigDecl.Exception (item0, _) ->
                FSharpSyntaxExceptionSig(this, item0)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxModuleSigDeclOpen internal (parent: FSharpSyntaxNode, internalNode: SynModuleSigDecl) =
    inherit FSharpSyntaxModuleSigDecl(parent, internalNode)

    let mutable __item0 : FSharpSyntaxNodeList<FSharpSyntaxIdent> = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynModuleSigDecl.Open (item0, _) ->
                FSharpSyntaxNodeList<FSharpSyntaxIdent>(this, item0)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxModuleSigDeclHashDirective internal (parent: FSharpSyntaxNode, internalNode: SynModuleSigDecl) =
    inherit FSharpSyntaxModuleSigDecl(parent, internalNode)

    let mutable __item0 : FSharpSyntaxParsedHashDirective = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynModuleSigDecl.HashDirective (item0, _) ->
                FSharpSyntaxParsedHashDirective(this, item0)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxModuleSigDeclNamespaceFragment internal (parent: FSharpSyntaxNode, internalNode: SynModuleSigDecl) =
    inherit FSharpSyntaxModuleSigDecl(parent, internalNode)

    let mutable __item0 : FSharpSyntaxModuleOrNamespaceSig = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynModuleSigDecl.NamespaceFragment (item0) ->
                FSharpSyntaxModuleOrNamespaceSig(this, item0)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<AbstractClass>]
type FSharpSyntaxComponentInfo internal (parent: FSharpSyntaxNode, internalNode: SynComponentInfo) =
    inherit FSharpSyntaxNode (parent)

    member _.InternalNode = internalNode

    override _.Range = FSharpSourceRange internalNode.Range


[<Sealed>]
type FSharpSyntaxComponentInfoComponentInfo internal (parent: FSharpSyntaxNode, internalNode: SynComponentInfo) =
    inherit FSharpSyntaxComponentInfo(parent, internalNode)

    let mutable __item0 : FSharpSyntaxNodeList<FSharpSyntaxAttributeList> = Unchecked.defaultof<_>
    let mutable __item1 : FSharpSyntaxNodeList<FSharpSyntaxTyparDecl> = Unchecked.defaultof<_>
    let mutable __item2 : FSharpSyntaxNodeList<FSharpSyntaxTypeConstraint> = Unchecked.defaultof<_>
    let mutable __item3 : FSharpSyntaxNodeList<FSharpSyntaxIdent> = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynComponentInfo.ComponentInfo (item0, _, _, _, _, _, _, _) ->
                FSharpSyntaxNodeList<FSharpSyntaxAttributeList>(this, item0)
            | _ -> failwith "invalid syntax" 

    member this.item1 =
            match this.InternalNode with
            | SynComponentInfo.ComponentInfo (_, item1, _, _, _, _, _, _) ->
                FSharpSyntaxNodeList<FSharpSyntaxTyparDecl>(this, item1)
            | _ -> failwith "invalid syntax" 

    member this.item2 =
            match this.InternalNode with
            | SynComponentInfo.ComponentInfo (_, _, item2, _, _, _, _, _) ->
                FSharpSyntaxNodeList<FSharpSyntaxTypeConstraint>(this, item2)
            | _ -> failwith "invalid syntax" 

    member this.item3 =
            match this.InternalNode with
            | SynComponentInfo.ComponentInfo (_, _, _, item3, _, _, _, _) ->
                FSharpSyntaxNodeList<FSharpSyntaxIdent>(this, item3)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<AbstractClass>]
type FSharpSyntaxBinding internal (parent: FSharpSyntaxNode, internalNode: SynBinding) =
    inherit FSharpSyntaxNode (parent)

    member _.InternalNode = internalNode

    override _.Range = FSharpSourceRange internalNode.Range


[<Sealed>]
type FSharpSyntaxBindingBinding internal (parent: FSharpSyntaxNode, internalNode: SynBinding) =
    inherit FSharpSyntaxBinding(parent, internalNode)

    let mutable __item1 : FSharpSyntaxBindingKind = Unchecked.defaultof<_>
    let mutable __item4 : FSharpSyntaxNodeList<FSharpSyntaxAttributeList> = Unchecked.defaultof<_>
    let mutable __item6 : FSharpSyntaxValData = Unchecked.defaultof<_>
    let mutable __item7 : FSharpSyntaxPat = Unchecked.defaultof<_>
    let mutable __item9 : FSharpSyntaxExpr = Unchecked.defaultof<_>

    member this.item1 =
            match this.InternalNode with
            | SynBinding.Binding (_, item1, _, _, _, _, _, _, _, _, _, _) ->
                FSharpSyntaxBindingKind(this, item1)
            | _ -> failwith "invalid syntax" 

    member this.item4 =
            match this.InternalNode with
            | SynBinding.Binding (_, _, _, _, item4, _, _, _, _, _, _, _) ->
                FSharpSyntaxNodeList<FSharpSyntaxAttributeList>(this, item4)
            | _ -> failwith "invalid syntax" 

    member this.item6 =
            match this.InternalNode with
            | SynBinding.Binding (_, _, _, _, _, _, item6, _, _, _, _, _) ->
                FSharpSyntaxValData(this, item6)
            | _ -> failwith "invalid syntax" 

    member this.item7 =
            match this.InternalNode with
            | SynBinding.Binding (_, _, _, _, _, _, _, item7, _, _, _, _) ->
                FSharpSyntaxPat(this, item7)
            | _ -> failwith "invalid syntax" 

    member this.item9 =
            match this.InternalNode with
            | SynBinding.Binding (_, _, _, _, _, _, _, _, _, item9, _, _) ->
                FSharpSyntaxExpr(this, item9)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<AbstractClass>]
type FSharpSyntaxExpr internal (parent: FSharpSyntaxNode, internalNode: SynExpr) =
    inherit FSharpSyntaxNode (parent)

    member _.InternalNode = internalNode

    override _.Range = FSharpSourceRange internalNode.Range


[<Sealed>]
type FSharpSyntaxExprParen internal (parent: FSharpSyntaxNode, internalNode: SynExpr) =
    inherit FSharpSyntaxExpr(parent, internalNode)

    let mutable __item0 : FSharpSyntaxExpr = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynExpr.Paren (item0, _, _, _) ->
                FSharpSyntaxExpr(this, item0)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxExprQuote internal (parent: FSharpSyntaxNode, internalNode: SynExpr) =
    inherit FSharpSyntaxExpr(parent, internalNode)

    let mutable __item0 : FSharpSyntaxExpr = Unchecked.defaultof<_>
    let mutable __item2 : FSharpSyntaxExpr = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynExpr.Quote (item0, _, _, _, _) ->
                FSharpSyntaxExpr(this, item0)
            | _ -> failwith "invalid syntax" 

    member this.item2 =
            match this.InternalNode with
            | SynExpr.Quote (_, _, item2, _, _) ->
                FSharpSyntaxExpr(this, item2)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxExprConst internal (parent: FSharpSyntaxNode, internalNode: SynExpr) =
    inherit FSharpSyntaxExpr(parent, internalNode)

    let mutable __item0 : FSharpSyntaxConst = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynExpr.Const (item0, _) ->
                FSharpSyntaxConst(this, item0)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxExprTyped internal (parent: FSharpSyntaxNode, internalNode: SynExpr) =
    inherit FSharpSyntaxExpr(parent, internalNode)

    let mutable __item0 : FSharpSyntaxExpr = Unchecked.defaultof<_>
    let mutable __item1 : FSharpSyntaxType = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynExpr.Typed (item0, _, _) ->
                FSharpSyntaxExpr(this, item0)
            | _ -> failwith "invalid syntax" 

    member this.item1 =
            match this.InternalNode with
            | SynExpr.Typed (_, item1, _) ->
                FSharpSyntaxType(this, item1)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxExprTuple internal (parent: FSharpSyntaxNode, internalNode: SynExpr) =
    inherit FSharpSyntaxExpr(parent, internalNode)

    let mutable __item1 : FSharpSyntaxNodeList<FSharpSyntaxExpr> = Unchecked.defaultof<_>

    member this.item1 =
            match this.InternalNode with
            | SynExpr.Tuple (_, item1, _, _) ->
                FSharpSyntaxNodeList<FSharpSyntaxExpr>(this, item1)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxExprAnonRecd internal (parent: FSharpSyntaxNode, internalNode: SynExpr) =
    inherit FSharpSyntaxExpr(parent, internalNode)






    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxExprArrayOrList internal (parent: FSharpSyntaxNode, internalNode: SynExpr) =
    inherit FSharpSyntaxExpr(parent, internalNode)

    let mutable __item1 : FSharpSyntaxNodeList<FSharpSyntaxExpr> = Unchecked.defaultof<_>

    member this.item1 =
            match this.InternalNode with
            | SynExpr.ArrayOrList (_, item1, _) ->
                FSharpSyntaxNodeList<FSharpSyntaxExpr>(this, item1)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxExprRecord internal (parent: FSharpSyntaxNode, internalNode: SynExpr) =
    inherit FSharpSyntaxExpr(parent, internalNode)






    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxExprNew internal (parent: FSharpSyntaxNode, internalNode: SynExpr) =
    inherit FSharpSyntaxExpr(parent, internalNode)

    let mutable __item1 : FSharpSyntaxType = Unchecked.defaultof<_>
    let mutable __item2 : FSharpSyntaxExpr = Unchecked.defaultof<_>

    member this.item1 =
            match this.InternalNode with
            | SynExpr.New (_, item1, _, _) ->
                FSharpSyntaxType(this, item1)
            | _ -> failwith "invalid syntax" 

    member this.item2 =
            match this.InternalNode with
            | SynExpr.New (_, _, item2, _) ->
                FSharpSyntaxExpr(this, item2)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxExprObjExpr internal (parent: FSharpSyntaxNode, internalNode: SynExpr) =
    inherit FSharpSyntaxExpr(parent, internalNode)

    let mutable __item0 : FSharpSyntaxType = Unchecked.defaultof<_>
    let mutable __item2 : FSharpSyntaxNodeList<FSharpSyntaxBinding> = Unchecked.defaultof<_>
    let mutable __item3 : FSharpSyntaxNodeList<FSharpSyntaxInterfaceImpl> = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynExpr.ObjExpr (item0, _, _, _, _, _) ->
                FSharpSyntaxType(this, item0)
            | _ -> failwith "invalid syntax" 

    member this.item2 =
            match this.InternalNode with
            | SynExpr.ObjExpr (_, _, item2, _, _, _) ->
                FSharpSyntaxNodeList<FSharpSyntaxBinding>(this, item2)
            | _ -> failwith "invalid syntax" 

    member this.item3 =
            match this.InternalNode with
            | SynExpr.ObjExpr (_, _, _, item3, _, _) ->
                FSharpSyntaxNodeList<FSharpSyntaxInterfaceImpl>(this, item3)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxExprWhile internal (parent: FSharpSyntaxNode, internalNode: SynExpr) =
    inherit FSharpSyntaxExpr(parent, internalNode)

    let mutable __item1 : FSharpSyntaxExpr = Unchecked.defaultof<_>
    let mutable __item2 : FSharpSyntaxExpr = Unchecked.defaultof<_>

    member this.item1 =
            match this.InternalNode with
            | SynExpr.While (_, item1, _, _) ->
                FSharpSyntaxExpr(this, item1)
            | _ -> failwith "invalid syntax" 

    member this.item2 =
            match this.InternalNode with
            | SynExpr.While (_, _, item2, _) ->
                FSharpSyntaxExpr(this, item2)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxExprFor internal (parent: FSharpSyntaxNode, internalNode: SynExpr) =
    inherit FSharpSyntaxExpr(parent, internalNode)

    let mutable __item1 : FSharpSyntaxIdent = Unchecked.defaultof<_>
    let mutable __item2 : FSharpSyntaxExpr = Unchecked.defaultof<_>
    let mutable __item4 : FSharpSyntaxExpr = Unchecked.defaultof<_>
    let mutable __item5 : FSharpSyntaxExpr = Unchecked.defaultof<_>

    member this.item1 =
            match this.InternalNode with
            | SynExpr.For (_, item1, _, _, _, _, _) ->
                FSharpSyntaxIdent(this, item1)
            | _ -> failwith "invalid syntax" 

    member this.item2 =
            match this.InternalNode with
            | SynExpr.For (_, _, item2, _, _, _, _) ->
                FSharpSyntaxExpr(this, item2)
            | _ -> failwith "invalid syntax" 

    member this.item4 =
            match this.InternalNode with
            | SynExpr.For (_, _, _, _, item4, _, _) ->
                FSharpSyntaxExpr(this, item4)
            | _ -> failwith "invalid syntax" 

    member this.item5 =
            match this.InternalNode with
            | SynExpr.For (_, _, _, _, _, item5, _) ->
                FSharpSyntaxExpr(this, item5)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxExprForEach internal (parent: FSharpSyntaxNode, internalNode: SynExpr) =
    inherit FSharpSyntaxExpr(parent, internalNode)

    let mutable __item3 : FSharpSyntaxPat = Unchecked.defaultof<_>
    let mutable __item4 : FSharpSyntaxExpr = Unchecked.defaultof<_>
    let mutable __item5 : FSharpSyntaxExpr = Unchecked.defaultof<_>

    member this.item3 =
            match this.InternalNode with
            | SynExpr.ForEach (_, _, _, item3, _, _, _) ->
                FSharpSyntaxPat(this, item3)
            | _ -> failwith "invalid syntax" 

    member this.item4 =
            match this.InternalNode with
            | SynExpr.ForEach (_, _, _, _, item4, _, _) ->
                FSharpSyntaxExpr(this, item4)
            | _ -> failwith "invalid syntax" 

    member this.item5 =
            match this.InternalNode with
            | SynExpr.ForEach (_, _, _, _, _, item5, _) ->
                FSharpSyntaxExpr(this, item5)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxExprArrayOrListOfSeqExpr internal (parent: FSharpSyntaxNode, internalNode: SynExpr) =
    inherit FSharpSyntaxExpr(parent, internalNode)

    let mutable __item1 : FSharpSyntaxExpr = Unchecked.defaultof<_>

    member this.item1 =
            match this.InternalNode with
            | SynExpr.ArrayOrListOfSeqExpr (_, item1, _) ->
                FSharpSyntaxExpr(this, item1)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxExprCompExpr internal (parent: FSharpSyntaxNode, internalNode: SynExpr) =
    inherit FSharpSyntaxExpr(parent, internalNode)

    let mutable __item2 : FSharpSyntaxExpr = Unchecked.defaultof<_>

    member this.item2 =
            match this.InternalNode with
            | SynExpr.CompExpr (_, _, item2, _) ->
                FSharpSyntaxExpr(this, item2)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxExprLambda internal (parent: FSharpSyntaxNode, internalNode: SynExpr) =
    inherit FSharpSyntaxExpr(parent, internalNode)

    let mutable __item2 : FSharpSyntaxSimplePats = Unchecked.defaultof<_>
    let mutable __item3 : FSharpSyntaxExpr = Unchecked.defaultof<_>

    member this.item2 =
            match this.InternalNode with
            | SynExpr.Lambda (_, _, item2, _, _) ->
                FSharpSyntaxSimplePats(this, item2)
            | _ -> failwith "invalid syntax" 

    member this.item3 =
            match this.InternalNode with
            | SynExpr.Lambda (_, _, _, item3, _) ->
                FSharpSyntaxExpr(this, item3)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxExprMatchLambda internal (parent: FSharpSyntaxNode, internalNode: SynExpr) =
    inherit FSharpSyntaxExpr(parent, internalNode)

    let mutable __item2 : FSharpSyntaxNodeList<FSharpSyntaxMatchClause> = Unchecked.defaultof<_>

    member this.item2 =
            match this.InternalNode with
            | SynExpr.MatchLambda (_, _, item2, _, _) ->
                FSharpSyntaxNodeList<FSharpSyntaxMatchClause>(this, item2)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxExprMatch internal (parent: FSharpSyntaxNode, internalNode: SynExpr) =
    inherit FSharpSyntaxExpr(parent, internalNode)

    let mutable __item1 : FSharpSyntaxExpr = Unchecked.defaultof<_>
    let mutable __item2 : FSharpSyntaxNodeList<FSharpSyntaxMatchClause> = Unchecked.defaultof<_>

    member this.item1 =
            match this.InternalNode with
            | SynExpr.Match (_, item1, _, _) ->
                FSharpSyntaxExpr(this, item1)
            | _ -> failwith "invalid syntax" 

    member this.item2 =
            match this.InternalNode with
            | SynExpr.Match (_, _, item2, _) ->
                FSharpSyntaxNodeList<FSharpSyntaxMatchClause>(this, item2)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxExprDo internal (parent: FSharpSyntaxNode, internalNode: SynExpr) =
    inherit FSharpSyntaxExpr(parent, internalNode)

    let mutable __item0 : FSharpSyntaxExpr = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynExpr.Do (item0, _) ->
                FSharpSyntaxExpr(this, item0)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxExprAssert internal (parent: FSharpSyntaxNode, internalNode: SynExpr) =
    inherit FSharpSyntaxExpr(parent, internalNode)

    let mutable __item0 : FSharpSyntaxExpr = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynExpr.Assert (item0, _) ->
                FSharpSyntaxExpr(this, item0)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxExprApp internal (parent: FSharpSyntaxNode, internalNode: SynExpr) =
    inherit FSharpSyntaxExpr(parent, internalNode)

    let mutable __item2 : FSharpSyntaxExpr = Unchecked.defaultof<_>
    let mutable __item3 : FSharpSyntaxExpr = Unchecked.defaultof<_>

    member this.item2 =
            match this.InternalNode with
            | SynExpr.App (_, _, item2, _, _) ->
                FSharpSyntaxExpr(this, item2)
            | _ -> failwith "invalid syntax" 

    member this.item3 =
            match this.InternalNode with
            | SynExpr.App (_, _, _, item3, _) ->
                FSharpSyntaxExpr(this, item3)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxExprTypeApp internal (parent: FSharpSyntaxNode, internalNode: SynExpr) =
    inherit FSharpSyntaxExpr(parent, internalNode)

    let mutable __item0 : FSharpSyntaxExpr = Unchecked.defaultof<_>
    let mutable __item2 : FSharpSyntaxNodeList<FSharpSyntaxType> = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynExpr.TypeApp (item0, _, _, _, _, _, _) ->
                FSharpSyntaxExpr(this, item0)
            | _ -> failwith "invalid syntax" 

    member this.item2 =
            match this.InternalNode with
            | SynExpr.TypeApp (_, _, item2, _, _, _, _) ->
                FSharpSyntaxNodeList<FSharpSyntaxType>(this, item2)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxExprLetOrUse internal (parent: FSharpSyntaxNode, internalNode: SynExpr) =
    inherit FSharpSyntaxExpr(parent, internalNode)

    let mutable __item2 : FSharpSyntaxNodeList<FSharpSyntaxBinding> = Unchecked.defaultof<_>
    let mutable __item3 : FSharpSyntaxExpr = Unchecked.defaultof<_>

    member this.item2 =
            match this.InternalNode with
            | SynExpr.LetOrUse (_, _, item2, _, _) ->
                FSharpSyntaxNodeList<FSharpSyntaxBinding>(this, item2)
            | _ -> failwith "invalid syntax" 

    member this.item3 =
            match this.InternalNode with
            | SynExpr.LetOrUse (_, _, _, item3, _) ->
                FSharpSyntaxExpr(this, item3)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxExprTryWith internal (parent: FSharpSyntaxNode, internalNode: SynExpr) =
    inherit FSharpSyntaxExpr(parent, internalNode)

    let mutable __item0 : FSharpSyntaxExpr = Unchecked.defaultof<_>
    let mutable __item2 : FSharpSyntaxNodeList<FSharpSyntaxMatchClause> = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynExpr.TryWith (item0, _, _, _, _, _, _) ->
                FSharpSyntaxExpr(this, item0)
            | _ -> failwith "invalid syntax" 

    member this.item2 =
            match this.InternalNode with
            | SynExpr.TryWith (_, _, item2, _, _, _, _) ->
                FSharpSyntaxNodeList<FSharpSyntaxMatchClause>(this, item2)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxExprTryFinally internal (parent: FSharpSyntaxNode, internalNode: SynExpr) =
    inherit FSharpSyntaxExpr(parent, internalNode)

    let mutable __item0 : FSharpSyntaxExpr = Unchecked.defaultof<_>
    let mutable __item1 : FSharpSyntaxExpr = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynExpr.TryFinally (item0, _, _, _, _) ->
                FSharpSyntaxExpr(this, item0)
            | _ -> failwith "invalid syntax" 

    member this.item1 =
            match this.InternalNode with
            | SynExpr.TryFinally (_, item1, _, _, _) ->
                FSharpSyntaxExpr(this, item1)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxExprLazy internal (parent: FSharpSyntaxNode, internalNode: SynExpr) =
    inherit FSharpSyntaxExpr(parent, internalNode)

    let mutable __item0 : FSharpSyntaxExpr = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynExpr.Lazy (item0, _) ->
                FSharpSyntaxExpr(this, item0)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxExprSequential internal (parent: FSharpSyntaxNode, internalNode: SynExpr) =
    inherit FSharpSyntaxExpr(parent, internalNode)

    let mutable __item2 : FSharpSyntaxExpr = Unchecked.defaultof<_>
    let mutable __item3 : FSharpSyntaxExpr = Unchecked.defaultof<_>

    member this.item2 =
            match this.InternalNode with
            | SynExpr.Sequential (_, _, item2, _, _) ->
                FSharpSyntaxExpr(this, item2)
            | _ -> failwith "invalid syntax" 

    member this.item3 =
            match this.InternalNode with
            | SynExpr.Sequential (_, _, _, item3, _) ->
                FSharpSyntaxExpr(this, item3)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxExprIfThenElse internal (parent: FSharpSyntaxNode, internalNode: SynExpr) =
    inherit FSharpSyntaxExpr(parent, internalNode)

    let mutable __item0 : FSharpSyntaxExpr = Unchecked.defaultof<_>
    let mutable __item1 : FSharpSyntaxExpr = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynExpr.IfThenElse (item0, _, _, _, _, _, _) ->
                FSharpSyntaxExpr(this, item0)
            | _ -> failwith "invalid syntax" 

    member this.item1 =
            match this.InternalNode with
            | SynExpr.IfThenElse (_, item1, _, _, _, _, _) ->
                FSharpSyntaxExpr(this, item1)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxExprIdent internal (parent: FSharpSyntaxNode, internalNode: SynExpr) =
    inherit FSharpSyntaxExpr(parent, internalNode)

    let mutable __item0 : FSharpSyntaxIdent = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynExpr.Ident (item0) ->
                FSharpSyntaxIdent(this, item0)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxExprLongIdent internal (parent: FSharpSyntaxNode, internalNode: SynExpr) =
    inherit FSharpSyntaxExpr(parent, internalNode)

    let mutable __item1 : FSharpSyntaxLongIdentWithDots = Unchecked.defaultof<_>

    member this.item1 =
            match this.InternalNode with
            | SynExpr.LongIdent (_, item1, _, _) ->
                FSharpSyntaxLongIdentWithDots(this, item1)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxExprLongIdentSet internal (parent: FSharpSyntaxNode, internalNode: SynExpr) =
    inherit FSharpSyntaxExpr(parent, internalNode)

    let mutable __item0 : FSharpSyntaxLongIdentWithDots = Unchecked.defaultof<_>
    let mutable __item1 : FSharpSyntaxExpr = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynExpr.LongIdentSet (item0, _, _) ->
                FSharpSyntaxLongIdentWithDots(this, item0)
            | _ -> failwith "invalid syntax" 

    member this.item1 =
            match this.InternalNode with
            | SynExpr.LongIdentSet (_, item1, _) ->
                FSharpSyntaxExpr(this, item1)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxExprDotGet internal (parent: FSharpSyntaxNode, internalNode: SynExpr) =
    inherit FSharpSyntaxExpr(parent, internalNode)

    let mutable __item0 : FSharpSyntaxExpr = Unchecked.defaultof<_>
    let mutable __item2 : FSharpSyntaxLongIdentWithDots = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynExpr.DotGet (item0, _, _, _) ->
                FSharpSyntaxExpr(this, item0)
            | _ -> failwith "invalid syntax" 

    member this.item2 =
            match this.InternalNode with
            | SynExpr.DotGet (_, _, item2, _) ->
                FSharpSyntaxLongIdentWithDots(this, item2)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxExprDotSet internal (parent: FSharpSyntaxNode, internalNode: SynExpr) =
    inherit FSharpSyntaxExpr(parent, internalNode)

    let mutable __item0 : FSharpSyntaxExpr = Unchecked.defaultof<_>
    let mutable __item1 : FSharpSyntaxLongIdentWithDots = Unchecked.defaultof<_>
    let mutable __item2 : FSharpSyntaxExpr = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynExpr.DotSet (item0, _, _, _) ->
                FSharpSyntaxExpr(this, item0)
            | _ -> failwith "invalid syntax" 

    member this.item1 =
            match this.InternalNode with
            | SynExpr.DotSet (_, item1, _, _) ->
                FSharpSyntaxLongIdentWithDots(this, item1)
            | _ -> failwith "invalid syntax" 

    member this.item2 =
            match this.InternalNode with
            | SynExpr.DotSet (_, _, item2, _) ->
                FSharpSyntaxExpr(this, item2)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxExprSet internal (parent: FSharpSyntaxNode, internalNode: SynExpr) =
    inherit FSharpSyntaxExpr(parent, internalNode)

    let mutable __item0 : FSharpSyntaxExpr = Unchecked.defaultof<_>
    let mutable __item1 : FSharpSyntaxExpr = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynExpr.Set (item0, _, _) ->
                FSharpSyntaxExpr(this, item0)
            | _ -> failwith "invalid syntax" 

    member this.item1 =
            match this.InternalNode with
            | SynExpr.Set (_, item1, _) ->
                FSharpSyntaxExpr(this, item1)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxExprDotIndexedGet internal (parent: FSharpSyntaxNode, internalNode: SynExpr) =
    inherit FSharpSyntaxExpr(parent, internalNode)

    let mutable __item0 : FSharpSyntaxExpr = Unchecked.defaultof<_>
    let mutable __item1 : FSharpSyntaxNodeList<FSharpSyntaxIndexerArg> = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynExpr.DotIndexedGet (item0, _, _, _) ->
                FSharpSyntaxExpr(this, item0)
            | _ -> failwith "invalid syntax" 

    member this.item1 =
            match this.InternalNode with
            | SynExpr.DotIndexedGet (_, item1, _, _) ->
                FSharpSyntaxNodeList<FSharpSyntaxIndexerArg>(this, item1)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxExprDotIndexedSet internal (parent: FSharpSyntaxNode, internalNode: SynExpr) =
    inherit FSharpSyntaxExpr(parent, internalNode)

    let mutable __item0 : FSharpSyntaxExpr = Unchecked.defaultof<_>
    let mutable __item1 : FSharpSyntaxNodeList<FSharpSyntaxIndexerArg> = Unchecked.defaultof<_>
    let mutable __item2 : FSharpSyntaxExpr = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynExpr.DotIndexedSet (item0, _, _, _, _, _) ->
                FSharpSyntaxExpr(this, item0)
            | _ -> failwith "invalid syntax" 

    member this.item1 =
            match this.InternalNode with
            | SynExpr.DotIndexedSet (_, item1, _, _, _, _) ->
                FSharpSyntaxNodeList<FSharpSyntaxIndexerArg>(this, item1)
            | _ -> failwith "invalid syntax" 

    member this.item2 =
            match this.InternalNode with
            | SynExpr.DotIndexedSet (_, _, item2, _, _, _) ->
                FSharpSyntaxExpr(this, item2)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxExprNamedIndexedPropertySet internal (parent: FSharpSyntaxNode, internalNode: SynExpr) =
    inherit FSharpSyntaxExpr(parent, internalNode)

    let mutable __item0 : FSharpSyntaxLongIdentWithDots = Unchecked.defaultof<_>
    let mutable __item1 : FSharpSyntaxExpr = Unchecked.defaultof<_>
    let mutable __item2 : FSharpSyntaxExpr = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynExpr.NamedIndexedPropertySet (item0, _, _, _) ->
                FSharpSyntaxLongIdentWithDots(this, item0)
            | _ -> failwith "invalid syntax" 

    member this.item1 =
            match this.InternalNode with
            | SynExpr.NamedIndexedPropertySet (_, item1, _, _) ->
                FSharpSyntaxExpr(this, item1)
            | _ -> failwith "invalid syntax" 

    member this.item2 =
            match this.InternalNode with
            | SynExpr.NamedIndexedPropertySet (_, _, item2, _) ->
                FSharpSyntaxExpr(this, item2)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxExprDotNamedIndexedPropertySet internal (parent: FSharpSyntaxNode, internalNode: SynExpr) =
    inherit FSharpSyntaxExpr(parent, internalNode)

    let mutable __item0 : FSharpSyntaxExpr = Unchecked.defaultof<_>
    let mutable __item1 : FSharpSyntaxLongIdentWithDots = Unchecked.defaultof<_>
    let mutable __item2 : FSharpSyntaxExpr = Unchecked.defaultof<_>
    let mutable __item3 : FSharpSyntaxExpr = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynExpr.DotNamedIndexedPropertySet (item0, _, _, _, _) ->
                FSharpSyntaxExpr(this, item0)
            | _ -> failwith "invalid syntax" 

    member this.item1 =
            match this.InternalNode with
            | SynExpr.DotNamedIndexedPropertySet (_, item1, _, _, _) ->
                FSharpSyntaxLongIdentWithDots(this, item1)
            | _ -> failwith "invalid syntax" 

    member this.item2 =
            match this.InternalNode with
            | SynExpr.DotNamedIndexedPropertySet (_, _, item2, _, _) ->
                FSharpSyntaxExpr(this, item2)
            | _ -> failwith "invalid syntax" 

    member this.item3 =
            match this.InternalNode with
            | SynExpr.DotNamedIndexedPropertySet (_, _, _, item3, _) ->
                FSharpSyntaxExpr(this, item3)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxExprTypeTest internal (parent: FSharpSyntaxNode, internalNode: SynExpr) =
    inherit FSharpSyntaxExpr(parent, internalNode)

    let mutable __item0 : FSharpSyntaxExpr = Unchecked.defaultof<_>
    let mutable __item1 : FSharpSyntaxType = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynExpr.TypeTest (item0, _, _) ->
                FSharpSyntaxExpr(this, item0)
            | _ -> failwith "invalid syntax" 

    member this.item1 =
            match this.InternalNode with
            | SynExpr.TypeTest (_, item1, _) ->
                FSharpSyntaxType(this, item1)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxExprUpcast internal (parent: FSharpSyntaxNode, internalNode: SynExpr) =
    inherit FSharpSyntaxExpr(parent, internalNode)

    let mutable __item0 : FSharpSyntaxExpr = Unchecked.defaultof<_>
    let mutable __item1 : FSharpSyntaxType = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynExpr.Upcast (item0, _, _) ->
                FSharpSyntaxExpr(this, item0)
            | _ -> failwith "invalid syntax" 

    member this.item1 =
            match this.InternalNode with
            | SynExpr.Upcast (_, item1, _) ->
                FSharpSyntaxType(this, item1)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxExprDowncast internal (parent: FSharpSyntaxNode, internalNode: SynExpr) =
    inherit FSharpSyntaxExpr(parent, internalNode)

    let mutable __item0 : FSharpSyntaxExpr = Unchecked.defaultof<_>
    let mutable __item1 : FSharpSyntaxType = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynExpr.Downcast (item0, _, _) ->
                FSharpSyntaxExpr(this, item0)
            | _ -> failwith "invalid syntax" 

    member this.item1 =
            match this.InternalNode with
            | SynExpr.Downcast (_, item1, _) ->
                FSharpSyntaxType(this, item1)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxExprInferredUpcast internal (parent: FSharpSyntaxNode, internalNode: SynExpr) =
    inherit FSharpSyntaxExpr(parent, internalNode)

    let mutable __item0 : FSharpSyntaxExpr = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynExpr.InferredUpcast (item0, _) ->
                FSharpSyntaxExpr(this, item0)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxExprInferredDowncast internal (parent: FSharpSyntaxNode, internalNode: SynExpr) =
    inherit FSharpSyntaxExpr(parent, internalNode)

    let mutable __item0 : FSharpSyntaxExpr = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynExpr.InferredDowncast (item0, _) ->
                FSharpSyntaxExpr(this, item0)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxExprNull internal (parent: FSharpSyntaxNode, internalNode: SynExpr) =
    inherit FSharpSyntaxExpr(parent, internalNode)






    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxExprAddressOf internal (parent: FSharpSyntaxNode, internalNode: SynExpr) =
    inherit FSharpSyntaxExpr(parent, internalNode)

    let mutable __item1 : FSharpSyntaxExpr = Unchecked.defaultof<_>

    member this.item1 =
            match this.InternalNode with
            | SynExpr.AddressOf (_, item1, _, _) ->
                FSharpSyntaxExpr(this, item1)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxExprTraitCall internal (parent: FSharpSyntaxNode, internalNode: SynExpr) =
    inherit FSharpSyntaxExpr(parent, internalNode)

    let mutable __item0 : FSharpSyntaxNodeList<FSharpSyntaxTypar> = Unchecked.defaultof<_>
    let mutable __item1 : FSharpSyntaxMemberSig = Unchecked.defaultof<_>
    let mutable __item2 : FSharpSyntaxExpr = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynExpr.TraitCall (item0, _, _, _) ->
                FSharpSyntaxNodeList<FSharpSyntaxTypar>(this, item0)
            | _ -> failwith "invalid syntax" 

    member this.item1 =
            match this.InternalNode with
            | SynExpr.TraitCall (_, item1, _, _) ->
                FSharpSyntaxMemberSig(this, item1)
            | _ -> failwith "invalid syntax" 

    member this.item2 =
            match this.InternalNode with
            | SynExpr.TraitCall (_, _, item2, _) ->
                FSharpSyntaxExpr(this, item2)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxExprJoinIn internal (parent: FSharpSyntaxNode, internalNode: SynExpr) =
    inherit FSharpSyntaxExpr(parent, internalNode)

    let mutable __item0 : FSharpSyntaxExpr = Unchecked.defaultof<_>
    let mutable __item2 : FSharpSyntaxExpr = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynExpr.JoinIn (item0, _, _, _) ->
                FSharpSyntaxExpr(this, item0)
            | _ -> failwith "invalid syntax" 

    member this.item2 =
            match this.InternalNode with
            | SynExpr.JoinIn (_, _, item2, _) ->
                FSharpSyntaxExpr(this, item2)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxExprImplicitZero internal (parent: FSharpSyntaxNode, internalNode: SynExpr) =
    inherit FSharpSyntaxExpr(parent, internalNode)






    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxExprSequentialOrImplicitYield internal (parent: FSharpSyntaxNode, internalNode: SynExpr) =
    inherit FSharpSyntaxExpr(parent, internalNode)

    let mutable __item1 : FSharpSyntaxExpr = Unchecked.defaultof<_>
    let mutable __item2 : FSharpSyntaxExpr = Unchecked.defaultof<_>
    let mutable __item3 : FSharpSyntaxExpr = Unchecked.defaultof<_>

    member this.item1 =
            match this.InternalNode with
            | SynExpr.SequentialOrImplicitYield (_, item1, _, _, _) ->
                FSharpSyntaxExpr(this, item1)
            | _ -> failwith "invalid syntax" 

    member this.item2 =
            match this.InternalNode with
            | SynExpr.SequentialOrImplicitYield (_, _, item2, _, _) ->
                FSharpSyntaxExpr(this, item2)
            | _ -> failwith "invalid syntax" 

    member this.item3 =
            match this.InternalNode with
            | SynExpr.SequentialOrImplicitYield (_, _, _, item3, _) ->
                FSharpSyntaxExpr(this, item3)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxExprYieldOrReturn internal (parent: FSharpSyntaxNode, internalNode: SynExpr) =
    inherit FSharpSyntaxExpr(parent, internalNode)

    let mutable __item1 : FSharpSyntaxExpr = Unchecked.defaultof<_>

    member this.item1 =
            match this.InternalNode with
            | SynExpr.YieldOrReturn (_, item1, _) ->
                FSharpSyntaxExpr(this, item1)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxExprYieldOrReturnFrom internal (parent: FSharpSyntaxNode, internalNode: SynExpr) =
    inherit FSharpSyntaxExpr(parent, internalNode)

    let mutable __item1 : FSharpSyntaxExpr = Unchecked.defaultof<_>

    member this.item1 =
            match this.InternalNode with
            | SynExpr.YieldOrReturnFrom (_, item1, _) ->
                FSharpSyntaxExpr(this, item1)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxExprLetOrUseBang internal (parent: FSharpSyntaxNode, internalNode: SynExpr) =
    inherit FSharpSyntaxExpr(parent, internalNode)

    let mutable __item3 : FSharpSyntaxPat = Unchecked.defaultof<_>
    let mutable __item4 : FSharpSyntaxExpr = Unchecked.defaultof<_>
    let mutable __item6 : FSharpSyntaxExpr = Unchecked.defaultof<_>

    member this.item3 =
            match this.InternalNode with
            | SynExpr.LetOrUseBang (_, _, _, item3, _, _, _, _) ->
                FSharpSyntaxPat(this, item3)
            | _ -> failwith "invalid syntax" 

    member this.item4 =
            match this.InternalNode with
            | SynExpr.LetOrUseBang (_, _, _, _, item4, _, _, _) ->
                FSharpSyntaxExpr(this, item4)
            | _ -> failwith "invalid syntax" 

    member this.item6 =
            match this.InternalNode with
            | SynExpr.LetOrUseBang (_, _, _, _, _, _, item6, _) ->
                FSharpSyntaxExpr(this, item6)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxExprMatchBang internal (parent: FSharpSyntaxNode, internalNode: SynExpr) =
    inherit FSharpSyntaxExpr(parent, internalNode)

    let mutable __item1 : FSharpSyntaxExpr = Unchecked.defaultof<_>
    let mutable __item2 : FSharpSyntaxNodeList<FSharpSyntaxMatchClause> = Unchecked.defaultof<_>

    member this.item1 =
            match this.InternalNode with
            | SynExpr.MatchBang (_, item1, _, _) ->
                FSharpSyntaxExpr(this, item1)
            | _ -> failwith "invalid syntax" 

    member this.item2 =
            match this.InternalNode with
            | SynExpr.MatchBang (_, _, item2, _) ->
                FSharpSyntaxNodeList<FSharpSyntaxMatchClause>(this, item2)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxExprDoBang internal (parent: FSharpSyntaxNode, internalNode: SynExpr) =
    inherit FSharpSyntaxExpr(parent, internalNode)

    let mutable __item0 : FSharpSyntaxExpr = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynExpr.DoBang (item0, _) ->
                FSharpSyntaxExpr(this, item0)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxExprLibraryOnlyILAssembly internal (parent: FSharpSyntaxNode, internalNode: SynExpr) =
    inherit FSharpSyntaxExpr(parent, internalNode)

    let mutable __item1 : FSharpSyntaxNodeList<FSharpSyntaxType> = Unchecked.defaultof<_>
    let mutable __item2 : FSharpSyntaxNodeList<FSharpSyntaxExpr> = Unchecked.defaultof<_>
    let mutable __item3 : FSharpSyntaxNodeList<FSharpSyntaxType> = Unchecked.defaultof<_>

    member this.item1 =
            match this.InternalNode with
            | SynExpr.LibraryOnlyILAssembly (_, item1, _, _, _) ->
                FSharpSyntaxNodeList<FSharpSyntaxType>(this, item1)
            | _ -> failwith "invalid syntax" 

    member this.item2 =
            match this.InternalNode with
            | SynExpr.LibraryOnlyILAssembly (_, _, item2, _, _) ->
                FSharpSyntaxNodeList<FSharpSyntaxExpr>(this, item2)
            | _ -> failwith "invalid syntax" 

    member this.item3 =
            match this.InternalNode with
            | SynExpr.LibraryOnlyILAssembly (_, _, _, item3, _) ->
                FSharpSyntaxNodeList<FSharpSyntaxType>(this, item3)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxExprLibraryOnlyStaticOptimization internal (parent: FSharpSyntaxNode, internalNode: SynExpr) =
    inherit FSharpSyntaxExpr(parent, internalNode)

    let mutable __item0 : FSharpSyntaxNodeList<FSharpSyntaxStaticOptimizationConstraint> = Unchecked.defaultof<_>
    let mutable __item1 : FSharpSyntaxExpr = Unchecked.defaultof<_>
    let mutable __item2 : FSharpSyntaxExpr = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynExpr.LibraryOnlyStaticOptimization (item0, _, _, _) ->
                FSharpSyntaxNodeList<FSharpSyntaxStaticOptimizationConstraint>(this, item0)
            | _ -> failwith "invalid syntax" 

    member this.item1 =
            match this.InternalNode with
            | SynExpr.LibraryOnlyStaticOptimization (_, item1, _, _) ->
                FSharpSyntaxExpr(this, item1)
            | _ -> failwith "invalid syntax" 

    member this.item2 =
            match this.InternalNode with
            | SynExpr.LibraryOnlyStaticOptimization (_, _, item2, _) ->
                FSharpSyntaxExpr(this, item2)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxExprLibraryOnlyUnionCaseFieldGet internal (parent: FSharpSyntaxNode, internalNode: SynExpr) =
    inherit FSharpSyntaxExpr(parent, internalNode)

    let mutable __item0 : FSharpSyntaxExpr = Unchecked.defaultof<_>
    let mutable __item1 : FSharpSyntaxNodeList<FSharpSyntaxIdent> = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynExpr.LibraryOnlyUnionCaseFieldGet (item0, _, _, _) ->
                FSharpSyntaxExpr(this, item0)
            | _ -> failwith "invalid syntax" 

    member this.item1 =
            match this.InternalNode with
            | SynExpr.LibraryOnlyUnionCaseFieldGet (_, item1, _, _) ->
                FSharpSyntaxNodeList<FSharpSyntaxIdent>(this, item1)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxExprLibraryOnlyUnionCaseFieldSet internal (parent: FSharpSyntaxNode, internalNode: SynExpr) =
    inherit FSharpSyntaxExpr(parent, internalNode)

    let mutable __item0 : FSharpSyntaxExpr = Unchecked.defaultof<_>
    let mutable __item1 : FSharpSyntaxNodeList<FSharpSyntaxIdent> = Unchecked.defaultof<_>
    let mutable __item3 : FSharpSyntaxExpr = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynExpr.LibraryOnlyUnionCaseFieldSet (item0, _, _, _, _) ->
                FSharpSyntaxExpr(this, item0)
            | _ -> failwith "invalid syntax" 

    member this.item1 =
            match this.InternalNode with
            | SynExpr.LibraryOnlyUnionCaseFieldSet (_, item1, _, _, _) ->
                FSharpSyntaxNodeList<FSharpSyntaxIdent>(this, item1)
            | _ -> failwith "invalid syntax" 

    member this.item3 =
            match this.InternalNode with
            | SynExpr.LibraryOnlyUnionCaseFieldSet (_, _, _, item3, _) ->
                FSharpSyntaxExpr(this, item3)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxExprArbitraryAfterError internal (parent: FSharpSyntaxNode, internalNode: SynExpr) =
    inherit FSharpSyntaxExpr(parent, internalNode)






    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxExprFromParseError internal (parent: FSharpSyntaxNode, internalNode: SynExpr) =
    inherit FSharpSyntaxExpr(parent, internalNode)

    let mutable __item0 : FSharpSyntaxExpr = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynExpr.FromParseError (item0, _) ->
                FSharpSyntaxExpr(this, item0)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxExprDiscardAfterMissingQualificationAfterDot internal (parent: FSharpSyntaxNode, internalNode: SynExpr) =
    inherit FSharpSyntaxExpr(parent, internalNode)

    let mutable __item0 : FSharpSyntaxExpr = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynExpr.DiscardAfterMissingQualificationAfterDot (item0, _) ->
                FSharpSyntaxExpr(this, item0)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxExprFixed internal (parent: FSharpSyntaxNode, internalNode: SynExpr) =
    inherit FSharpSyntaxExpr(parent, internalNode)

    let mutable __item0 : FSharpSyntaxExpr = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynExpr.Fixed (item0, _) ->
                FSharpSyntaxExpr(this, item0)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<AbstractClass>]
type FSharpSyntaxTypeDefn internal (parent: FSharpSyntaxNode, internalNode: SynTypeDefn) =
    inherit FSharpSyntaxNode (parent)

    member _.InternalNode = internalNode

    override _.Range = FSharpSourceRange internalNode.Range


[<Sealed>]
type FSharpSyntaxTypeDefnTypeDefn internal (parent: FSharpSyntaxNode, internalNode: SynTypeDefn) =
    inherit FSharpSyntaxTypeDefn(parent, internalNode)

    let mutable __item0 : FSharpSyntaxComponentInfo = Unchecked.defaultof<_>
    let mutable __item1 : FSharpSyntaxTypeDefnRepr = Unchecked.defaultof<_>
    let mutable __item2 : FSharpSyntaxNodeList<FSharpSyntaxMemberDefn> = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynTypeDefn.TypeDefn (item0, _, _, _) ->
                FSharpSyntaxComponentInfo(this, item0)
            | _ -> failwith "invalid syntax" 

    member this.item1 =
            match this.InternalNode with
            | SynTypeDefn.TypeDefn (_, item1, _, _) ->
                FSharpSyntaxTypeDefnRepr(this, item1)
            | _ -> failwith "invalid syntax" 

    member this.item2 =
            match this.InternalNode with
            | SynTypeDefn.TypeDefn (_, _, item2, _) ->
                FSharpSyntaxNodeList<FSharpSyntaxMemberDefn>(this, item2)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<AbstractClass>]
type FSharpSyntaxExceptionDefn internal (parent: FSharpSyntaxNode, internalNode: SynExceptionDefn) =
    inherit FSharpSyntaxNode (parent)

    member _.InternalNode = internalNode

    override _.Range = FSharpSourceRange internalNode.Range


[<Sealed>]
type FSharpSyntaxExceptionDefnSynExceptionDefn internal (parent: FSharpSyntaxNode, internalNode: SynExceptionDefn) =
    inherit FSharpSyntaxExceptionDefn(parent, internalNode)

    let mutable __item0 : FSharpSyntaxExceptionDefnRepr = Unchecked.defaultof<_>
    let mutable __item1 : FSharpSyntaxNodeList<FSharpSyntaxMemberDefn> = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynExceptionDefn.SynExceptionDefn (item0, _, _) ->
                FSharpSyntaxExceptionDefnRepr(this, item0)
            | _ -> failwith "invalid syntax" 

    member this.item1 =
            match this.InternalNode with
            | SynExceptionDefn.SynExceptionDefn (_, item1, _) ->
                FSharpSyntaxNodeList<FSharpSyntaxMemberDefn>(this, item1)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<AbstractClass>]
type FSharpSyntaxLongIdentWithDots internal (parent: FSharpSyntaxNode, internalNode: LongIdentWithDots) =
    inherit FSharpSyntaxNode (parent)

    member _.InternalNode = internalNode

    override _.Range = FSharpSourceRange internalNode.Range


[<Sealed>]
type FSharpSyntaxLongIdentWithDotsLongIdentWithDots internal (parent: FSharpSyntaxNode, internalNode: LongIdentWithDots) =
    inherit FSharpSyntaxLongIdentWithDots(parent, internalNode)

    let mutable __item0 : FSharpSyntaxNodeList<FSharpSyntaxIdent> = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | LongIdentWithDots.LongIdentWithDots (item0, _) ->
                FSharpSyntaxNodeList<FSharpSyntaxIdent>(this, item0)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<AbstractClass>]
type FSharpSyntaxValSig internal (parent: FSharpSyntaxNode, internalNode: SynValSig) =
    inherit FSharpSyntaxNode (parent)

    member _.InternalNode = internalNode

    override _.Range = FSharpSourceRange internalNode.Range


[<Sealed>]
type FSharpSyntaxValSigValSpfn internal (parent: FSharpSyntaxNode, internalNode: SynValSig) =
    inherit FSharpSyntaxValSig(parent, internalNode)

    let mutable __item0 : FSharpSyntaxNodeList<FSharpSyntaxAttributeList> = Unchecked.defaultof<_>
    let mutable __item1 : FSharpSyntaxIdent = Unchecked.defaultof<_>
    let mutable __item2 : FSharpSyntaxValTyparDecls = Unchecked.defaultof<_>
    let mutable __item3 : FSharpSyntaxType = Unchecked.defaultof<_>
    let mutable __item4 : FSharpSyntaxValInfo = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynValSig.ValSpfn (item0, _, _, _, _, _, _, _, _, _, _) ->
                FSharpSyntaxNodeList<FSharpSyntaxAttributeList>(this, item0)
            | _ -> failwith "invalid syntax" 

    member this.item1 =
            match this.InternalNode with
            | SynValSig.ValSpfn (_, item1, _, _, _, _, _, _, _, _, _) ->
                FSharpSyntaxIdent(this, item1)
            | _ -> failwith "invalid syntax" 

    member this.item2 =
            match this.InternalNode with
            | SynValSig.ValSpfn (_, _, item2, _, _, _, _, _, _, _, _) ->
                FSharpSyntaxValTyparDecls(this, item2)
            | _ -> failwith "invalid syntax" 

    member this.item3 =
            match this.InternalNode with
            | SynValSig.ValSpfn (_, _, _, item3, _, _, _, _, _, _, _) ->
                FSharpSyntaxType(this, item3)
            | _ -> failwith "invalid syntax" 

    member this.item4 =
            match this.InternalNode with
            | SynValSig.ValSpfn (_, _, _, _, item4, _, _, _, _, _, _) ->
                FSharpSyntaxValInfo(this, item4)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<AbstractClass>]
type FSharpSyntaxTypeDefnSig internal (parent: FSharpSyntaxNode, internalNode: SynTypeDefnSig) =
    inherit FSharpSyntaxNode (parent)

    member _.InternalNode = internalNode

    override _.Range = FSharpSourceRange internalNode.Range


[<Sealed>]
type FSharpSyntaxTypeDefnSigTypeDefnSig internal (parent: FSharpSyntaxNode, internalNode: SynTypeDefnSig) =
    inherit FSharpSyntaxTypeDefnSig(parent, internalNode)

    let mutable __item0 : FSharpSyntaxComponentInfo = Unchecked.defaultof<_>
    let mutable __item1 : FSharpSyntaxTypeDefnSigRepr = Unchecked.defaultof<_>
    let mutable __item2 : FSharpSyntaxNodeList<FSharpSyntaxMemberSig> = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynTypeDefnSig.TypeDefnSig (item0, _, _, _) ->
                FSharpSyntaxComponentInfo(this, item0)
            | _ -> failwith "invalid syntax" 

    member this.item1 =
            match this.InternalNode with
            | SynTypeDefnSig.TypeDefnSig (_, item1, _, _) ->
                FSharpSyntaxTypeDefnSigRepr(this, item1)
            | _ -> failwith "invalid syntax" 

    member this.item2 =
            match this.InternalNode with
            | SynTypeDefnSig.TypeDefnSig (_, _, item2, _) ->
                FSharpSyntaxNodeList<FSharpSyntaxMemberSig>(this, item2)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<AbstractClass>]
type FSharpSyntaxExceptionSig internal (parent: FSharpSyntaxNode, internalNode: SynExceptionSig) =
    inherit FSharpSyntaxNode (parent)

    member _.InternalNode = internalNode

    override _.Range = FSharpSourceRange internalNode.Range


[<Sealed>]
type FSharpSyntaxExceptionSigSynExceptionSig internal (parent: FSharpSyntaxNode, internalNode: SynExceptionSig) =
    inherit FSharpSyntaxExceptionSig(parent, internalNode)

    let mutable __item0 : FSharpSyntaxExceptionDefnRepr = Unchecked.defaultof<_>
    let mutable __item1 : FSharpSyntaxNodeList<FSharpSyntaxMemberSig> = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynExceptionSig.SynExceptionSig (item0, _, _) ->
                FSharpSyntaxExceptionDefnRepr(this, item0)
            | _ -> failwith "invalid syntax" 

    member this.item1 =
            match this.InternalNode with
            | SynExceptionSig.SynExceptionSig (_, item1, _) ->
                FSharpSyntaxNodeList<FSharpSyntaxMemberSig>(this, item1)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<AbstractClass>]
type FSharpSyntaxTyparDecl internal (parent: FSharpSyntaxNode, internalNode: SynTyparDecl) =
    inherit FSharpSyntaxNode (parent)

    member _.InternalNode = internalNode

    override _.Range = FSharpSourceRange internalNode.Range


[<Sealed>]
type FSharpSyntaxTyparDeclTyparDecl internal (parent: FSharpSyntaxNode, internalNode: SynTyparDecl) =
    inherit FSharpSyntaxTyparDecl(parent, internalNode)

    let mutable __item0 : FSharpSyntaxNodeList<FSharpSyntaxAttributeList> = Unchecked.defaultof<_>
    let mutable __item1 : FSharpSyntaxTypar = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynTyparDecl.TyparDecl (item0, _) ->
                FSharpSyntaxNodeList<FSharpSyntaxAttributeList>(this, item0)
            | _ -> failwith "invalid syntax" 

    member this.item1 =
            match this.InternalNode with
            | SynTyparDecl.TyparDecl (_, item1) ->
                FSharpSyntaxTypar(this, item1)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<AbstractClass>]
type FSharpSyntaxTypeConstraint internal (parent: FSharpSyntaxNode, internalNode: SynTypeConstraint) =
    inherit FSharpSyntaxNode (parent)

    member _.InternalNode = internalNode

    override _.Range = FSharpSourceRange internalNode.Range


[<Sealed>]
type FSharpSyntaxTypeConstraintWhereTyparIsValueType internal (parent: FSharpSyntaxNode, internalNode: SynTypeConstraint) =
    inherit FSharpSyntaxTypeConstraint(parent, internalNode)

    let mutable __item0 : FSharpSyntaxTypar = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynTypeConstraint.WhereTyparIsValueType (item0, _) ->
                FSharpSyntaxTypar(this, item0)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxTypeConstraintWhereTyparIsReferenceType internal (parent: FSharpSyntaxNode, internalNode: SynTypeConstraint) =
    inherit FSharpSyntaxTypeConstraint(parent, internalNode)

    let mutable __item0 : FSharpSyntaxTypar = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynTypeConstraint.WhereTyparIsReferenceType (item0, _) ->
                FSharpSyntaxTypar(this, item0)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxTypeConstraintWhereTyparIsUnmanaged internal (parent: FSharpSyntaxNode, internalNode: SynTypeConstraint) =
    inherit FSharpSyntaxTypeConstraint(parent, internalNode)

    let mutable __item0 : FSharpSyntaxTypar = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynTypeConstraint.WhereTyparIsUnmanaged (item0, _) ->
                FSharpSyntaxTypar(this, item0)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxTypeConstraintWhereTyparSupportsNull internal (parent: FSharpSyntaxNode, internalNode: SynTypeConstraint) =
    inherit FSharpSyntaxTypeConstraint(parent, internalNode)

    let mutable __item0 : FSharpSyntaxTypar = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynTypeConstraint.WhereTyparSupportsNull (item0, _) ->
                FSharpSyntaxTypar(this, item0)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxTypeConstraintWhereTyparIsComparable internal (parent: FSharpSyntaxNode, internalNode: SynTypeConstraint) =
    inherit FSharpSyntaxTypeConstraint(parent, internalNode)

    let mutable __item0 : FSharpSyntaxTypar = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynTypeConstraint.WhereTyparIsComparable (item0, _) ->
                FSharpSyntaxTypar(this, item0)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxTypeConstraintWhereTyparIsEquatable internal (parent: FSharpSyntaxNode, internalNode: SynTypeConstraint) =
    inherit FSharpSyntaxTypeConstraint(parent, internalNode)

    let mutable __item0 : FSharpSyntaxTypar = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynTypeConstraint.WhereTyparIsEquatable (item0, _) ->
                FSharpSyntaxTypar(this, item0)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxTypeConstraintWhereTyparDefaultsToType internal (parent: FSharpSyntaxNode, internalNode: SynTypeConstraint) =
    inherit FSharpSyntaxTypeConstraint(parent, internalNode)

    let mutable __item0 : FSharpSyntaxTypar = Unchecked.defaultof<_>
    let mutable __item1 : FSharpSyntaxType = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynTypeConstraint.WhereTyparDefaultsToType (item0, _, _) ->
                FSharpSyntaxTypar(this, item0)
            | _ -> failwith "invalid syntax" 

    member this.item1 =
            match this.InternalNode with
            | SynTypeConstraint.WhereTyparDefaultsToType (_, item1, _) ->
                FSharpSyntaxType(this, item1)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxTypeConstraintWhereTyparSubtypeOfType internal (parent: FSharpSyntaxNode, internalNode: SynTypeConstraint) =
    inherit FSharpSyntaxTypeConstraint(parent, internalNode)

    let mutable __item0 : FSharpSyntaxTypar = Unchecked.defaultof<_>
    let mutable __item1 : FSharpSyntaxType = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynTypeConstraint.WhereTyparSubtypeOfType (item0, _, _) ->
                FSharpSyntaxTypar(this, item0)
            | _ -> failwith "invalid syntax" 

    member this.item1 =
            match this.InternalNode with
            | SynTypeConstraint.WhereTyparSubtypeOfType (_, item1, _) ->
                FSharpSyntaxType(this, item1)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxTypeConstraintWhereTyparSupportsMember internal (parent: FSharpSyntaxNode, internalNode: SynTypeConstraint) =
    inherit FSharpSyntaxTypeConstraint(parent, internalNode)

    let mutable __item0 : FSharpSyntaxNodeList<FSharpSyntaxType> = Unchecked.defaultof<_>
    let mutable __item1 : FSharpSyntaxMemberSig = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynTypeConstraint.WhereTyparSupportsMember (item0, _, _) ->
                FSharpSyntaxNodeList<FSharpSyntaxType>(this, item0)
            | _ -> failwith "invalid syntax" 

    member this.item1 =
            match this.InternalNode with
            | SynTypeConstraint.WhereTyparSupportsMember (_, item1, _) ->
                FSharpSyntaxMemberSig(this, item1)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxTypeConstraintWhereTyparIsEnum internal (parent: FSharpSyntaxNode, internalNode: SynTypeConstraint) =
    inherit FSharpSyntaxTypeConstraint(parent, internalNode)

    let mutable __item0 : FSharpSyntaxTypar = Unchecked.defaultof<_>
    let mutable __item1 : FSharpSyntaxNodeList<FSharpSyntaxType> = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynTypeConstraint.WhereTyparIsEnum (item0, _, _) ->
                FSharpSyntaxTypar(this, item0)
            | _ -> failwith "invalid syntax" 

    member this.item1 =
            match this.InternalNode with
            | SynTypeConstraint.WhereTyparIsEnum (_, item1, _) ->
                FSharpSyntaxNodeList<FSharpSyntaxType>(this, item1)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxTypeConstraintWhereTyparIsDelegate internal (parent: FSharpSyntaxNode, internalNode: SynTypeConstraint) =
    inherit FSharpSyntaxTypeConstraint(parent, internalNode)

    let mutable __item0 : FSharpSyntaxTypar = Unchecked.defaultof<_>
    let mutable __item1 : FSharpSyntaxNodeList<FSharpSyntaxType> = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynTypeConstraint.WhereTyparIsDelegate (item0, _, _) ->
                FSharpSyntaxTypar(this, item0)
            | _ -> failwith "invalid syntax" 

    member this.item1 =
            match this.InternalNode with
            | SynTypeConstraint.WhereTyparIsDelegate (_, item1, _) ->
                FSharpSyntaxNodeList<FSharpSyntaxType>(this, item1)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<AbstractClass>]
type FSharpSyntaxBindingKind internal (parent: FSharpSyntaxNode, internalNode: SynBindingKind) =
    inherit FSharpSyntaxNode (parent)

    member _.InternalNode = internalNode

    override _.Range = FSharpSourceRange internalNode.Range


[<Sealed>]
type FSharpSyntaxBindingKindStandaloneExpression internal (parent: FSharpSyntaxNode, internalNode: SynBindingKind) =
    inherit FSharpSyntaxBindingKind(parent, internalNode)


[<Sealed>]
type FSharpSyntaxBindingKindNormalBinding internal (parent: FSharpSyntaxNode, internalNode: SynBindingKind) =
    inherit FSharpSyntaxBindingKind(parent, internalNode)


[<Sealed>]
type FSharpSyntaxBindingKindDoBinding internal (parent: FSharpSyntaxNode, internalNode: SynBindingKind) =
    inherit FSharpSyntaxBindingKind(parent, internalNode)


[<AbstractClass>]
type FSharpSyntaxValData internal (parent: FSharpSyntaxNode, internalNode: SynValData) =
    inherit FSharpSyntaxNode (parent)

    member _.InternalNode = internalNode

    override _.Range = FSharpSourceRange internalNode.Range


[<Sealed>]
type FSharpSyntaxValDataSynValData internal (parent: FSharpSyntaxNode, internalNode: SynValData) =
    inherit FSharpSyntaxValData(parent, internalNode)

    let mutable __item1 : FSharpSyntaxValInfo = Unchecked.defaultof<_>

    member this.item1 =
            match this.InternalNode with
            | SynValData.SynValData (_, item1, _) ->
                FSharpSyntaxValInfo(this, item1)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<AbstractClass>]
type FSharpSyntaxPat internal (parent: FSharpSyntaxNode, internalNode: SynPat) =
    inherit FSharpSyntaxNode (parent)

    member _.InternalNode = internalNode

    override _.Range = FSharpSourceRange internalNode.Range


[<Sealed>]
type FSharpSyntaxPatConst internal (parent: FSharpSyntaxNode, internalNode: SynPat) =
    inherit FSharpSyntaxPat(parent, internalNode)

    let mutable __item0 : FSharpSyntaxConst = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynPat.Const (item0, _) ->
                FSharpSyntaxConst(this, item0)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxPatWild internal (parent: FSharpSyntaxNode, internalNode: SynPat) =
    inherit FSharpSyntaxPat(parent, internalNode)






    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxPatNamed internal (parent: FSharpSyntaxNode, internalNode: SynPat) =
    inherit FSharpSyntaxPat(parent, internalNode)

    let mutable __item0 : FSharpSyntaxPat = Unchecked.defaultof<_>
    let mutable __item1 : FSharpSyntaxIdent = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynPat.Named (item0, _, _, _, _) ->
                FSharpSyntaxPat(this, item0)
            | _ -> failwith "invalid syntax" 

    member this.item1 =
            match this.InternalNode with
            | SynPat.Named (_, item1, _, _, _) ->
                FSharpSyntaxIdent(this, item1)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxPatTyped internal (parent: FSharpSyntaxNode, internalNode: SynPat) =
    inherit FSharpSyntaxPat(parent, internalNode)

    let mutable __item0 : FSharpSyntaxPat = Unchecked.defaultof<_>
    let mutable __item1 : FSharpSyntaxType = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynPat.Typed (item0, _, _) ->
                FSharpSyntaxPat(this, item0)
            | _ -> failwith "invalid syntax" 

    member this.item1 =
            match this.InternalNode with
            | SynPat.Typed (_, item1, _) ->
                FSharpSyntaxType(this, item1)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxPatAttrib internal (parent: FSharpSyntaxNode, internalNode: SynPat) =
    inherit FSharpSyntaxPat(parent, internalNode)

    let mutable __item0 : FSharpSyntaxPat = Unchecked.defaultof<_>
    let mutable __item1 : FSharpSyntaxNodeList<FSharpSyntaxAttributeList> = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynPat.Attrib (item0, _, _) ->
                FSharpSyntaxPat(this, item0)
            | _ -> failwith "invalid syntax" 

    member this.item1 =
            match this.InternalNode with
            | SynPat.Attrib (_, item1, _) ->
                FSharpSyntaxNodeList<FSharpSyntaxAttributeList>(this, item1)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxPatOr internal (parent: FSharpSyntaxNode, internalNode: SynPat) =
    inherit FSharpSyntaxPat(parent, internalNode)

    let mutable __item0 : FSharpSyntaxPat = Unchecked.defaultof<_>
    let mutable __item1 : FSharpSyntaxPat = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynPat.Or (item0, _, _) ->
                FSharpSyntaxPat(this, item0)
            | _ -> failwith "invalid syntax" 

    member this.item1 =
            match this.InternalNode with
            | SynPat.Or (_, item1, _) ->
                FSharpSyntaxPat(this, item1)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxPatAnds internal (parent: FSharpSyntaxNode, internalNode: SynPat) =
    inherit FSharpSyntaxPat(parent, internalNode)

    let mutable __item0 : FSharpSyntaxNodeList<FSharpSyntaxPat> = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynPat.Ands (item0, _) ->
                FSharpSyntaxNodeList<FSharpSyntaxPat>(this, item0)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxPatLongIdent internal (parent: FSharpSyntaxNode, internalNode: SynPat) =
    inherit FSharpSyntaxPat(parent, internalNode)

    let mutable __item0 : FSharpSyntaxLongIdentWithDots = Unchecked.defaultof<_>
    let mutable __item3 : FSharpSyntaxArgPats = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynPat.LongIdent (item0, _, _, _, _, _) ->
                FSharpSyntaxLongIdentWithDots(this, item0)
            | _ -> failwith "invalid syntax" 

    member this.item3 =
            match this.InternalNode with
            | SynPat.LongIdent (_, _, _, item3, _, _) ->
                FSharpSyntaxArgPats(this, item3)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxPatTuple internal (parent: FSharpSyntaxNode, internalNode: SynPat) =
    inherit FSharpSyntaxPat(parent, internalNode)

    let mutable __item1 : FSharpSyntaxNodeList<FSharpSyntaxPat> = Unchecked.defaultof<_>

    member this.item1 =
            match this.InternalNode with
            | SynPat.Tuple (_, item1, _) ->
                FSharpSyntaxNodeList<FSharpSyntaxPat>(this, item1)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxPatParen internal (parent: FSharpSyntaxNode, internalNode: SynPat) =
    inherit FSharpSyntaxPat(parent, internalNode)

    let mutable __item0 : FSharpSyntaxPat = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynPat.Paren (item0, _) ->
                FSharpSyntaxPat(this, item0)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxPatArrayOrList internal (parent: FSharpSyntaxNode, internalNode: SynPat) =
    inherit FSharpSyntaxPat(parent, internalNode)

    let mutable __item1 : FSharpSyntaxNodeList<FSharpSyntaxPat> = Unchecked.defaultof<_>

    member this.item1 =
            match this.InternalNode with
            | SynPat.ArrayOrList (_, item1, _) ->
                FSharpSyntaxNodeList<FSharpSyntaxPat>(this, item1)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxPatRecord internal (parent: FSharpSyntaxNode, internalNode: SynPat) =
    inherit FSharpSyntaxPat(parent, internalNode)






    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxPatNull internal (parent: FSharpSyntaxNode, internalNode: SynPat) =
    inherit FSharpSyntaxPat(parent, internalNode)






    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxPatOptionalVal internal (parent: FSharpSyntaxNode, internalNode: SynPat) =
    inherit FSharpSyntaxPat(parent, internalNode)

    let mutable __item0 : FSharpSyntaxIdent = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynPat.OptionalVal (item0, _) ->
                FSharpSyntaxIdent(this, item0)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxPatIsInst internal (parent: FSharpSyntaxNode, internalNode: SynPat) =
    inherit FSharpSyntaxPat(parent, internalNode)

    let mutable __item0 : FSharpSyntaxType = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynPat.IsInst (item0, _) ->
                FSharpSyntaxType(this, item0)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxPatQuoteExpr internal (parent: FSharpSyntaxNode, internalNode: SynPat) =
    inherit FSharpSyntaxPat(parent, internalNode)

    let mutable __item0 : FSharpSyntaxExpr = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynPat.QuoteExpr (item0, _) ->
                FSharpSyntaxExpr(this, item0)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxPatDeprecatedCharRange internal (parent: FSharpSyntaxNode, internalNode: SynPat) =
    inherit FSharpSyntaxPat(parent, internalNode)






    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxPatInstanceMember internal (parent: FSharpSyntaxNode, internalNode: SynPat) =
    inherit FSharpSyntaxPat(parent, internalNode)

    let mutable __item0 : FSharpSyntaxIdent = Unchecked.defaultof<_>
    let mutable __item1 : FSharpSyntaxIdent = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynPat.InstanceMember (item0, _, _, _, _) ->
                FSharpSyntaxIdent(this, item0)
            | _ -> failwith "invalid syntax" 

    member this.item1 =
            match this.InternalNode with
            | SynPat.InstanceMember (_, item1, _, _, _) ->
                FSharpSyntaxIdent(this, item1)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxPatFromParseError internal (parent: FSharpSyntaxNode, internalNode: SynPat) =
    inherit FSharpSyntaxPat(parent, internalNode)

    let mutable __item0 : FSharpSyntaxPat = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynPat.FromParseError (item0, _) ->
                FSharpSyntaxPat(this, item0)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<AbstractClass>]
type FSharpSyntaxConst internal (parent: FSharpSyntaxNode, internalNode: SynConst) =
    inherit FSharpSyntaxNode (parent)

    member _.InternalNode = internalNode

    override _.Range = FSharpSourceRange internalNode.Range


[<Sealed>]
type FSharpSyntaxConstUnit internal (parent: FSharpSyntaxNode, internalNode: SynConst) =
    inherit FSharpSyntaxConst(parent, internalNode)


[<Sealed>]
type FSharpSyntaxConstBool internal (parent: FSharpSyntaxNode, internalNode: SynConst) =
    inherit FSharpSyntaxConst(parent, internalNode)






    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxConstSByte internal (parent: FSharpSyntaxNode, internalNode: SynConst) =
    inherit FSharpSyntaxConst(parent, internalNode)






    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxConstByte internal (parent: FSharpSyntaxNode, internalNode: SynConst) =
    inherit FSharpSyntaxConst(parent, internalNode)






    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxConstInt16 internal (parent: FSharpSyntaxNode, internalNode: SynConst) =
    inherit FSharpSyntaxConst(parent, internalNode)






    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxConstUInt16 internal (parent: FSharpSyntaxNode, internalNode: SynConst) =
    inherit FSharpSyntaxConst(parent, internalNode)






    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxConstInt32 internal (parent: FSharpSyntaxNode, internalNode: SynConst) =
    inherit FSharpSyntaxConst(parent, internalNode)






    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxConstUInt32 internal (parent: FSharpSyntaxNode, internalNode: SynConst) =
    inherit FSharpSyntaxConst(parent, internalNode)






    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxConstInt64 internal (parent: FSharpSyntaxNode, internalNode: SynConst) =
    inherit FSharpSyntaxConst(parent, internalNode)






    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxConstUInt64 internal (parent: FSharpSyntaxNode, internalNode: SynConst) =
    inherit FSharpSyntaxConst(parent, internalNode)






    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxConstIntPtr internal (parent: FSharpSyntaxNode, internalNode: SynConst) =
    inherit FSharpSyntaxConst(parent, internalNode)






    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxConstUIntPtr internal (parent: FSharpSyntaxNode, internalNode: SynConst) =
    inherit FSharpSyntaxConst(parent, internalNode)






    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxConstSingle internal (parent: FSharpSyntaxNode, internalNode: SynConst) =
    inherit FSharpSyntaxConst(parent, internalNode)






    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxConstDouble internal (parent: FSharpSyntaxNode, internalNode: SynConst) =
    inherit FSharpSyntaxConst(parent, internalNode)






    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxConstChar internal (parent: FSharpSyntaxNode, internalNode: SynConst) =
    inherit FSharpSyntaxConst(parent, internalNode)






    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxConstDecimal internal (parent: FSharpSyntaxNode, internalNode: SynConst) =
    inherit FSharpSyntaxConst(parent, internalNode)






    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxConstUserNum internal (parent: FSharpSyntaxNode, internalNode: SynConst) =
    inherit FSharpSyntaxConst(parent, internalNode)






    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxConstString internal (parent: FSharpSyntaxNode, internalNode: SynConst) =
    inherit FSharpSyntaxConst(parent, internalNode)






    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxConstBytes internal (parent: FSharpSyntaxNode, internalNode: SynConst) =
    inherit FSharpSyntaxConst(parent, internalNode)






    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxConstUInt16s internal (parent: FSharpSyntaxNode, internalNode: SynConst) =
    inherit FSharpSyntaxConst(parent, internalNode)






    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxConstMeasure internal (parent: FSharpSyntaxNode, internalNode: SynConst) =
    inherit FSharpSyntaxConst(parent, internalNode)

    let mutable __item0 : FSharpSyntaxConst = Unchecked.defaultof<_>
    let mutable __item1 : FSharpSyntaxMeasure = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynConst.Measure (item0, _) ->
                FSharpSyntaxConst(this, item0)
            | _ -> failwith "invalid syntax" 

    member this.item1 =
            match this.InternalNode with
            | SynConst.Measure (_, item1) ->
                FSharpSyntaxMeasure(this, item1)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<AbstractClass>]
type FSharpSyntaxType internal (parent: FSharpSyntaxNode, internalNode: SynType) =
    inherit FSharpSyntaxNode (parent)

    member _.InternalNode = internalNode

    override _.Range = FSharpSourceRange internalNode.Range


[<Sealed>]
type FSharpSyntaxTypeLongIdent internal (parent: FSharpSyntaxNode, internalNode: SynType) =
    inherit FSharpSyntaxType(parent, internalNode)

    let mutable __item0 : FSharpSyntaxLongIdentWithDots = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynType.LongIdent (item0) ->
                FSharpSyntaxLongIdentWithDots(this, item0)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxTypeApp internal (parent: FSharpSyntaxNode, internalNode: SynType) =
    inherit FSharpSyntaxType(parent, internalNode)

    let mutable __item0 : FSharpSyntaxType = Unchecked.defaultof<_>
    let mutable __item2 : FSharpSyntaxNodeList<FSharpSyntaxType> = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynType.App (item0, _, _, _, _, _, _) ->
                FSharpSyntaxType(this, item0)
            | _ -> failwith "invalid syntax" 

    member this.item2 =
            match this.InternalNode with
            | SynType.App (_, _, item2, _, _, _, _) ->
                FSharpSyntaxNodeList<FSharpSyntaxType>(this, item2)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxTypeLongIdentApp internal (parent: FSharpSyntaxNode, internalNode: SynType) =
    inherit FSharpSyntaxType(parent, internalNode)

    let mutable __item0 : FSharpSyntaxType = Unchecked.defaultof<_>
    let mutable __item1 : FSharpSyntaxLongIdentWithDots = Unchecked.defaultof<_>
    let mutable __item3 : FSharpSyntaxNodeList<FSharpSyntaxType> = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynType.LongIdentApp (item0, _, _, _, _, _, _) ->
                FSharpSyntaxType(this, item0)
            | _ -> failwith "invalid syntax" 

    member this.item1 =
            match this.InternalNode with
            | SynType.LongIdentApp (_, item1, _, _, _, _, _) ->
                FSharpSyntaxLongIdentWithDots(this, item1)
            | _ -> failwith "invalid syntax" 

    member this.item3 =
            match this.InternalNode with
            | SynType.LongIdentApp (_, _, _, item3, _, _, _) ->
                FSharpSyntaxNodeList<FSharpSyntaxType>(this, item3)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxTypeTuple internal (parent: FSharpSyntaxNode, internalNode: SynType) =
    inherit FSharpSyntaxType(parent, internalNode)






    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxTypeAnonRecd internal (parent: FSharpSyntaxNode, internalNode: SynType) =
    inherit FSharpSyntaxType(parent, internalNode)






    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxTypeArray internal (parent: FSharpSyntaxNode, internalNode: SynType) =
    inherit FSharpSyntaxType(parent, internalNode)

    let mutable __item1 : FSharpSyntaxType = Unchecked.defaultof<_>

    member this.item1 =
            match this.InternalNode with
            | SynType.Array (_, item1, _) ->
                FSharpSyntaxType(this, item1)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxTypeFun internal (parent: FSharpSyntaxNode, internalNode: SynType) =
    inherit FSharpSyntaxType(parent, internalNode)

    let mutable __item0 : FSharpSyntaxType = Unchecked.defaultof<_>
    let mutable __item1 : FSharpSyntaxType = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynType.Fun (item0, _, _) ->
                FSharpSyntaxType(this, item0)
            | _ -> failwith "invalid syntax" 

    member this.item1 =
            match this.InternalNode with
            | SynType.Fun (_, item1, _) ->
                FSharpSyntaxType(this, item1)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxTypeVar internal (parent: FSharpSyntaxNode, internalNode: SynType) =
    inherit FSharpSyntaxType(parent, internalNode)

    let mutable __item0 : FSharpSyntaxTypar = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynType.Var (item0, _) ->
                FSharpSyntaxTypar(this, item0)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxTypeAnon internal (parent: FSharpSyntaxNode, internalNode: SynType) =
    inherit FSharpSyntaxType(parent, internalNode)






    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxTypeWithGlobalConstraints internal (parent: FSharpSyntaxNode, internalNode: SynType) =
    inherit FSharpSyntaxType(parent, internalNode)

    let mutable __item0 : FSharpSyntaxType = Unchecked.defaultof<_>
    let mutable __item1 : FSharpSyntaxNodeList<FSharpSyntaxTypeConstraint> = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynType.WithGlobalConstraints (item0, _, _) ->
                FSharpSyntaxType(this, item0)
            | _ -> failwith "invalid syntax" 

    member this.item1 =
            match this.InternalNode with
            | SynType.WithGlobalConstraints (_, item1, _) ->
                FSharpSyntaxNodeList<FSharpSyntaxTypeConstraint>(this, item1)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxTypeHashConstraint internal (parent: FSharpSyntaxNode, internalNode: SynType) =
    inherit FSharpSyntaxType(parent, internalNode)

    let mutable __item0 : FSharpSyntaxType = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynType.HashConstraint (item0, _) ->
                FSharpSyntaxType(this, item0)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxTypeMeasureDivide internal (parent: FSharpSyntaxNode, internalNode: SynType) =
    inherit FSharpSyntaxType(parent, internalNode)

    let mutable __item0 : FSharpSyntaxType = Unchecked.defaultof<_>
    let mutable __item1 : FSharpSyntaxType = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynType.MeasureDivide (item0, _, _) ->
                FSharpSyntaxType(this, item0)
            | _ -> failwith "invalid syntax" 

    member this.item1 =
            match this.InternalNode with
            | SynType.MeasureDivide (_, item1, _) ->
                FSharpSyntaxType(this, item1)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxTypeMeasurePower internal (parent: FSharpSyntaxNode, internalNode: SynType) =
    inherit FSharpSyntaxType(parent, internalNode)

    let mutable __item0 : FSharpSyntaxType = Unchecked.defaultof<_>
    let mutable __item1 : FSharpSyntaxRationalConst = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynType.MeasurePower (item0, _, _) ->
                FSharpSyntaxType(this, item0)
            | _ -> failwith "invalid syntax" 

    member this.item1 =
            match this.InternalNode with
            | SynType.MeasurePower (_, item1, _) ->
                FSharpSyntaxRationalConst(this, item1)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxTypeStaticConstant internal (parent: FSharpSyntaxNode, internalNode: SynType) =
    inherit FSharpSyntaxType(parent, internalNode)

    let mutable __item0 : FSharpSyntaxConst = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynType.StaticConstant (item0, _) ->
                FSharpSyntaxConst(this, item0)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxTypeStaticConstantExpr internal (parent: FSharpSyntaxNode, internalNode: SynType) =
    inherit FSharpSyntaxType(parent, internalNode)

    let mutable __item0 : FSharpSyntaxExpr = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynType.StaticConstantExpr (item0, _) ->
                FSharpSyntaxExpr(this, item0)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxTypeStaticConstantNamed internal (parent: FSharpSyntaxNode, internalNode: SynType) =
    inherit FSharpSyntaxType(parent, internalNode)

    let mutable __item0 : FSharpSyntaxType = Unchecked.defaultof<_>
    let mutable __item1 : FSharpSyntaxType = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynType.StaticConstantNamed (item0, _, _) ->
                FSharpSyntaxType(this, item0)
            | _ -> failwith "invalid syntax" 

    member this.item1 =
            match this.InternalNode with
            | SynType.StaticConstantNamed (_, item1, _) ->
                FSharpSyntaxType(this, item1)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxTypeParen internal (parent: FSharpSyntaxNode, internalNode: SynType) =
    inherit FSharpSyntaxType(parent, internalNode)

    let mutable __item0 : FSharpSyntaxType = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynType.Paren (item0, _) ->
                FSharpSyntaxType(this, item0)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<AbstractClass>]
type FSharpSyntaxInterfaceImpl internal (parent: FSharpSyntaxNode, internalNode: SynInterfaceImpl) =
    inherit FSharpSyntaxNode (parent)

    member _.InternalNode = internalNode

    override _.Range = FSharpSourceRange internalNode.Range


[<Sealed>]
type FSharpSyntaxInterfaceImplInterfaceImpl internal (parent: FSharpSyntaxNode, internalNode: SynInterfaceImpl) =
    inherit FSharpSyntaxInterfaceImpl(parent, internalNode)

    let mutable __item0 : FSharpSyntaxType = Unchecked.defaultof<_>
    let mutable __item1 : FSharpSyntaxNodeList<FSharpSyntaxBinding> = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynInterfaceImpl.InterfaceImpl (item0, _, _) ->
                FSharpSyntaxType(this, item0)
            | _ -> failwith "invalid syntax" 

    member this.item1 =
            match this.InternalNode with
            | SynInterfaceImpl.InterfaceImpl (_, item1, _) ->
                FSharpSyntaxNodeList<FSharpSyntaxBinding>(this, item1)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<AbstractClass>]
type FSharpSyntaxSimplePats internal (parent: FSharpSyntaxNode, internalNode: SynSimplePats) =
    inherit FSharpSyntaxNode (parent)

    member _.InternalNode = internalNode

    override _.Range = FSharpSourceRange internalNode.Range


[<Sealed>]
type FSharpSyntaxSimplePatsSimplePats internal (parent: FSharpSyntaxNode, internalNode: SynSimplePats) =
    inherit FSharpSyntaxSimplePats(parent, internalNode)

    let mutable __item0 : FSharpSyntaxNodeList<FSharpSyntaxSimplePat> = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynSimplePats.SimplePats (item0, _) ->
                FSharpSyntaxNodeList<FSharpSyntaxSimplePat>(this, item0)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxSimplePatsTyped internal (parent: FSharpSyntaxNode, internalNode: SynSimplePats) =
    inherit FSharpSyntaxSimplePats(parent, internalNode)

    let mutable __item0 : FSharpSyntaxSimplePats = Unchecked.defaultof<_>
    let mutable __item1 : FSharpSyntaxType = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynSimplePats.Typed (item0, _, _) ->
                FSharpSyntaxSimplePats(this, item0)
            | _ -> failwith "invalid syntax" 

    member this.item1 =
            match this.InternalNode with
            | SynSimplePats.Typed (_, item1, _) ->
                FSharpSyntaxType(this, item1)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<AbstractClass>]
type FSharpSyntaxMatchClause internal (parent: FSharpSyntaxNode, internalNode: SynMatchClause) =
    inherit FSharpSyntaxNode (parent)

    member _.InternalNode = internalNode

    override _.Range = FSharpSourceRange internalNode.Range


[<Sealed>]
type FSharpSyntaxMatchClauseClause internal (parent: FSharpSyntaxNode, internalNode: SynMatchClause) =
    inherit FSharpSyntaxMatchClause(parent, internalNode)

    let mutable __item0 : FSharpSyntaxPat = Unchecked.defaultof<_>
    let mutable __item2 : FSharpSyntaxExpr = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynMatchClause.Clause (item0, _, _, _, _) ->
                FSharpSyntaxPat(this, item0)
            | _ -> failwith "invalid syntax" 

    member this.item2 =
            match this.InternalNode with
            | SynMatchClause.Clause (_, _, item2, _, _) ->
                FSharpSyntaxExpr(this, item2)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<AbstractClass>]
type FSharpSyntaxIndexerArg internal (parent: FSharpSyntaxNode, internalNode: SynIndexerArg) =
    inherit FSharpSyntaxNode (parent)

    member _.InternalNode = internalNode

    override _.Range = FSharpSourceRange internalNode.Range


[<Sealed>]
type FSharpSyntaxIndexerArgTwo internal (parent: FSharpSyntaxNode, internalNode: SynIndexerArg) =
    inherit FSharpSyntaxIndexerArg(parent, internalNode)

    let mutable __item0 : FSharpSyntaxExpr = Unchecked.defaultof<_>
    let mutable __item2 : FSharpSyntaxExpr = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynIndexerArg.Two (item0, _, _, _, _, _) ->
                FSharpSyntaxExpr(this, item0)
            | _ -> failwith "invalid syntax" 

    member this.item2 =
            match this.InternalNode with
            | SynIndexerArg.Two (_, _, item2, _, _, _) ->
                FSharpSyntaxExpr(this, item2)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxIndexerArgOne internal (parent: FSharpSyntaxNode, internalNode: SynIndexerArg) =
    inherit FSharpSyntaxIndexerArg(parent, internalNode)

    let mutable __item0 : FSharpSyntaxExpr = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynIndexerArg.One (item0, _, _) ->
                FSharpSyntaxExpr(this, item0)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<AbstractClass>]
type FSharpSyntaxTypar internal (parent: FSharpSyntaxNode, internalNode: SynTypar) =
    inherit FSharpSyntaxNode (parent)

    member _.InternalNode = internalNode

    override _.Range = FSharpSourceRange internalNode.Range


[<Sealed>]
type FSharpSyntaxTyparTypar internal (parent: FSharpSyntaxNode, internalNode: SynTypar) =
    inherit FSharpSyntaxTypar(parent, internalNode)

    let mutable __item0 : FSharpSyntaxIdent = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynTypar.Typar (item0, _, _) ->
                FSharpSyntaxIdent(this, item0)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<AbstractClass>]
type FSharpSyntaxMemberSig internal (parent: FSharpSyntaxNode, internalNode: SynMemberSig) =
    inherit FSharpSyntaxNode (parent)

    member _.InternalNode = internalNode

    override _.Range = FSharpSourceRange internalNode.Range


[<Sealed>]
type FSharpSyntaxMemberSigMember internal (parent: FSharpSyntaxNode, internalNode: SynMemberSig) =
    inherit FSharpSyntaxMemberSig(parent, internalNode)

    let mutable __item0 : FSharpSyntaxValSig = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynMemberSig.Member (item0, _, _) ->
                FSharpSyntaxValSig(this, item0)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxMemberSigInterface internal (parent: FSharpSyntaxNode, internalNode: SynMemberSig) =
    inherit FSharpSyntaxMemberSig(parent, internalNode)

    let mutable __item0 : FSharpSyntaxType = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynMemberSig.Interface (item0, _) ->
                FSharpSyntaxType(this, item0)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxMemberSigInherit internal (parent: FSharpSyntaxNode, internalNode: SynMemberSig) =
    inherit FSharpSyntaxMemberSig(parent, internalNode)

    let mutable __item0 : FSharpSyntaxType = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynMemberSig.Inherit (item0, _) ->
                FSharpSyntaxType(this, item0)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxMemberSigValField internal (parent: FSharpSyntaxNode, internalNode: SynMemberSig) =
    inherit FSharpSyntaxMemberSig(parent, internalNode)

    let mutable __item0 : FSharpSyntaxField = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynMemberSig.ValField (item0, _) ->
                FSharpSyntaxField(this, item0)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxMemberSigNestedType internal (parent: FSharpSyntaxNode, internalNode: SynMemberSig) =
    inherit FSharpSyntaxMemberSig(parent, internalNode)

    let mutable __item0 : FSharpSyntaxTypeDefnSig = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynMemberSig.NestedType (item0, _) ->
                FSharpSyntaxTypeDefnSig(this, item0)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<AbstractClass>]
type FSharpSyntaxStaticOptimizationConstraint internal (parent: FSharpSyntaxNode, internalNode: SynStaticOptimizationConstraint) =
    inherit FSharpSyntaxNode (parent)

    member _.InternalNode = internalNode

    override _.Range = FSharpSourceRange internalNode.Range


[<Sealed>]
type FSharpSyntaxStaticOptimizationConstraintWhenTyparTyconEqualsTycon internal (parent: FSharpSyntaxNode, internalNode: SynStaticOptimizationConstraint) =
    inherit FSharpSyntaxStaticOptimizationConstraint(parent, internalNode)

    let mutable __item0 : FSharpSyntaxTypar = Unchecked.defaultof<_>
    let mutable __item1 : FSharpSyntaxType = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynStaticOptimizationConstraint.WhenTyparTyconEqualsTycon (item0, _, _) ->
                FSharpSyntaxTypar(this, item0)
            | _ -> failwith "invalid syntax" 

    member this.item1 =
            match this.InternalNode with
            | SynStaticOptimizationConstraint.WhenTyparTyconEqualsTycon (_, item1, _) ->
                FSharpSyntaxType(this, item1)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxStaticOptimizationConstraintWhenTyparIsStruct internal (parent: FSharpSyntaxNode, internalNode: SynStaticOptimizationConstraint) =
    inherit FSharpSyntaxStaticOptimizationConstraint(parent, internalNode)

    let mutable __item0 : FSharpSyntaxTypar = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynStaticOptimizationConstraint.WhenTyparIsStruct (item0, _) ->
                FSharpSyntaxTypar(this, item0)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<AbstractClass>]
type FSharpSyntaxTypeDefnRepr internal (parent: FSharpSyntaxNode, internalNode: SynTypeDefnRepr) =
    inherit FSharpSyntaxNode (parent)

    member _.InternalNode = internalNode

    override _.Range = FSharpSourceRange internalNode.Range


[<Sealed>]
type FSharpSyntaxTypeDefnReprObjectModel internal (parent: FSharpSyntaxNode, internalNode: SynTypeDefnRepr) =
    inherit FSharpSyntaxTypeDefnRepr(parent, internalNode)

    let mutable __item0 : FSharpSyntaxTypeDefnKind = Unchecked.defaultof<_>
    let mutable __item1 : FSharpSyntaxNodeList<FSharpSyntaxMemberDefn> = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynTypeDefnRepr.ObjectModel (item0, _, _) ->
                FSharpSyntaxTypeDefnKind(this, item0)
            | _ -> failwith "invalid syntax" 

    member this.item1 =
            match this.InternalNode with
            | SynTypeDefnRepr.ObjectModel (_, item1, _) ->
                FSharpSyntaxNodeList<FSharpSyntaxMemberDefn>(this, item1)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxTypeDefnReprSimple internal (parent: FSharpSyntaxNode, internalNode: SynTypeDefnRepr) =
    inherit FSharpSyntaxTypeDefnRepr(parent, internalNode)

    let mutable __item0 : FSharpSyntaxTypeDefnSimpleRepr = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynTypeDefnRepr.Simple (item0, _) ->
                FSharpSyntaxTypeDefnSimpleRepr(this, item0)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxTypeDefnReprException internal (parent: FSharpSyntaxNode, internalNode: SynTypeDefnRepr) =
    inherit FSharpSyntaxTypeDefnRepr(parent, internalNode)

    let mutable __item0 : FSharpSyntaxExceptionDefnRepr = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynTypeDefnRepr.Exception (item0) ->
                FSharpSyntaxExceptionDefnRepr(this, item0)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<AbstractClass>]
type FSharpSyntaxMemberDefn internal (parent: FSharpSyntaxNode, internalNode: SynMemberDefn) =
    inherit FSharpSyntaxNode (parent)

    member _.InternalNode = internalNode

    override _.Range = FSharpSourceRange internalNode.Range


[<Sealed>]
type FSharpSyntaxMemberDefnOpen internal (parent: FSharpSyntaxNode, internalNode: SynMemberDefn) =
    inherit FSharpSyntaxMemberDefn(parent, internalNode)

    let mutable __item0 : FSharpSyntaxNodeList<FSharpSyntaxIdent> = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynMemberDefn.Open (item0, _) ->
                FSharpSyntaxNodeList<FSharpSyntaxIdent>(this, item0)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxMemberDefnMember internal (parent: FSharpSyntaxNode, internalNode: SynMemberDefn) =
    inherit FSharpSyntaxMemberDefn(parent, internalNode)

    let mutable __item0 : FSharpSyntaxBinding = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynMemberDefn.Member (item0, _) ->
                FSharpSyntaxBinding(this, item0)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxMemberDefnImplicitCtor internal (parent: FSharpSyntaxNode, internalNode: SynMemberDefn) =
    inherit FSharpSyntaxMemberDefn(parent, internalNode)

    let mutable __item1 : FSharpSyntaxNodeList<FSharpSyntaxAttributeList> = Unchecked.defaultof<_>
    let mutable __item2 : FSharpSyntaxSimplePats = Unchecked.defaultof<_>

    member this.item1 =
            match this.InternalNode with
            | SynMemberDefn.ImplicitCtor (_, item1, _, _, _) ->
                FSharpSyntaxNodeList<FSharpSyntaxAttributeList>(this, item1)
            | _ -> failwith "invalid syntax" 

    member this.item2 =
            match this.InternalNode with
            | SynMemberDefn.ImplicitCtor (_, _, item2, _, _) ->
                FSharpSyntaxSimplePats(this, item2)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxMemberDefnImplicitInherit internal (parent: FSharpSyntaxNode, internalNode: SynMemberDefn) =
    inherit FSharpSyntaxMemberDefn(parent, internalNode)

    let mutable __item0 : FSharpSyntaxType = Unchecked.defaultof<_>
    let mutable __item1 : FSharpSyntaxExpr = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynMemberDefn.ImplicitInherit (item0, _, _, _) ->
                FSharpSyntaxType(this, item0)
            | _ -> failwith "invalid syntax" 

    member this.item1 =
            match this.InternalNode with
            | SynMemberDefn.ImplicitInherit (_, item1, _, _) ->
                FSharpSyntaxExpr(this, item1)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxMemberDefnLetBindings internal (parent: FSharpSyntaxNode, internalNode: SynMemberDefn) =
    inherit FSharpSyntaxMemberDefn(parent, internalNode)

    let mutable __item0 : FSharpSyntaxNodeList<FSharpSyntaxBinding> = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynMemberDefn.LetBindings (item0, _, _, _) ->
                FSharpSyntaxNodeList<FSharpSyntaxBinding>(this, item0)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxMemberDefnAbstractSlot internal (parent: FSharpSyntaxNode, internalNode: SynMemberDefn) =
    inherit FSharpSyntaxMemberDefn(parent, internalNode)

    let mutable __item0 : FSharpSyntaxValSig = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynMemberDefn.AbstractSlot (item0, _, _) ->
                FSharpSyntaxValSig(this, item0)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxMemberDefnInterface internal (parent: FSharpSyntaxNode, internalNode: SynMemberDefn) =
    inherit FSharpSyntaxMemberDefn(parent, internalNode)

    let mutable __item0 : FSharpSyntaxType = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynMemberDefn.Interface (item0, _, _) ->
                FSharpSyntaxType(this, item0)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxMemberDefnInherit internal (parent: FSharpSyntaxNode, internalNode: SynMemberDefn) =
    inherit FSharpSyntaxMemberDefn(parent, internalNode)

    let mutable __item0 : FSharpSyntaxType = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynMemberDefn.Inherit (item0, _, _) ->
                FSharpSyntaxType(this, item0)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxMemberDefnValField internal (parent: FSharpSyntaxNode, internalNode: SynMemberDefn) =
    inherit FSharpSyntaxMemberDefn(parent, internalNode)

    let mutable __item0 : FSharpSyntaxField = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynMemberDefn.ValField (item0, _) ->
                FSharpSyntaxField(this, item0)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxMemberDefnNestedType internal (parent: FSharpSyntaxNode, internalNode: SynMemberDefn) =
    inherit FSharpSyntaxMemberDefn(parent, internalNode)

    let mutable __item0 : FSharpSyntaxTypeDefn = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynMemberDefn.NestedType (item0, _, _) ->
                FSharpSyntaxTypeDefn(this, item0)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxMemberDefnAutoProperty internal (parent: FSharpSyntaxNode, internalNode: SynMemberDefn) =
    inherit FSharpSyntaxMemberDefn(parent, internalNode)

    let mutable __item0 : FSharpSyntaxNodeList<FSharpSyntaxAttributeList> = Unchecked.defaultof<_>
    let mutable __item2 : FSharpSyntaxIdent = Unchecked.defaultof<_>
    let mutable __item8 : FSharpSyntaxExpr = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynMemberDefn.AutoProperty (item0, _, _, _, _, _, _, _, _, _, _) ->
                FSharpSyntaxNodeList<FSharpSyntaxAttributeList>(this, item0)
            | _ -> failwith "invalid syntax" 

    member this.item2 =
            match this.InternalNode with
            | SynMemberDefn.AutoProperty (_, _, item2, _, _, _, _, _, _, _, _) ->
                FSharpSyntaxIdent(this, item2)
            | _ -> failwith "invalid syntax" 

    member this.item8 =
            match this.InternalNode with
            | SynMemberDefn.AutoProperty (_, _, _, _, _, _, _, _, item8, _, _) ->
                FSharpSyntaxExpr(this, item8)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<AbstractClass>]
type FSharpSyntaxExceptionDefnRepr internal (parent: FSharpSyntaxNode, internalNode: SynExceptionDefnRepr) =
    inherit FSharpSyntaxNode (parent)

    member _.InternalNode = internalNode

    override _.Range = FSharpSourceRange internalNode.Range


[<Sealed>]
type FSharpSyntaxExceptionDefnReprSynExceptionDefnRepr internal (parent: FSharpSyntaxNode, internalNode: SynExceptionDefnRepr) =
    inherit FSharpSyntaxExceptionDefnRepr(parent, internalNode)

    let mutable __item0 : FSharpSyntaxNodeList<FSharpSyntaxAttributeList> = Unchecked.defaultof<_>
    let mutable __item1 : FSharpSyntaxUnionCase = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynExceptionDefnRepr.SynExceptionDefnRepr (item0, _, _, _, _, _) ->
                FSharpSyntaxNodeList<FSharpSyntaxAttributeList>(this, item0)
            | _ -> failwith "invalid syntax" 

    member this.item1 =
            match this.InternalNode with
            | SynExceptionDefnRepr.SynExceptionDefnRepr (_, item1, _, _, _, _) ->
                FSharpSyntaxUnionCase(this, item1)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<AbstractClass>]
type FSharpSyntaxValTyparDecls internal (parent: FSharpSyntaxNode, internalNode: SynValTyparDecls) =
    inherit FSharpSyntaxNode (parent)

    member _.InternalNode = internalNode

    override _.Range = FSharpSourceRange internalNode.Range


[<Sealed>]
type FSharpSyntaxValTyparDeclsSynValTyparDecls internal (parent: FSharpSyntaxNode, internalNode: SynValTyparDecls) =
    inherit FSharpSyntaxValTyparDecls(parent, internalNode)

    let mutable __item0 : FSharpSyntaxNodeList<FSharpSyntaxTyparDecl> = Unchecked.defaultof<_>
    let mutable __item2 : FSharpSyntaxNodeList<FSharpSyntaxTypeConstraint> = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynValTyparDecls.SynValTyparDecls (item0, _, _) ->
                FSharpSyntaxNodeList<FSharpSyntaxTyparDecl>(this, item0)
            | _ -> failwith "invalid syntax" 

    member this.item2 =
            match this.InternalNode with
            | SynValTyparDecls.SynValTyparDecls (_, _, item2) ->
                FSharpSyntaxNodeList<FSharpSyntaxTypeConstraint>(this, item2)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<AbstractClass>]
type FSharpSyntaxValInfo internal (parent: FSharpSyntaxNode, internalNode: SynValInfo) =
    inherit FSharpSyntaxNode (parent)

    member _.InternalNode = internalNode

    override _.Range = FSharpSourceRange internalNode.Range


[<Sealed>]
type FSharpSyntaxValInfoSynValInfo internal (parent: FSharpSyntaxNode, internalNode: SynValInfo) =
    inherit FSharpSyntaxValInfo(parent, internalNode)

    let mutable __item1 : FSharpSyntaxArgInfo = Unchecked.defaultof<_>

    member this.item1 =
            match this.InternalNode with
            | SynValInfo.SynValInfo (_, item1) ->
                FSharpSyntaxArgInfo(this, item1)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<AbstractClass>]
type FSharpSyntaxTypeDefnSigRepr internal (parent: FSharpSyntaxNode, internalNode: SynTypeDefnSigRepr) =
    inherit FSharpSyntaxNode (parent)

    member _.InternalNode = internalNode

    override _.Range = FSharpSourceRange internalNode.Range


[<Sealed>]
type FSharpSyntaxTypeDefnSigReprObjectModel internal (parent: FSharpSyntaxNode, internalNode: SynTypeDefnSigRepr) =
    inherit FSharpSyntaxTypeDefnSigRepr(parent, internalNode)

    let mutable __item0 : FSharpSyntaxTypeDefnKind = Unchecked.defaultof<_>
    let mutable __item1 : FSharpSyntaxNodeList<FSharpSyntaxMemberSig> = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynTypeDefnSigRepr.ObjectModel (item0, _, _) ->
                FSharpSyntaxTypeDefnKind(this, item0)
            | _ -> failwith "invalid syntax" 

    member this.item1 =
            match this.InternalNode with
            | SynTypeDefnSigRepr.ObjectModel (_, item1, _) ->
                FSharpSyntaxNodeList<FSharpSyntaxMemberSig>(this, item1)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxTypeDefnSigReprSimple internal (parent: FSharpSyntaxNode, internalNode: SynTypeDefnSigRepr) =
    inherit FSharpSyntaxTypeDefnSigRepr(parent, internalNode)

    let mutable __item0 : FSharpSyntaxTypeDefnSimpleRepr = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynTypeDefnSigRepr.Simple (item0, _) ->
                FSharpSyntaxTypeDefnSimpleRepr(this, item0)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxTypeDefnSigReprException internal (parent: FSharpSyntaxNode, internalNode: SynTypeDefnSigRepr) =
    inherit FSharpSyntaxTypeDefnSigRepr(parent, internalNode)

    let mutable __item0 : FSharpSyntaxExceptionDefnRepr = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynTypeDefnSigRepr.Exception (item0) ->
                FSharpSyntaxExceptionDefnRepr(this, item0)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<AbstractClass>]
type FSharpSyntaxArgPats internal (parent: FSharpSyntaxNode, internalNode: SynArgPats) =
    inherit FSharpSyntaxNode (parent)

    member _.InternalNode = internalNode

    override _.Range = FSharpSourceRange internalNode.Range


[<Sealed>]
type FSharpSyntaxArgPatsPats internal (parent: FSharpSyntaxNode, internalNode: SynArgPats) =
    inherit FSharpSyntaxArgPats(parent, internalNode)

    let mutable __item0 : FSharpSyntaxNodeList<FSharpSyntaxPat> = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynArgPats.Pats (item0) ->
                FSharpSyntaxNodeList<FSharpSyntaxPat>(this, item0)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxArgPatsNamePatPairs internal (parent: FSharpSyntaxNode, internalNode: SynArgPats) =
    inherit FSharpSyntaxArgPats(parent, internalNode)






    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<AbstractClass>]
type FSharpSyntaxMeasure internal (parent: FSharpSyntaxNode, internalNode: SynMeasure) =
    inherit FSharpSyntaxNode (parent)

    member _.InternalNode = internalNode

    override _.Range = FSharpSourceRange internalNode.Range


[<Sealed>]
type FSharpSyntaxMeasureNamed internal (parent: FSharpSyntaxNode, internalNode: SynMeasure) =
    inherit FSharpSyntaxMeasure(parent, internalNode)

    let mutable __item0 : FSharpSyntaxNodeList<FSharpSyntaxIdent> = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynMeasure.Named (item0, _) ->
                FSharpSyntaxNodeList<FSharpSyntaxIdent>(this, item0)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxMeasureProduct internal (parent: FSharpSyntaxNode, internalNode: SynMeasure) =
    inherit FSharpSyntaxMeasure(parent, internalNode)

    let mutable __item0 : FSharpSyntaxMeasure = Unchecked.defaultof<_>
    let mutable __item1 : FSharpSyntaxMeasure = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynMeasure.Product (item0, _, _) ->
                FSharpSyntaxMeasure(this, item0)
            | _ -> failwith "invalid syntax" 

    member this.item1 =
            match this.InternalNode with
            | SynMeasure.Product (_, item1, _) ->
                FSharpSyntaxMeasure(this, item1)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxMeasureSeq internal (parent: FSharpSyntaxNode, internalNode: SynMeasure) =
    inherit FSharpSyntaxMeasure(parent, internalNode)

    let mutable __item0 : FSharpSyntaxNodeList<FSharpSyntaxMeasure> = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynMeasure.Seq (item0, _) ->
                FSharpSyntaxNodeList<FSharpSyntaxMeasure>(this, item0)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxMeasureDivide internal (parent: FSharpSyntaxNode, internalNode: SynMeasure) =
    inherit FSharpSyntaxMeasure(parent, internalNode)

    let mutable __item0 : FSharpSyntaxMeasure = Unchecked.defaultof<_>
    let mutable __item1 : FSharpSyntaxMeasure = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynMeasure.Divide (item0, _, _) ->
                FSharpSyntaxMeasure(this, item0)
            | _ -> failwith "invalid syntax" 

    member this.item1 =
            match this.InternalNode with
            | SynMeasure.Divide (_, item1, _) ->
                FSharpSyntaxMeasure(this, item1)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxMeasurePower internal (parent: FSharpSyntaxNode, internalNode: SynMeasure) =
    inherit FSharpSyntaxMeasure(parent, internalNode)

    let mutable __item0 : FSharpSyntaxMeasure = Unchecked.defaultof<_>
    let mutable __item1 : FSharpSyntaxRationalConst = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynMeasure.Power (item0, _, _) ->
                FSharpSyntaxMeasure(this, item0)
            | _ -> failwith "invalid syntax" 

    member this.item1 =
            match this.InternalNode with
            | SynMeasure.Power (_, item1, _) ->
                FSharpSyntaxRationalConst(this, item1)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxMeasureOne internal (parent: FSharpSyntaxNode, internalNode: SynMeasure) =
    inherit FSharpSyntaxMeasure(parent, internalNode)


[<Sealed>]
type FSharpSyntaxMeasureAnon internal (parent: FSharpSyntaxNode, internalNode: SynMeasure) =
    inherit FSharpSyntaxMeasure(parent, internalNode)






    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxMeasureVar internal (parent: FSharpSyntaxNode, internalNode: SynMeasure) =
    inherit FSharpSyntaxMeasure(parent, internalNode)

    let mutable __item0 : FSharpSyntaxTypar = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynMeasure.Var (item0, _) ->
                FSharpSyntaxTypar(this, item0)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<AbstractClass>]
type FSharpSyntaxRationalConst internal (parent: FSharpSyntaxNode, internalNode: SynRationalConst) =
    inherit FSharpSyntaxNode (parent)

    member _.InternalNode = internalNode

    override _.Range = FSharpSourceRange internalNode.Range


[<Sealed>]
type FSharpSyntaxRationalConstInteger internal (parent: FSharpSyntaxNode, internalNode: SynRationalConst) =
    inherit FSharpSyntaxRationalConst(parent, internalNode)






    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxRationalConstRational internal (parent: FSharpSyntaxNode, internalNode: SynRationalConst) =
    inherit FSharpSyntaxRationalConst(parent, internalNode)






    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxRationalConstNegate internal (parent: FSharpSyntaxNode, internalNode: SynRationalConst) =
    inherit FSharpSyntaxRationalConst(parent, internalNode)

    let mutable __item0 : FSharpSyntaxRationalConst = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynRationalConst.Negate (item0) ->
                FSharpSyntaxRationalConst(this, item0)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<AbstractClass>]
type FSharpSyntaxSimplePat internal (parent: FSharpSyntaxNode, internalNode: SynSimplePat) =
    inherit FSharpSyntaxNode (parent)

    member _.InternalNode = internalNode

    override _.Range = FSharpSourceRange internalNode.Range


[<Sealed>]
type FSharpSyntaxSimplePatId internal (parent: FSharpSyntaxNode, internalNode: SynSimplePat) =
    inherit FSharpSyntaxSimplePat(parent, internalNode)

    let mutable __item0 : FSharpSyntaxIdent = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynSimplePat.Id (item0, _, _, _, _, _) ->
                FSharpSyntaxIdent(this, item0)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxSimplePatTyped internal (parent: FSharpSyntaxNode, internalNode: SynSimplePat) =
    inherit FSharpSyntaxSimplePat(parent, internalNode)

    let mutable __item0 : FSharpSyntaxSimplePat = Unchecked.defaultof<_>
    let mutable __item1 : FSharpSyntaxType = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynSimplePat.Typed (item0, _, _) ->
                FSharpSyntaxSimplePat(this, item0)
            | _ -> failwith "invalid syntax" 

    member this.item1 =
            match this.InternalNode with
            | SynSimplePat.Typed (_, item1, _) ->
                FSharpSyntaxType(this, item1)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxSimplePatAttrib internal (parent: FSharpSyntaxNode, internalNode: SynSimplePat) =
    inherit FSharpSyntaxSimplePat(parent, internalNode)

    let mutable __item0 : FSharpSyntaxSimplePat = Unchecked.defaultof<_>
    let mutable __item1 : FSharpSyntaxNodeList<FSharpSyntaxAttributeList> = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynSimplePat.Attrib (item0, _, _) ->
                FSharpSyntaxSimplePat(this, item0)
            | _ -> failwith "invalid syntax" 

    member this.item1 =
            match this.InternalNode with
            | SynSimplePat.Attrib (_, item1, _) ->
                FSharpSyntaxNodeList<FSharpSyntaxAttributeList>(this, item1)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<AbstractClass>]
type FSharpSyntaxField internal (parent: FSharpSyntaxNode, internalNode: SynField) =
    inherit FSharpSyntaxNode (parent)

    member _.InternalNode = internalNode

    override _.Range = FSharpSourceRange internalNode.Range


[<Sealed>]
type FSharpSyntaxFieldField internal (parent: FSharpSyntaxNode, internalNode: SynField) =
    inherit FSharpSyntaxField(parent, internalNode)

    let mutable __item0 : FSharpSyntaxNodeList<FSharpSyntaxAttributeList> = Unchecked.defaultof<_>
    let mutable __item3 : FSharpSyntaxType = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynField.Field (item0, _, _, _, _, _, _, _) ->
                FSharpSyntaxNodeList<FSharpSyntaxAttributeList>(this, item0)
            | _ -> failwith "invalid syntax" 

    member this.item3 =
            match this.InternalNode with
            | SynField.Field (_, _, _, item3, _, _, _, _) ->
                FSharpSyntaxType(this, item3)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<AbstractClass>]
type FSharpSyntaxTypeDefnKind internal (parent: FSharpSyntaxNode, internalNode: SynTypeDefnKind) =
    inherit FSharpSyntaxNode (parent)

    member _.InternalNode = internalNode

    override _.Range = FSharpSourceRange internalNode.Range


[<Sealed>]
type FSharpSyntaxTypeDefnKindTyconUnspecified internal (parent: FSharpSyntaxNode, internalNode: SynTypeDefnKind) =
    inherit FSharpSyntaxTypeDefnKind(parent, internalNode)


[<Sealed>]
type FSharpSyntaxTypeDefnKindTyconClass internal (parent: FSharpSyntaxNode, internalNode: SynTypeDefnKind) =
    inherit FSharpSyntaxTypeDefnKind(parent, internalNode)


[<Sealed>]
type FSharpSyntaxTypeDefnKindTyconInterface internal (parent: FSharpSyntaxNode, internalNode: SynTypeDefnKind) =
    inherit FSharpSyntaxTypeDefnKind(parent, internalNode)


[<Sealed>]
type FSharpSyntaxTypeDefnKindTyconStruct internal (parent: FSharpSyntaxNode, internalNode: SynTypeDefnKind) =
    inherit FSharpSyntaxTypeDefnKind(parent, internalNode)


[<Sealed>]
type FSharpSyntaxTypeDefnKindTyconRecord internal (parent: FSharpSyntaxNode, internalNode: SynTypeDefnKind) =
    inherit FSharpSyntaxTypeDefnKind(parent, internalNode)


[<Sealed>]
type FSharpSyntaxTypeDefnKindTyconUnion internal (parent: FSharpSyntaxNode, internalNode: SynTypeDefnKind) =
    inherit FSharpSyntaxTypeDefnKind(parent, internalNode)


[<Sealed>]
type FSharpSyntaxTypeDefnKindTyconAbbrev internal (parent: FSharpSyntaxNode, internalNode: SynTypeDefnKind) =
    inherit FSharpSyntaxTypeDefnKind(parent, internalNode)


[<Sealed>]
type FSharpSyntaxTypeDefnKindTyconHiddenRepr internal (parent: FSharpSyntaxNode, internalNode: SynTypeDefnKind) =
    inherit FSharpSyntaxTypeDefnKind(parent, internalNode)


[<Sealed>]
type FSharpSyntaxTypeDefnKindTyconAugmentation internal (parent: FSharpSyntaxNode, internalNode: SynTypeDefnKind) =
    inherit FSharpSyntaxTypeDefnKind(parent, internalNode)


[<Sealed>]
type FSharpSyntaxTypeDefnKindTyconILAssemblyCode internal (parent: FSharpSyntaxNode, internalNode: SynTypeDefnKind) =
    inherit FSharpSyntaxTypeDefnKind(parent, internalNode)


[<Sealed>]
type FSharpSyntaxTypeDefnKindTyconDelegate internal (parent: FSharpSyntaxNode, internalNode: SynTypeDefnKind) =
    inherit FSharpSyntaxTypeDefnKind(parent, internalNode)

    let mutable __item0 : FSharpSyntaxType = Unchecked.defaultof<_>
    let mutable __item1 : FSharpSyntaxValInfo = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynTypeDefnKind.TyconDelegate (item0, _) ->
                FSharpSyntaxType(this, item0)
            | _ -> failwith "invalid syntax" 

    member this.item1 =
            match this.InternalNode with
            | SynTypeDefnKind.TyconDelegate (_, item1) ->
                FSharpSyntaxValInfo(this, item1)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<AbstractClass>]
type FSharpSyntaxTypeDefnSimpleRepr internal (parent: FSharpSyntaxNode, internalNode: SynTypeDefnSimpleRepr) =
    inherit FSharpSyntaxNode (parent)

    member _.InternalNode = internalNode

    override _.Range = FSharpSourceRange internalNode.Range


[<Sealed>]
type FSharpSyntaxTypeDefnSimpleReprUnion internal (parent: FSharpSyntaxNode, internalNode: SynTypeDefnSimpleRepr) =
    inherit FSharpSyntaxTypeDefnSimpleRepr(parent, internalNode)

    let mutable __item1 : FSharpSyntaxNodeList<FSharpSyntaxUnionCase> = Unchecked.defaultof<_>

    member this.item1 =
            match this.InternalNode with
            | SynTypeDefnSimpleRepr.Union (_, item1, _) ->
                FSharpSyntaxNodeList<FSharpSyntaxUnionCase>(this, item1)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxTypeDefnSimpleReprEnum internal (parent: FSharpSyntaxNode, internalNode: SynTypeDefnSimpleRepr) =
    inherit FSharpSyntaxTypeDefnSimpleRepr(parent, internalNode)

    let mutable __item0 : FSharpSyntaxNodeList<FSharpSyntaxEnumCase> = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynTypeDefnSimpleRepr.Enum (item0, _) ->
                FSharpSyntaxNodeList<FSharpSyntaxEnumCase>(this, item0)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxTypeDefnSimpleReprRecord internal (parent: FSharpSyntaxNode, internalNode: SynTypeDefnSimpleRepr) =
    inherit FSharpSyntaxTypeDefnSimpleRepr(parent, internalNode)

    let mutable __item1 : FSharpSyntaxNodeList<FSharpSyntaxField> = Unchecked.defaultof<_>

    member this.item1 =
            match this.InternalNode with
            | SynTypeDefnSimpleRepr.Record (_, item1, _) ->
                FSharpSyntaxNodeList<FSharpSyntaxField>(this, item1)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxTypeDefnSimpleReprGeneral internal (parent: FSharpSyntaxNode, internalNode: SynTypeDefnSimpleRepr) =
    inherit FSharpSyntaxTypeDefnSimpleRepr(parent, internalNode)

    let mutable __item0 : FSharpSyntaxTypeDefnKind = Unchecked.defaultof<_>
    let mutable __item3 : FSharpSyntaxNodeList<FSharpSyntaxField> = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynTypeDefnSimpleRepr.General (item0, _, _, _, _, _, _, _) ->
                FSharpSyntaxTypeDefnKind(this, item0)
            | _ -> failwith "invalid syntax" 

    member this.item3 =
            match this.InternalNode with
            | SynTypeDefnSimpleRepr.General (_, _, _, item3, _, _, _, _) ->
                FSharpSyntaxNodeList<FSharpSyntaxField>(this, item3)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxTypeDefnSimpleReprLibraryOnlyILAssembly internal (parent: FSharpSyntaxNode, internalNode: SynTypeDefnSimpleRepr) =
    inherit FSharpSyntaxTypeDefnSimpleRepr(parent, internalNode)






    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxTypeDefnSimpleReprTypeAbbrev internal (parent: FSharpSyntaxNode, internalNode: SynTypeDefnSimpleRepr) =
    inherit FSharpSyntaxTypeDefnSimpleRepr(parent, internalNode)

    let mutable __item1 : FSharpSyntaxType = Unchecked.defaultof<_>

    member this.item1 =
            match this.InternalNode with
            | SynTypeDefnSimpleRepr.TypeAbbrev (_, item1, _) ->
                FSharpSyntaxType(this, item1)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxTypeDefnSimpleReprNone internal (parent: FSharpSyntaxNode, internalNode: SynTypeDefnSimpleRepr) =
    inherit FSharpSyntaxTypeDefnSimpleRepr(parent, internalNode)






    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxTypeDefnSimpleReprException internal (parent: FSharpSyntaxNode, internalNode: SynTypeDefnSimpleRepr) =
    inherit FSharpSyntaxTypeDefnSimpleRepr(parent, internalNode)

    let mutable __item0 : FSharpSyntaxExceptionDefnRepr = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynTypeDefnSimpleRepr.Exception (item0) ->
                FSharpSyntaxExceptionDefnRepr(this, item0)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<AbstractClass>]
type FSharpSyntaxUnionCase internal (parent: FSharpSyntaxNode, internalNode: SynUnionCase) =
    inherit FSharpSyntaxNode (parent)

    member _.InternalNode = internalNode

    override _.Range = FSharpSourceRange internalNode.Range


[<Sealed>]
type FSharpSyntaxUnionCaseUnionCase internal (parent: FSharpSyntaxNode, internalNode: SynUnionCase) =
    inherit FSharpSyntaxUnionCase(parent, internalNode)

    let mutable __item0 : FSharpSyntaxNodeList<FSharpSyntaxAttributeList> = Unchecked.defaultof<_>
    let mutable __item1 : FSharpSyntaxIdent = Unchecked.defaultof<_>
    let mutable __item2 : FSharpSyntaxUnionCaseType = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynUnionCase.UnionCase (item0, _, _, _, _, _) ->
                FSharpSyntaxNodeList<FSharpSyntaxAttributeList>(this, item0)
            | _ -> failwith "invalid syntax" 

    member this.item1 =
            match this.InternalNode with
            | SynUnionCase.UnionCase (_, item1, _, _, _, _) ->
                FSharpSyntaxIdent(this, item1)
            | _ -> failwith "invalid syntax" 

    member this.item2 =
            match this.InternalNode with
            | SynUnionCase.UnionCase (_, _, item2, _, _, _) ->
                FSharpSyntaxUnionCaseType(this, item2)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<AbstractClass>]
type FSharpSyntaxArgInfo internal (parent: FSharpSyntaxNode, internalNode: SynArgInfo) =
    inherit FSharpSyntaxNode (parent)

    member _.InternalNode = internalNode

    override _.Range = FSharpSourceRange internalNode.Range


[<Sealed>]
type FSharpSyntaxArgInfoSynArgInfo internal (parent: FSharpSyntaxNode, internalNode: SynArgInfo) =
    inherit FSharpSyntaxArgInfo(parent, internalNode)

    let mutable __item0 : FSharpSyntaxNodeList<FSharpSyntaxAttributeList> = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynArgInfo.SynArgInfo (item0, _, _) ->
                FSharpSyntaxNodeList<FSharpSyntaxAttributeList>(this, item0)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<AbstractClass>]
type FSharpSyntaxEnumCase internal (parent: FSharpSyntaxNode, internalNode: SynEnumCase) =
    inherit FSharpSyntaxNode (parent)

    member _.InternalNode = internalNode

    override _.Range = FSharpSourceRange internalNode.Range


[<Sealed>]
type FSharpSyntaxEnumCaseEnumCase internal (parent: FSharpSyntaxNode, internalNode: SynEnumCase) =
    inherit FSharpSyntaxEnumCase(parent, internalNode)

    let mutable __item0 : FSharpSyntaxNodeList<FSharpSyntaxAttributeList> = Unchecked.defaultof<_>
    let mutable __item1 : FSharpSyntaxIdent = Unchecked.defaultof<_>
    let mutable __item2 : FSharpSyntaxConst = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynEnumCase.EnumCase (item0, _, _, _, _) ->
                FSharpSyntaxNodeList<FSharpSyntaxAttributeList>(this, item0)
            | _ -> failwith "invalid syntax" 

    member this.item1 =
            match this.InternalNode with
            | SynEnumCase.EnumCase (_, item1, _, _, _) ->
                FSharpSyntaxIdent(this, item1)
            | _ -> failwith "invalid syntax" 

    member this.item2 =
            match this.InternalNode with
            | SynEnumCase.EnumCase (_, _, item2, _, _) ->
                FSharpSyntaxConst(this, item2)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<AbstractClass>]
type FSharpSyntaxUnionCaseType internal (parent: FSharpSyntaxNode, internalNode: SynUnionCaseType) =
    inherit FSharpSyntaxNode (parent)

    member _.InternalNode = internalNode

    override _.Range = FSharpSourceRange internalNode.Range


[<Sealed>]
type FSharpSyntaxUnionCaseTypeUnionCaseFields internal (parent: FSharpSyntaxNode, internalNode: SynUnionCaseType) =
    inherit FSharpSyntaxUnionCaseType(parent, internalNode)

    let mutable __item0 : FSharpSyntaxNodeList<FSharpSyntaxField> = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynUnionCaseType.UnionCaseFields (item0) ->
                FSharpSyntaxNodeList<FSharpSyntaxField>(this, item0)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            
[<Sealed>]
type FSharpSyntaxUnionCaseTypeUnionCaseFullType internal (parent: FSharpSyntaxNode, internalNode: SynUnionCaseType) =
    inherit FSharpSyntaxUnionCaseType(parent, internalNode)

    let mutable __item0 : FSharpSyntaxType = Unchecked.defaultof<_>
    let mutable __item1 : FSharpSyntaxValInfo = Unchecked.defaultof<_>

    member this.item0 =
            match this.InternalNode with
            | SynUnionCaseType.UnionCaseFullType (item0, _) ->
                FSharpSyntaxType(this, item0)
            | _ -> failwith "invalid syntax" 

    member this.item1 =
            match this.InternalNode with
            | SynUnionCaseType.UnionCaseFullType (_, item1) ->
                FSharpSyntaxValInfo(this, item1)
            | _ -> failwith "invalid syntax" 


    override _.GetChild index =
        Unchecked.defaultof<_>

    override _.GetChildrenCount() = 0
            