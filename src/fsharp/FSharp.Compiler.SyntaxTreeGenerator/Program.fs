open System
open System.Text
open System.Reflection
open System.Collections.Generic
open FSharp.Reflection
open FSharp.Compiler.SyntaxTree

module rec Visitor =

    type cenv = { queue: Queue<Type * string>; cache: Dictionary<Type, string * string> }

    let isListType (ty: Type) =
        ty.IsGenericType && ty.GetGenericTypeDefinition() = typedefof<_ list>

    let stripType (ty: Type) =
        if isListType ty then ty.GenericTypeArguments.[0]
        else ty

    let canGen (ty: Type) =
        ty.Name.StartsWith "Syn" ||
        ty.Name.StartsWith "Parsed" ||
        ty.Name = "Ident" ||
        ty.Name = "LongIdentWithDots"

    let genUnionFields target (fields: PropertyInfo []) =
        fields
        |> Array.mapi (fun i _ -> if i = target then "item" + string i else "_")
        |> Array.reduce (fun x y -> x + ", " + y)

    let genSyntaxSubNodeByUnionCase cenv tyDefName (info: UnionCaseInfo) =
        let name = "FSharpSyntax" + (info.DeclaringType.Name.Replace("Syn", String.Empty) + info.Name)

        sprintf """
[<Sealed>]
type %s internal (parent: FSharpSyntaxNode, internalNode: %s) =
    inherit %s(parent, internalNode)""" name info.DeclaringType.Name tyDefName + "\n\n" +
        match info.GetFields() with
        | [||] -> String.Empty
        | props ->
            let fields =
                props
                |> Array.mapi (fun i field -> i, field.PropertyType, if canGen (stripType field.PropertyType) then "item" + string i else "_")

            let mutableFields =
                fields
                |> Array.filter (fun (_, _, name) -> name <> "_")
                |> Array.map (fun (_, ty, name) ->
                    sprintf "    let mutable __%s : %s = Unchecked.defaultof<_>" name (genSyntaxNode cenv ty)
                )
                |> Array.reduce (fun x y -> x + "\n" + y)

            let properties =
                fields
                |> Array.filter (fun (_, _, name) -> name <> "_")
                |> Array.map (fun (i, ty, name) ->
                    sprintf "    member this.%s =
        match this with
        | %s.%s (%s) ->
            %s(this, %s)" name info.DeclaringType.Name info.Name (genUnionFields i props) (genSyntaxNode cenv ty) name)
                |> Array.reduce (fun x y -> x + "\n\n" + y)

            mutableFields + "\n\n" + properties

    let genSyntaxNodeBody cenv tyDefName (ty: Type) =
        if FSharpType.IsUnion ty then
            FSharpType.GetUnionCases(ty)
            |> Array.map (genSyntaxSubNodeByUnionCase cenv tyDefName)
            |> function
            | [||] -> String.Empty
            | [|x|] -> x
            | xs -> xs |> Array.reduce (+)
        else
            String.Empty

    let genSyntaxNode cenv (ty: Type) =
        match cenv.cache.TryGetValue ty with
        | true, (name, _) -> name
        | _ ->

        if isListType ty then
            
        elif canGen ty then
            let tyDefName = "FSharpSyntax" + ty.Name.Replace("Syn", String.Empty)
            let gen =
                sprintf """
[<Sealed>]
type FSharpSyntax%s internal (parent: FSharpSyntaxNode, internalNode: %s) =
    inherit FSharpSyntaxNode (parent)

    member _.InternalNode = internalNode

    override _.Range = internalNode.Range""" tyDefName ty.Name + "\n\n" + genSyntaxNodeBody cenv tyDefName ty
            cenv.queue.Enqueue(ty, gen)
            cenv.cache.[ty] <- (tyDefName, gen)
            tyDefName
        else
            failwith "invalid syntax node"

    let gen () =
        let cenv = { queue = Queue(); cache = Dictionary() }
        let src = genSyntaxNode cenv typeof<ParsedInput>
        """module rec FSharp.CodeAnalysis.SyntaxTree
        
open FSharp.Compiler.Range
open FSharp.Compiler.AbstractIL.Internal.Library
open FSharp.Compiler.SyntaxTree

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

    override _.Range = range0

    override _.GetChild index = nodes.[i]

    override _.GetChildrenCount() = nodes.Length

    member _.Nodes = nodes

[<AbstractClass>]
type FSharpSyntaxNode internal (parent: FSharpSyntaxNode) =
    
    abstract Range : FSharpSourceRange

    abstract GetChild : index: int -> FSharpSyntaxNode

    abstract ChildrenCount : unit -> int

    member _.Parent = parent""" + "\n" + src

open System.IO

[<EntryPoint>]
let main _ =
    File.WriteAllText("FSharpSyntaxTree.fs", Visitor.gen ())
    0
