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

    let genUnionFields (fields: PropertyInfo []) =
        fields
        |> Array.mapi (fun i field -> if canGen (stripType field.PropertyType) then "field" + string i else "_")
        |> Array.reduce (fun x y -> x + ", " + y)

    let genUnionFieldVisits cenv (fields: PropertyInfo []) =
        fields
        |> Array.mapi (fun i field -> ("field" + string i, field.PropertyType))
        |> Array.map (fun (fieldName, fieldTy) ->
            let isListType = isListType fieldTy
            let fieldTy = stripType fieldTy
            if canGen fieldTy then
                if not (cenv.genVisit.Contains fieldTy || cenv.genVisitComplete.Contains fieldTy)  then
                    cenv.genVisit.Add fieldTy |> ignore

                if isListType then
                    sprintf """            %s |> List.iter this.Visit""" fieldName
                else
                    sprintf """            this.Visit %s""" fieldName
            else
                String.Empty
        )
        |> Array.filter (fun x -> not (String.IsNullOrWhiteSpace x))
        |> function
        | [||] -> """            ()"""
        | [|x|] -> x
        | xs -> xs |> Array.reduce (fun x y -> x + "\n" + y)

    let genSyntaxSubNodeByUnionCase cenv tyDefName (info: UnionCaseInfo) =
        let name = "FSharpSyntax" + (info.DeclaringType.Name.Replace("Syn", String.Empty) + info.Name)

        sprintf """
[<Sealed>]
type %s internal (parent: FSharpSyntaxNode, internalNode: %s) =
    inherit %s(parent, internalNode)""" name info.DeclaringType.Name tyDefName + "\n\n" +
        match info.GetFields() with
        | [||] -> String.Empty
        | fields ->
            let fields =
                fields
                |> Array.filter (fun field ->
                  //  let isListType = isListType fieldTy
                    let fieldTy = stripType fieldTy
                    canGen fieldTy
                )

            let mutableFields =
                fields
                |> Array.mapi (fun i field ->
                    let name = "Item" + string i

                    sprintf "    let mutable __%s : %s = Unchecked.defaultof<_>" name (genSyntaxNode cenv field.PropertyType)
                )
                |> Array.reduce (fun x y -> x + "\n" + y)

            let properties =
                fields
                |> Array.mapi (fun i field ->
                    let name = "Item" + string i

                    sprintf "    member this.%s =
        match this with
        | SynType.
                    "
                )

        if Array.isEmpty fields then
            String.Empty
        else
            sprintf """
        | %s (%s) -> 
%s
            """ caseName (genUnionFields fields) (genUnionFieldVisits cenv fields)

    let genSyntaxNodeBody cenv (ty: Type) =
        if FSharpType.IsUnion ty then
            FSharpType.GetUnionCases(ty)
            |> Array.map (genSyntaxSubNodeByUnionCase cenv)
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

        if canGen ty then
            let tyDefName = "FSharpSyntax" + ty.Name.Replace("Syn", String.Empty)
            let gen =
                sprintf """
[<Sealed>]
type FSharpSyntax%s internal (parent: FSharpSyntaxNode, internalNode: %s) =
    inherit FSharpSyntaxNode (parent)

    member _.InternalNode = internalNode

    override _.Range = internalNode.Range""" tyDefName ty.Name + "\n\n" + genSyntaxNodeBody cenv ty
            cenv.queue.Enqueue(ty, gen)
            cenv.cache.[ty] <- (tyDefName, gen)
            tyDefName
        else
            failwith "invalid syntax node"

    let gen () =
        let src, types = genRootSyntaxNode ()
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
