open System
open System.Text
open System.Reflection
open System.Collections.Generic
open FSharp.Reflection
open FSharp.Compiler.SyntaxTree

module rec Visitor =

    type cenv = { genVisit: HashSet<Type>; genVisitComplete: HashSet<Type> }

    let canGen (ty: Type) =
        ty.Name.StartsWith "Syn" ||
        ty.Name.StartsWith "Parsed" ||
        ty.Name = "Ident"

    let genUnionFields (fields: PropertyInfo []) =
        fields
        |> Array.mapi (fun i _ -> "field" + string i)
        |> Array.reduce (fun x y -> x + ", " + y)

    let genUnionFieldVisits cenv (fields: PropertyInfo []) =
        fields
        |> Array.mapi (fun i field -> ("field" + string i, field.PropertyType))
        |> Array.mapi (fun i (fieldName, fieldTy) ->
            let isListType = fieldTy.IsGenericType && fieldTy.GetGenericTypeDefinition() = typedefof<_ list>
            let fieldTy =
                if isListType then fieldTy.GenericTypeArguments.[0]
                else fieldTy
            if not (canGen fieldTy) then
                if i = fields.Length - 1 then
                    sprintf """            %s |> ignore""" fieldName + "\n" +
                    sprintf """            None"""
                else
                    sprintf """            %s |> ignore""" fieldName
            else
                if not (cenv.genVisit.Contains fieldTy || cenv.genVisitComplete.Contains fieldTy)  then
                    cenv.genVisit.Add fieldTy |> ignore

                let isConst = fieldTy.Name = "SynConst" // heuristic, TODO: fixme

                if i = fields.Length - 1 then
                    sprintf """            this.TryVisit%s(%s, this.Visit, fun x -> %s)""" (if isListType then "List" else String.Empty) fieldName (if isConst then "x.Range range0" else "x.Range")
                else
                    sprintf """            let %s = this.TryVisit%s(%s, this.Visit, fun x -> %s)""" fieldName (if isListType then "List" else String.Empty) fieldName (if isConst then "x.Range range0" else "x.Range") + "\n" +
                    sprintf """            if %s.IsSome then %s""" fieldName fieldName + "\n" +
                    sprintf """            else"""
        )
        |> function
        | [||] -> "            None"
        | [|x|] -> x
        | xs -> xs |> Array.reduce (fun x y -> x + "\n" + y)

    let genSyntaxNodeUnionCaseInfo cenv (info: UnionCaseInfo) =
        let caseName = (info.DeclaringType.Name + "." + info.Name)

        let fields = info.GetFields()
        if Array.isEmpty fields then
            sprintf """
        | %s -> None""" caseName
        else
            sprintf """
        | %s (%s) -> 
%s
            """ caseName (genUnionFields fields) (genUnionFieldVisits cenv fields)

    let genSyntaxNodeUnionType cenv (ty: Type) =
        let genMatch =
            sprintf """
        match node with
            """
            
        genMatch +
        (
            FSharpType.GetUnionCases(ty)
            |> Array.map (genSyntaxNodeUnionCaseInfo cenv)
            |> function
            | [||] -> "        None"
            | [|x|] -> x
            | xs -> xs |> Array.reduce (+)
        )  

    let genSyntaxNode cenv (ty: Type) =
        if canGen ty then
            sprintf """
    abstract Visit: %s -> 'T option
    default this.Visit(node: %s) : 'T option =""" ty.Name ty.Name + "\n" +
            (
                if FSharpType.IsUnion ty then
                    genSyntaxNodeUnionType cenv ty
                else
                    "        node |> ignore\n        None"
            ) + "\n\n"
        else
            String.Empty

    let genRootSyntaxNode () =
        let cenv = { genVisit = HashSet(); genVisitComplete = HashSet() }
        cenv.genVisitComplete.Add typeof<ParsedInput> |> ignore

        let sb = StringBuilder()
        sb.Append(genSyntaxNode cenv typeof<ParsedInput>) |> ignore

        let queue = Queue(cenv.genVisit)
        cenv.genVisit
        |> Seq.iter (fun x -> cenv.genVisitComplete.Add x |> ignore)
        cenv.genVisit.Clear()

        let mutable ty = Unchecked.defaultof<_>
        while queue.TryDequeue &ty do
            sb.Append(genSyntaxNode cenv ty) |> ignore
            cenv.genVisit
            |> Seq.iter (queue.Enqueue)
            cenv.genVisit
            |> Seq.iter (fun x -> cenv.genVisitComplete.Add x |> ignore)
            cenv.genVisit.Clear()

        sb.ToString()

    let gen () =
        """module FSharp.Compiler.SyntaxTreeVisitor
        
open FSharp.Compiler.AbstractIL.Internal.Library
open FSharp.Compiler.SyntaxTree
open FSharp.Compiler.Range
open FSharp.Compiler.SyntaxTreeExtendedRanges

[<AbstractClass>]
type SyntaxTreeVisitor<'T> () =

    member inline private this.TryVisit(item, visit, getRange) =
        if this.CanVisit (getRange item) then
            visit item
        else
            None

    member inline private this.TryVisitList(items: _ list, visit, getRange) =
        let mutable result = None
        let mutable items = items
        while not (List.isEmpty items) && result.IsNone do
            match items with
            | [] -> ()
            | item :: tail ->
                items <- tail
                if this.CanVisit (getRange item) then
                    result <- visit item
        result

    abstract CanVisit: range -> bool  
    default this.CanVisit _ = true
        """ + "\n\n" + genRootSyntaxNode ()

open System.IO

[<EntryPoint>]
let main _ =
    File.WriteAllText("SyntaxTreeVisitor.fs", Visitor.gen ())
    0
