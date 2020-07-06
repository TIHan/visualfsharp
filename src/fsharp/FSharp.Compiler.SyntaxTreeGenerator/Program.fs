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
                    sprintf """            %s |> ignore""" fieldName 
                else
                    sprintf """            %s |> ignore""" fieldName
            else
                if not (cenv.genVisit.Contains fieldTy || cenv.genVisitComplete.Contains fieldTy)  then
                    cenv.genVisit.Add fieldTy |> ignore

                if isListType then
                    sprintf """            %s |> List.iter this.Visit""" fieldName
                else
                    sprintf """            this.Visit %s""" fieldName
        )
        |> function
        | [||] -> String.Empty
        | [|x|] -> x
        | xs -> xs |> Array.reduce (fun x y -> x + "\n" + y)

    let genSyntaxNodeUnionCaseInfo cenv (info: UnionCaseInfo) =
        let caseName = (info.DeclaringType.Name + "." + info.Name)

        let fields = info.GetFields()
        if Array.isEmpty fields then
            sprintf """
        | %s -> ()""" caseName
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
    abstract Visit: %s -> unit
    default this.Visit(node: %s) : unit =""" ty.Name ty.Name + "\n" +
            (
                if FSharpType.IsUnion ty then
                    genSyntaxNodeUnionType cenv ty
                else
                    "        ()"
            ) + "\n"
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
        """ + "\n" + genRootSyntaxNode ()

open System.IO

[<EntryPoint>]
let main _ =
    File.WriteAllText("SyntaxTreeVisitor.fs", Visitor.gen ())
    0
