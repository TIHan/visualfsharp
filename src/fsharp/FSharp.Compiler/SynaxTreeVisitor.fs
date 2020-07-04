module FSharp.Compiler.SyntaxTreeVisitor
        
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
        


    abstract Visit: ParsedInput -> 'T option
    default this.Visit(node: ParsedInput) : 'T option =

        match node with
            
        | ParsedInput.ImplFile (field0) -> 
            this.TryVisit(field0, this.Visit, fun x -> x.Range)
            
        | ParsedInput.SigFile (field0) -> 
            this.TryVisit(field0, this.Visit, fun x -> x.Range)
            


    abstract Visit: ParsedImplFileInput -> 'T option
    default this.Visit(node: ParsedImplFileInput) : 'T option =

        match node with
            
        | ParsedImplFileInput.ParsedImplFileInput (field0, field1, field2, field3, field4, field5, field6) -> 
            field0 |> ignore
            field1 |> ignore
            field2 |> ignore
            field3 |> ignore
            let field4 = this.TryVisitList(field4, this.Visit, fun x -> x.Range)
            if field4.IsSome then field4
            else
            let field5 = this.TryVisitList(field5, this.Visit, fun x -> x.Range)
            if field5.IsSome then field5
            else
            field6 |> ignore
            None
            


    abstract Visit: ParsedSigFileInput -> 'T option
    default this.Visit(node: ParsedSigFileInput) : 'T option =

        match node with
            
        | ParsedSigFileInput.ParsedSigFileInput (field0, field1, field2, field3, field4) -> 
            field0 |> ignore
            field1 |> ignore
            field2 |> ignore
            let field3 = this.TryVisitList(field3, this.Visit, fun x -> x.Range)
            if field3.IsSome then field3
            else
            this.TryVisitList(field4, this.Visit, fun x -> x.Range)
            


    abstract Visit: ParsedHashDirective -> 'T option
    default this.Visit(node: ParsedHashDirective) : 'T option =

        match node with
            
        | ParsedHashDirective.ParsedHashDirective (field0, field1, field2) -> 
            field0 |> ignore
            field1 |> ignore
            field2 |> ignore
            None
            


    abstract Visit: SynModuleOrNamespace -> 'T option
    default this.Visit(node: SynModuleOrNamespace) : 'T option =

        match node with
            
        | SynModuleOrNamespace.SynModuleOrNamespace (field0, field1, field2, field3, field4, field5, field6, field7) -> 
            let field0 = this.TryVisitList(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            field1 |> ignore
            let field2 = this.TryVisit(field2, this.Visit, fun x -> x.Range)
            if field2.IsSome then field2
            else
            let field3 = this.TryVisitList(field3, this.Visit, fun x -> x.Range)
            if field3.IsSome then field3
            else
            field4 |> ignore
            let field5 = this.TryVisitList(field5, this.Visit, fun x -> x.Range)
            if field5.IsSome then field5
            else
            field6 |> ignore
            field7 |> ignore
            None
            


    abstract Visit: SynModuleOrNamespaceSig -> 'T option
    default this.Visit(node: SynModuleOrNamespaceSig) : 'T option =

        match node with
            
        | SynModuleOrNamespaceSig.SynModuleOrNamespaceSig (field0, field1, field2, field3, field4, field5, field6, field7) -> 
            let field0 = this.TryVisitList(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            field1 |> ignore
            let field2 = this.TryVisit(field2, this.Visit, fun x -> x.Range)
            if field2.IsSome then field2
            else
            let field3 = this.TryVisitList(field3, this.Visit, fun x -> x.Range)
            if field3.IsSome then field3
            else
            field4 |> ignore
            let field5 = this.TryVisitList(field5, this.Visit, fun x -> x.Range)
            if field5.IsSome then field5
            else
            field6 |> ignore
            field7 |> ignore
            None
            


    abstract Visit: Ident -> 'T option
    default this.Visit(node: Ident) : 'T option =
        node |> ignore
        None


    abstract Visit: SynModuleOrNamespaceKind -> 'T option
    default this.Visit(node: SynModuleOrNamespaceKind) : 'T option =

        match node with
            
        | SynModuleOrNamespaceKind.NamedModule -> None
        | SynModuleOrNamespaceKind.AnonModule -> None
        | SynModuleOrNamespaceKind.DeclaredNamespace -> None
        | SynModuleOrNamespaceKind.GlobalNamespace -> None


    abstract Visit: SynModuleDecl -> 'T option
    default this.Visit(node: SynModuleDecl) : 'T option =

        match node with
            
        | SynModuleDecl.ModuleAbbrev (field0, field1, field2) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            let field1 = this.TryVisitList(field1, this.Visit, fun x -> x.Range)
            if field1.IsSome then field1
            else
            field2 |> ignore
            None
            
        | SynModuleDecl.NestedModule (field0, field1, field2, field3, field4) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            field1 |> ignore
            let field2 = this.TryVisitList(field2, this.Visit, fun x -> x.Range)
            if field2.IsSome then field2
            else
            field3 |> ignore
            field4 |> ignore
            None
            
        | SynModuleDecl.Let (field0, field1, field2) -> 
            field0 |> ignore
            let field1 = this.TryVisitList(field1, this.Visit, fun x -> x.Range)
            if field1.IsSome then field1
            else
            field2 |> ignore
            None
            
        | SynModuleDecl.DoExpr (field0, field1, field2) -> 
            field0 |> ignore
            let field1 = this.TryVisit(field1, this.Visit, fun x -> x.Range)
            if field1.IsSome then field1
            else
            field2 |> ignore
            None
            
        | SynModuleDecl.Types (field0, field1) -> 
            let field0 = this.TryVisitList(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            field1 |> ignore
            None
            
        | SynModuleDecl.Exception (field0, field1) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            field1 |> ignore
            None
            
        | SynModuleDecl.Open (field0, field1) -> 
            field0 |> ignore
            field1 |> ignore
            None
            
        | SynModuleDecl.Attributes (field0, field1) -> 
            let field0 = this.TryVisitList(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            field1 |> ignore
            None
            
        | SynModuleDecl.HashDirective (field0, field1) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            field1 |> ignore
            None
            
        | SynModuleDecl.NamespaceFragment (field0) -> 
            this.TryVisit(field0, this.Visit, fun x -> x.Range)
            


    abstract Visit: SynAttributeList -> 'T option
    default this.Visit(node: SynAttributeList) : 'T option =
        node |> ignore
        None


    abstract Visit: SynModuleSigDecl -> 'T option
    default this.Visit(node: SynModuleSigDecl) : 'T option =

        match node with
            
        | SynModuleSigDecl.ModuleAbbrev (field0, field1, field2) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            let field1 = this.TryVisitList(field1, this.Visit, fun x -> x.Range)
            if field1.IsSome then field1
            else
            field2 |> ignore
            None
            
        | SynModuleSigDecl.NestedModule (field0, field1, field2, field3) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            field1 |> ignore
            let field2 = this.TryVisitList(field2, this.Visit, fun x -> x.Range)
            if field2.IsSome then field2
            else
            field3 |> ignore
            None
            
        | SynModuleSigDecl.Val (field0, field1) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            field1 |> ignore
            None
            
        | SynModuleSigDecl.Types (field0, field1) -> 
            let field0 = this.TryVisitList(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            field1 |> ignore
            None
            
        | SynModuleSigDecl.Exception (field0, field1) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            field1 |> ignore
            None
            
        | SynModuleSigDecl.Open (field0, field1) -> 
            let field0 = this.TryVisitList(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            field1 |> ignore
            None
            
        | SynModuleSigDecl.HashDirective (field0, field1) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            field1 |> ignore
            None
            
        | SynModuleSigDecl.NamespaceFragment (field0) -> 
            this.TryVisit(field0, this.Visit, fun x -> x.Range)
            


    abstract Visit: SynComponentInfo -> 'T option
    default this.Visit(node: SynComponentInfo) : 'T option =

        match node with
            
        | SynComponentInfo.ComponentInfo (field0, field1, field2, field3, field4, field5, field6, field7) -> 
            let field0 = this.TryVisitList(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            let field1 = this.TryVisitList(field1, this.Visit, fun x -> x.Range)
            if field1.IsSome then field1
            else
            let field2 = this.TryVisitList(field2, this.Visit, fun x -> x.Range)
            if field2.IsSome then field2
            else
            let field3 = this.TryVisitList(field3, this.Visit, fun x -> x.Range)
            if field3.IsSome then field3
            else
            field4 |> ignore
            field5 |> ignore
            field6 |> ignore
            field7 |> ignore
            None
            


    abstract Visit: SynBinding -> 'T option
    default this.Visit(node: SynBinding) : 'T option =

        match node with
            
        | SynBinding.Binding (field0, field1, field2, field3, field4, field5, field6, field7, field8, field9, field10, field11) -> 
            field0 |> ignore
            let field1 = this.TryVisit(field1, this.Visit, fun x -> x.Range)
            if field1.IsSome then field1
            else
            field2 |> ignore
            field3 |> ignore
            let field4 = this.TryVisitList(field4, this.Visit, fun x -> x.Range)
            if field4.IsSome then field4
            else
            field5 |> ignore
            let field6 = this.TryVisit(field6, this.Visit, fun x -> x.Range)
            if field6.IsSome then field6
            else
            let field7 = this.TryVisit(field7, this.Visit, fun x -> x.Range)
            if field7.IsSome then field7
            else
            field8 |> ignore
            let field9 = this.TryVisit(field9, this.Visit, fun x -> x.Range)
            if field9.IsSome then field9
            else
            field10 |> ignore
            field11 |> ignore
            None
            


    abstract Visit: SynExpr -> 'T option
    default this.Visit(node: SynExpr) : 'T option =

        match node with
            
        | SynExpr.Paren (field0, field1, field2, field3) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            field1 |> ignore
            field2 |> ignore
            field3 |> ignore
            None
            
        | SynExpr.Quote (field0, field1, field2, field3, field4) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            field1 |> ignore
            let field2 = this.TryVisit(field2, this.Visit, fun x -> x.Range)
            if field2.IsSome then field2
            else
            field3 |> ignore
            field4 |> ignore
            None
            
        | SynExpr.Const (field0, field1) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range range0)
            if field0.IsSome then field0
            else
            field1 |> ignore
            None
            
        | SynExpr.Typed (field0, field1, field2) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            let field1 = this.TryVisit(field1, this.Visit, fun x -> x.Range)
            if field1.IsSome then field1
            else
            field2 |> ignore
            None
            
        | SynExpr.Tuple (field0, field1, field2, field3) -> 
            field0 |> ignore
            let field1 = this.TryVisitList(field1, this.Visit, fun x -> x.Range)
            if field1.IsSome then field1
            else
            field2 |> ignore
            field3 |> ignore
            None
            
        | SynExpr.AnonRecd (field0, field1, field2, field3) -> 
            field0 |> ignore
            field1 |> ignore
            field2 |> ignore
            field3 |> ignore
            None
            
        | SynExpr.ArrayOrList (field0, field1, field2) -> 
            field0 |> ignore
            let field1 = this.TryVisitList(field1, this.Visit, fun x -> x.Range)
            if field1.IsSome then field1
            else
            field2 |> ignore
            None
            
        | SynExpr.Record (field0, field1, field2, field3) -> 
            field0 |> ignore
            field1 |> ignore
            field2 |> ignore
            field3 |> ignore
            None
            
        | SynExpr.New (field0, field1, field2, field3) -> 
            field0 |> ignore
            let field1 = this.TryVisit(field1, this.Visit, fun x -> x.Range)
            if field1.IsSome then field1
            else
            let field2 = this.TryVisit(field2, this.Visit, fun x -> x.Range)
            if field2.IsSome then field2
            else
            field3 |> ignore
            None
            
        | SynExpr.ObjExpr (field0, field1, field2, field3, field4, field5) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            field1 |> ignore
            let field2 = this.TryVisitList(field2, this.Visit, fun x -> x.Range)
            if field2.IsSome then field2
            else
            let field3 = this.TryVisitList(field3, this.Visit, fun x -> x.Range)
            if field3.IsSome then field3
            else
            field4 |> ignore
            field5 |> ignore
            None
            
        | SynExpr.While (field0, field1, field2, field3) -> 
            field0 |> ignore
            let field1 = this.TryVisit(field1, this.Visit, fun x -> x.Range)
            if field1.IsSome then field1
            else
            let field2 = this.TryVisit(field2, this.Visit, fun x -> x.Range)
            if field2.IsSome then field2
            else
            field3 |> ignore
            None
            
        | SynExpr.For (field0, field1, field2, field3, field4, field5, field6) -> 
            field0 |> ignore
            let field1 = this.TryVisit(field1, this.Visit, fun x -> x.Range)
            if field1.IsSome then field1
            else
            let field2 = this.TryVisit(field2, this.Visit, fun x -> x.Range)
            if field2.IsSome then field2
            else
            field3 |> ignore
            let field4 = this.TryVisit(field4, this.Visit, fun x -> x.Range)
            if field4.IsSome then field4
            else
            let field5 = this.TryVisit(field5, this.Visit, fun x -> x.Range)
            if field5.IsSome then field5
            else
            field6 |> ignore
            None
            
        | SynExpr.ForEach (field0, field1, field2, field3, field4, field5, field6) -> 
            field0 |> ignore
            field1 |> ignore
            field2 |> ignore
            let field3 = this.TryVisit(field3, this.Visit, fun x -> x.Range)
            if field3.IsSome then field3
            else
            let field4 = this.TryVisit(field4, this.Visit, fun x -> x.Range)
            if field4.IsSome then field4
            else
            let field5 = this.TryVisit(field5, this.Visit, fun x -> x.Range)
            if field5.IsSome then field5
            else
            field6 |> ignore
            None
            
        | SynExpr.ArrayOrListOfSeqExpr (field0, field1, field2) -> 
            field0 |> ignore
            let field1 = this.TryVisit(field1, this.Visit, fun x -> x.Range)
            if field1.IsSome then field1
            else
            field2 |> ignore
            None
            
        | SynExpr.CompExpr (field0, field1, field2, field3) -> 
            field0 |> ignore
            field1 |> ignore
            let field2 = this.TryVisit(field2, this.Visit, fun x -> x.Range)
            if field2.IsSome then field2
            else
            field3 |> ignore
            None
            
        | SynExpr.Lambda (field0, field1, field2, field3, field4) -> 
            field0 |> ignore
            field1 |> ignore
            let field2 = this.TryVisit(field2, this.Visit, fun x -> x.Range)
            if field2.IsSome then field2
            else
            let field3 = this.TryVisit(field3, this.Visit, fun x -> x.Range)
            if field3.IsSome then field3
            else
            field4 |> ignore
            None
            
        | SynExpr.MatchLambda (field0, field1, field2, field3, field4) -> 
            field0 |> ignore
            field1 |> ignore
            let field2 = this.TryVisitList(field2, this.Visit, fun x -> x.Range)
            if field2.IsSome then field2
            else
            field3 |> ignore
            field4 |> ignore
            None
            
        | SynExpr.Match (field0, field1, field2, field3) -> 
            field0 |> ignore
            let field1 = this.TryVisit(field1, this.Visit, fun x -> x.Range)
            if field1.IsSome then field1
            else
            let field2 = this.TryVisitList(field2, this.Visit, fun x -> x.Range)
            if field2.IsSome then field2
            else
            field3 |> ignore
            None
            
        | SynExpr.Do (field0, field1) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            field1 |> ignore
            None
            
        | SynExpr.Assert (field0, field1) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            field1 |> ignore
            None
            
        | SynExpr.App (field0, field1, field2, field3, field4) -> 
            field0 |> ignore
            field1 |> ignore
            let field2 = this.TryVisit(field2, this.Visit, fun x -> x.Range)
            if field2.IsSome then field2
            else
            let field3 = this.TryVisit(field3, this.Visit, fun x -> x.Range)
            if field3.IsSome then field3
            else
            field4 |> ignore
            None
            
        | SynExpr.TypeApp (field0, field1, field2, field3, field4, field5, field6) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            field1 |> ignore
            let field2 = this.TryVisitList(field2, this.Visit, fun x -> x.Range)
            if field2.IsSome then field2
            else
            field3 |> ignore
            field4 |> ignore
            field5 |> ignore
            field6 |> ignore
            None
            
        | SynExpr.LetOrUse (field0, field1, field2, field3, field4) -> 
            field0 |> ignore
            field1 |> ignore
            let field2 = this.TryVisitList(field2, this.Visit, fun x -> x.Range)
            if field2.IsSome then field2
            else
            let field3 = this.TryVisit(field3, this.Visit, fun x -> x.Range)
            if field3.IsSome then field3
            else
            field4 |> ignore
            None
            
        | SynExpr.TryWith (field0, field1, field2, field3, field4, field5, field6) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            field1 |> ignore
            let field2 = this.TryVisitList(field2, this.Visit, fun x -> x.Range)
            if field2.IsSome then field2
            else
            field3 |> ignore
            field4 |> ignore
            field5 |> ignore
            field6 |> ignore
            None
            
        | SynExpr.TryFinally (field0, field1, field2, field3, field4) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            let field1 = this.TryVisit(field1, this.Visit, fun x -> x.Range)
            if field1.IsSome then field1
            else
            field2 |> ignore
            field3 |> ignore
            field4 |> ignore
            None
            
        | SynExpr.Lazy (field0, field1) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            field1 |> ignore
            None
            
        | SynExpr.Sequential (field0, field1, field2, field3, field4) -> 
            field0 |> ignore
            field1 |> ignore
            let field2 = this.TryVisit(field2, this.Visit, fun x -> x.Range)
            if field2.IsSome then field2
            else
            let field3 = this.TryVisit(field3, this.Visit, fun x -> x.Range)
            if field3.IsSome then field3
            else
            field4 |> ignore
            None
            
        | SynExpr.IfThenElse (field0, field1, field2, field3, field4, field5, field6) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            let field1 = this.TryVisit(field1, this.Visit, fun x -> x.Range)
            if field1.IsSome then field1
            else
            field2 |> ignore
            field3 |> ignore
            field4 |> ignore
            field5 |> ignore
            field6 |> ignore
            None
            
        | SynExpr.Ident (field0) -> 
            this.TryVisit(field0, this.Visit, fun x -> x.Range)
            
        | SynExpr.LongIdent (field0, field1, field2, field3) -> 
            field0 |> ignore
            field1 |> ignore
            field2 |> ignore
            field3 |> ignore
            None
            
        | SynExpr.LongIdentSet (field0, field1, field2) -> 
            field0 |> ignore
            let field1 = this.TryVisit(field1, this.Visit, fun x -> x.Range)
            if field1.IsSome then field1
            else
            field2 |> ignore
            None
            
        | SynExpr.DotGet (field0, field1, field2, field3) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            field1 |> ignore
            field2 |> ignore
            field3 |> ignore
            None
            
        | SynExpr.DotSet (field0, field1, field2, field3) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            field1 |> ignore
            let field2 = this.TryVisit(field2, this.Visit, fun x -> x.Range)
            if field2.IsSome then field2
            else
            field3 |> ignore
            None
            
        | SynExpr.Set (field0, field1, field2) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            let field1 = this.TryVisit(field1, this.Visit, fun x -> x.Range)
            if field1.IsSome then field1
            else
            field2 |> ignore
            None
            
        | SynExpr.DotIndexedGet (field0, field1, field2, field3) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            let field1 = this.TryVisitList(field1, this.Visit, fun x -> x.Range)
            if field1.IsSome then field1
            else
            field2 |> ignore
            field3 |> ignore
            None
            
        | SynExpr.DotIndexedSet (field0, field1, field2, field3, field4, field5) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            let field1 = this.TryVisitList(field1, this.Visit, fun x -> x.Range)
            if field1.IsSome then field1
            else
            let field2 = this.TryVisit(field2, this.Visit, fun x -> x.Range)
            if field2.IsSome then field2
            else
            field3 |> ignore
            field4 |> ignore
            field5 |> ignore
            None
            
        | SynExpr.NamedIndexedPropertySet (field0, field1, field2, field3) -> 
            field0 |> ignore
            let field1 = this.TryVisit(field1, this.Visit, fun x -> x.Range)
            if field1.IsSome then field1
            else
            let field2 = this.TryVisit(field2, this.Visit, fun x -> x.Range)
            if field2.IsSome then field2
            else
            field3 |> ignore
            None
            
        | SynExpr.DotNamedIndexedPropertySet (field0, field1, field2, field3, field4) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            field1 |> ignore
            let field2 = this.TryVisit(field2, this.Visit, fun x -> x.Range)
            if field2.IsSome then field2
            else
            let field3 = this.TryVisit(field3, this.Visit, fun x -> x.Range)
            if field3.IsSome then field3
            else
            field4 |> ignore
            None
            
        | SynExpr.TypeTest (field0, field1, field2) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            let field1 = this.TryVisit(field1, this.Visit, fun x -> x.Range)
            if field1.IsSome then field1
            else
            field2 |> ignore
            None
            
        | SynExpr.Upcast (field0, field1, field2) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            let field1 = this.TryVisit(field1, this.Visit, fun x -> x.Range)
            if field1.IsSome then field1
            else
            field2 |> ignore
            None
            
        | SynExpr.Downcast (field0, field1, field2) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            let field1 = this.TryVisit(field1, this.Visit, fun x -> x.Range)
            if field1.IsSome then field1
            else
            field2 |> ignore
            None
            
        | SynExpr.InferredUpcast (field0, field1) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            field1 |> ignore
            None
            
        | SynExpr.InferredDowncast (field0, field1) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            field1 |> ignore
            None
            
        | SynExpr.Null (field0) -> 
            field0 |> ignore
            None
            
        | SynExpr.AddressOf (field0, field1, field2, field3) -> 
            field0 |> ignore
            let field1 = this.TryVisit(field1, this.Visit, fun x -> x.Range)
            if field1.IsSome then field1
            else
            field2 |> ignore
            field3 |> ignore
            None
            
        | SynExpr.TraitCall (field0, field1, field2, field3) -> 
            let field0 = this.TryVisitList(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            let field1 = this.TryVisit(field1, this.Visit, fun x -> x.Range)
            if field1.IsSome then field1
            else
            let field2 = this.TryVisit(field2, this.Visit, fun x -> x.Range)
            if field2.IsSome then field2
            else
            field3 |> ignore
            None
            
        | SynExpr.JoinIn (field0, field1, field2, field3) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            field1 |> ignore
            let field2 = this.TryVisit(field2, this.Visit, fun x -> x.Range)
            if field2.IsSome then field2
            else
            field3 |> ignore
            None
            
        | SynExpr.ImplicitZero (field0) -> 
            field0 |> ignore
            None
            
        | SynExpr.SequentialOrImplicitYield (field0, field1, field2, field3, field4) -> 
            field0 |> ignore
            let field1 = this.TryVisit(field1, this.Visit, fun x -> x.Range)
            if field1.IsSome then field1
            else
            let field2 = this.TryVisit(field2, this.Visit, fun x -> x.Range)
            if field2.IsSome then field2
            else
            let field3 = this.TryVisit(field3, this.Visit, fun x -> x.Range)
            if field3.IsSome then field3
            else
            field4 |> ignore
            None
            
        | SynExpr.YieldOrReturn (field0, field1, field2) -> 
            field0 |> ignore
            let field1 = this.TryVisit(field1, this.Visit, fun x -> x.Range)
            if field1.IsSome then field1
            else
            field2 |> ignore
            None
            
        | SynExpr.YieldOrReturnFrom (field0, field1, field2) -> 
            field0 |> ignore
            let field1 = this.TryVisit(field1, this.Visit, fun x -> x.Range)
            if field1.IsSome then field1
            else
            field2 |> ignore
            None
            
        | SynExpr.LetOrUseBang (field0, field1, field2, field3, field4, field5, field6, field7) -> 
            field0 |> ignore
            field1 |> ignore
            field2 |> ignore
            let field3 = this.TryVisit(field3, this.Visit, fun x -> x.Range)
            if field3.IsSome then field3
            else
            let field4 = this.TryVisit(field4, this.Visit, fun x -> x.Range)
            if field4.IsSome then field4
            else
            field5 |> ignore
            let field6 = this.TryVisit(field6, this.Visit, fun x -> x.Range)
            if field6.IsSome then field6
            else
            field7 |> ignore
            None
            
        | SynExpr.MatchBang (field0, field1, field2, field3) -> 
            field0 |> ignore
            let field1 = this.TryVisit(field1, this.Visit, fun x -> x.Range)
            if field1.IsSome then field1
            else
            let field2 = this.TryVisitList(field2, this.Visit, fun x -> x.Range)
            if field2.IsSome then field2
            else
            field3 |> ignore
            None
            
        | SynExpr.DoBang (field0, field1) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            field1 |> ignore
            None
            
        | SynExpr.LibraryOnlyILAssembly (field0, field1, field2, field3, field4) -> 
            field0 |> ignore
            let field1 = this.TryVisitList(field1, this.Visit, fun x -> x.Range)
            if field1.IsSome then field1
            else
            let field2 = this.TryVisitList(field2, this.Visit, fun x -> x.Range)
            if field2.IsSome then field2
            else
            let field3 = this.TryVisitList(field3, this.Visit, fun x -> x.Range)
            if field3.IsSome then field3
            else
            field4 |> ignore
            None
            
        | SynExpr.LibraryOnlyStaticOptimization (field0, field1, field2, field3) -> 
            let field0 = this.TryVisitList(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            let field1 = this.TryVisit(field1, this.Visit, fun x -> x.Range)
            if field1.IsSome then field1
            else
            let field2 = this.TryVisit(field2, this.Visit, fun x -> x.Range)
            if field2.IsSome then field2
            else
            field3 |> ignore
            None
            
        | SynExpr.LibraryOnlyUnionCaseFieldGet (field0, field1, field2, field3) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            let field1 = this.TryVisitList(field1, this.Visit, fun x -> x.Range)
            if field1.IsSome then field1
            else
            field2 |> ignore
            field3 |> ignore
            None
            
        | SynExpr.LibraryOnlyUnionCaseFieldSet (field0, field1, field2, field3, field4) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            let field1 = this.TryVisitList(field1, this.Visit, fun x -> x.Range)
            if field1.IsSome then field1
            else
            field2 |> ignore
            let field3 = this.TryVisit(field3, this.Visit, fun x -> x.Range)
            if field3.IsSome then field3
            else
            field4 |> ignore
            None
            
        | SynExpr.ArbitraryAfterError (field0, field1) -> 
            field0 |> ignore
            field1 |> ignore
            None
            
        | SynExpr.FromParseError (field0, field1) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            field1 |> ignore
            None
            
        | SynExpr.DiscardAfterMissingQualificationAfterDot (field0, field1) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            field1 |> ignore
            None
            
        | SynExpr.Fixed (field0, field1) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            field1 |> ignore
            None
            


    abstract Visit: SynTypeDefn -> 'T option
    default this.Visit(node: SynTypeDefn) : 'T option =

        match node with
            
        | SynTypeDefn.TypeDefn (field0, field1, field2, field3) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            let field1 = this.TryVisit(field1, this.Visit, fun x -> x.Range)
            if field1.IsSome then field1
            else
            let field2 = this.TryVisitList(field2, this.Visit, fun x -> x.Range)
            if field2.IsSome then field2
            else
            field3 |> ignore
            None
            


    abstract Visit: SynExceptionDefn -> 'T option
    default this.Visit(node: SynExceptionDefn) : 'T option =

        match node with
            
        | SynExceptionDefn.SynExceptionDefn (field0, field1, field2) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            let field1 = this.TryVisitList(field1, this.Visit, fun x -> x.Range)
            if field1.IsSome then field1
            else
            field2 |> ignore
            None
            


    abstract Visit: SynValSig -> 'T option
    default this.Visit(node: SynValSig) : 'T option =

        match node with
            
        | SynValSig.ValSpfn (field0, field1, field2, field3, field4, field5, field6, field7, field8, field9, field10) -> 
            let field0 = this.TryVisitList(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            let field1 = this.TryVisit(field1, this.Visit, fun x -> x.Range)
            if field1.IsSome then field1
            else
            let field2 = this.TryVisit(field2, this.Visit, fun x -> x.Range)
            if field2.IsSome then field2
            else
            let field3 = this.TryVisit(field3, this.Visit, fun x -> x.Range)
            if field3.IsSome then field3
            else
            let field4 = this.TryVisit(field4, this.Visit, fun x -> x.Range)
            if field4.IsSome then field4
            else
            field5 |> ignore
            field6 |> ignore
            field7 |> ignore
            field8 |> ignore
            field9 |> ignore
            field10 |> ignore
            None
            


    abstract Visit: SynTypeDefnSig -> 'T option
    default this.Visit(node: SynTypeDefnSig) : 'T option =

        match node with
            
        | SynTypeDefnSig.TypeDefnSig (field0, field1, field2, field3) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            let field1 = this.TryVisit(field1, this.Visit, fun x -> x.Range)
            if field1.IsSome then field1
            else
            let field2 = this.TryVisitList(field2, this.Visit, fun x -> x.Range)
            if field2.IsSome then field2
            else
            field3 |> ignore
            None
            


    abstract Visit: SynExceptionSig -> 'T option
    default this.Visit(node: SynExceptionSig) : 'T option =

        match node with
            
        | SynExceptionSig.SynExceptionSig (field0, field1, field2) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            let field1 = this.TryVisitList(field1, this.Visit, fun x -> x.Range)
            if field1.IsSome then field1
            else
            field2 |> ignore
            None
            


    abstract Visit: SynTyparDecl -> 'T option
    default this.Visit(node: SynTyparDecl) : 'T option =

        match node with
            
        | SynTyparDecl.TyparDecl (field0, field1) -> 
            let field0 = this.TryVisitList(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            this.TryVisit(field1, this.Visit, fun x -> x.Range)
            


    abstract Visit: SynTypeConstraint -> 'T option
    default this.Visit(node: SynTypeConstraint) : 'T option =

        match node with
            
        | SynTypeConstraint.WhereTyparIsValueType (field0, field1) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            field1 |> ignore
            None
            
        | SynTypeConstraint.WhereTyparIsReferenceType (field0, field1) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            field1 |> ignore
            None
            
        | SynTypeConstraint.WhereTyparIsUnmanaged (field0, field1) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            field1 |> ignore
            None
            
        | SynTypeConstraint.WhereTyparSupportsNull (field0, field1) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            field1 |> ignore
            None
            
        | SynTypeConstraint.WhereTyparIsComparable (field0, field1) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            field1 |> ignore
            None
            
        | SynTypeConstraint.WhereTyparIsEquatable (field0, field1) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            field1 |> ignore
            None
            
        | SynTypeConstraint.WhereTyparDefaultsToType (field0, field1, field2) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            let field1 = this.TryVisit(field1, this.Visit, fun x -> x.Range)
            if field1.IsSome then field1
            else
            field2 |> ignore
            None
            
        | SynTypeConstraint.WhereTyparSubtypeOfType (field0, field1, field2) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            let field1 = this.TryVisit(field1, this.Visit, fun x -> x.Range)
            if field1.IsSome then field1
            else
            field2 |> ignore
            None
            
        | SynTypeConstraint.WhereTyparSupportsMember (field0, field1, field2) -> 
            let field0 = this.TryVisitList(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            let field1 = this.TryVisit(field1, this.Visit, fun x -> x.Range)
            if field1.IsSome then field1
            else
            field2 |> ignore
            None
            
        | SynTypeConstraint.WhereTyparIsEnum (field0, field1, field2) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            let field1 = this.TryVisitList(field1, this.Visit, fun x -> x.Range)
            if field1.IsSome then field1
            else
            field2 |> ignore
            None
            
        | SynTypeConstraint.WhereTyparIsDelegate (field0, field1, field2) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            let field1 = this.TryVisitList(field1, this.Visit, fun x -> x.Range)
            if field1.IsSome then field1
            else
            field2 |> ignore
            None
            


    abstract Visit: SynBindingKind -> 'T option
    default this.Visit(node: SynBindingKind) : 'T option =

        match node with
            
        | SynBindingKind.StandaloneExpression -> None
        | SynBindingKind.NormalBinding -> None
        | SynBindingKind.DoBinding -> None


    abstract Visit: SynValData -> 'T option
    default this.Visit(node: SynValData) : 'T option =

        match node with
            
        | SynValData.SynValData (field0, field1, field2) -> 
            field0 |> ignore
            let field1 = this.TryVisit(field1, this.Visit, fun x -> x.Range)
            if field1.IsSome then field1
            else
            field2 |> ignore
            None
            


    abstract Visit: SynPat -> 'T option
    default this.Visit(node: SynPat) : 'T option =

        match node with
            
        | SynPat.Const (field0, field1) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range range0)
            if field0.IsSome then field0
            else
            field1 |> ignore
            None
            
        | SynPat.Wild (field0) -> 
            field0 |> ignore
            None
            
        | SynPat.Named (field0, field1, field2, field3, field4) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            let field1 = this.TryVisit(field1, this.Visit, fun x -> x.Range)
            if field1.IsSome then field1
            else
            field2 |> ignore
            field3 |> ignore
            field4 |> ignore
            None
            
        | SynPat.Typed (field0, field1, field2) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            let field1 = this.TryVisit(field1, this.Visit, fun x -> x.Range)
            if field1.IsSome then field1
            else
            field2 |> ignore
            None
            
        | SynPat.Attrib (field0, field1, field2) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            let field1 = this.TryVisitList(field1, this.Visit, fun x -> x.Range)
            if field1.IsSome then field1
            else
            field2 |> ignore
            None
            
        | SynPat.Or (field0, field1, field2) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            let field1 = this.TryVisit(field1, this.Visit, fun x -> x.Range)
            if field1.IsSome then field1
            else
            field2 |> ignore
            None
            
        | SynPat.Ands (field0, field1) -> 
            let field0 = this.TryVisitList(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            field1 |> ignore
            None
            
        | SynPat.LongIdent (field0, field1, field2, field3, field4, field5) -> 
            field0 |> ignore
            field1 |> ignore
            field2 |> ignore
            let field3 = this.TryVisit(field3, this.Visit, fun x -> x.Range)
            if field3.IsSome then field3
            else
            field4 |> ignore
            field5 |> ignore
            None
            
        | SynPat.Tuple (field0, field1, field2) -> 
            field0 |> ignore
            let field1 = this.TryVisitList(field1, this.Visit, fun x -> x.Range)
            if field1.IsSome then field1
            else
            field2 |> ignore
            None
            
        | SynPat.Paren (field0, field1) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            field1 |> ignore
            None
            
        | SynPat.ArrayOrList (field0, field1, field2) -> 
            field0 |> ignore
            let field1 = this.TryVisitList(field1, this.Visit, fun x -> x.Range)
            if field1.IsSome then field1
            else
            field2 |> ignore
            None
            
        | SynPat.Record (field0, field1) -> 
            field0 |> ignore
            field1 |> ignore
            None
            
        | SynPat.Null (field0) -> 
            field0 |> ignore
            None
            
        | SynPat.OptionalVal (field0, field1) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            field1 |> ignore
            None
            
        | SynPat.IsInst (field0, field1) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            field1 |> ignore
            None
            
        | SynPat.QuoteExpr (field0, field1) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            field1 |> ignore
            None
            
        | SynPat.DeprecatedCharRange (field0, field1, field2) -> 
            field0 |> ignore
            field1 |> ignore
            field2 |> ignore
            None
            
        | SynPat.InstanceMember (field0, field1, field2, field3, field4) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            let field1 = this.TryVisit(field1, this.Visit, fun x -> x.Range)
            if field1.IsSome then field1
            else
            field2 |> ignore
            field3 |> ignore
            field4 |> ignore
            None
            
        | SynPat.FromParseError (field0, field1) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            field1 |> ignore
            None
            


    abstract Visit: SynConst -> 'T option
    default this.Visit(node: SynConst) : 'T option =

        match node with
            
        | SynConst.Unit -> None
        | SynConst.Bool (field0) -> 
            field0 |> ignore
            None
            
        | SynConst.SByte (field0) -> 
            field0 |> ignore
            None
            
        | SynConst.Byte (field0) -> 
            field0 |> ignore
            None
            
        | SynConst.Int16 (field0) -> 
            field0 |> ignore
            None
            
        | SynConst.UInt16 (field0) -> 
            field0 |> ignore
            None
            
        | SynConst.Int32 (field0) -> 
            field0 |> ignore
            None
            
        | SynConst.UInt32 (field0) -> 
            field0 |> ignore
            None
            
        | SynConst.Int64 (field0) -> 
            field0 |> ignore
            None
            
        | SynConst.UInt64 (field0) -> 
            field0 |> ignore
            None
            
        | SynConst.IntPtr (field0) -> 
            field0 |> ignore
            None
            
        | SynConst.UIntPtr (field0) -> 
            field0 |> ignore
            None
            
        | SynConst.Single (field0) -> 
            field0 |> ignore
            None
            
        | SynConst.Double (field0) -> 
            field0 |> ignore
            None
            
        | SynConst.Char (field0) -> 
            field0 |> ignore
            None
            
        | SynConst.Decimal (field0) -> 
            field0 |> ignore
            None
            
        | SynConst.UserNum (field0, field1) -> 
            field0 |> ignore
            field1 |> ignore
            None
            
        | SynConst.String (field0, field1) -> 
            field0 |> ignore
            field1 |> ignore
            None
            
        | SynConst.Bytes (field0, field1) -> 
            field0 |> ignore
            field1 |> ignore
            None
            
        | SynConst.UInt16s (field0) -> 
            field0 |> ignore
            None
            
        | SynConst.Measure (field0, field1) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range range0)
            if field0.IsSome then field0
            else
            this.TryVisit(field1, this.Visit, fun x -> x.Range)
            


    abstract Visit: SynType -> 'T option
    default this.Visit(node: SynType) : 'T option =

        match node with
            
        | SynType.LongIdent (field0) -> 
            field0 |> ignore
            None
            
        | SynType.App (field0, field1, field2, field3, field4, field5, field6) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            field1 |> ignore
            let field2 = this.TryVisitList(field2, this.Visit, fun x -> x.Range)
            if field2.IsSome then field2
            else
            field3 |> ignore
            field4 |> ignore
            field5 |> ignore
            field6 |> ignore
            None
            
        | SynType.LongIdentApp (field0, field1, field2, field3, field4, field5, field6) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            field1 |> ignore
            field2 |> ignore
            let field3 = this.TryVisitList(field3, this.Visit, fun x -> x.Range)
            if field3.IsSome then field3
            else
            field4 |> ignore
            field5 |> ignore
            field6 |> ignore
            None
            
        | SynType.Tuple (field0, field1, field2) -> 
            field0 |> ignore
            field1 |> ignore
            field2 |> ignore
            None
            
        | SynType.AnonRecd (field0, field1, field2) -> 
            field0 |> ignore
            field1 |> ignore
            field2 |> ignore
            None
            
        | SynType.Array (field0, field1, field2) -> 
            field0 |> ignore
            let field1 = this.TryVisit(field1, this.Visit, fun x -> x.Range)
            if field1.IsSome then field1
            else
            field2 |> ignore
            None
            
        | SynType.Fun (field0, field1, field2) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            let field1 = this.TryVisit(field1, this.Visit, fun x -> x.Range)
            if field1.IsSome then field1
            else
            field2 |> ignore
            None
            
        | SynType.Var (field0, field1) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            field1 |> ignore
            None
            
        | SynType.Anon (field0) -> 
            field0 |> ignore
            None
            
        | SynType.WithGlobalConstraints (field0, field1, field2) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            let field1 = this.TryVisitList(field1, this.Visit, fun x -> x.Range)
            if field1.IsSome then field1
            else
            field2 |> ignore
            None
            
        | SynType.HashConstraint (field0, field1) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            field1 |> ignore
            None
            
        | SynType.MeasureDivide (field0, field1, field2) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            let field1 = this.TryVisit(field1, this.Visit, fun x -> x.Range)
            if field1.IsSome then field1
            else
            field2 |> ignore
            None
            
        | SynType.MeasurePower (field0, field1, field2) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            let field1 = this.TryVisit(field1, this.Visit, fun x -> x.Range)
            if field1.IsSome then field1
            else
            field2 |> ignore
            None
            
        | SynType.StaticConstant (field0, field1) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range range0)
            if field0.IsSome then field0
            else
            field1 |> ignore
            None
            
        | SynType.StaticConstantExpr (field0, field1) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            field1 |> ignore
            None
            
        | SynType.StaticConstantNamed (field0, field1, field2) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            let field1 = this.TryVisit(field1, this.Visit, fun x -> x.Range)
            if field1.IsSome then field1
            else
            field2 |> ignore
            None
            
        | SynType.Paren (field0, field1) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            field1 |> ignore
            None
            


    abstract Visit: SynInterfaceImpl -> 'T option
    default this.Visit(node: SynInterfaceImpl) : 'T option =

        match node with
            
        | SynInterfaceImpl.InterfaceImpl (field0, field1, field2) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            let field1 = this.TryVisitList(field1, this.Visit, fun x -> x.Range)
            if field1.IsSome then field1
            else
            field2 |> ignore
            None
            


    abstract Visit: SynSimplePats -> 'T option
    default this.Visit(node: SynSimplePats) : 'T option =

        match node with
            
        | SynSimplePats.SimplePats (field0, field1) -> 
            let field0 = this.TryVisitList(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            field1 |> ignore
            None
            
        | SynSimplePats.Typed (field0, field1, field2) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            let field1 = this.TryVisit(field1, this.Visit, fun x -> x.Range)
            if field1.IsSome then field1
            else
            field2 |> ignore
            None
            


    abstract Visit: SynMatchClause -> 'T option
    default this.Visit(node: SynMatchClause) : 'T option =

        match node with
            
        | SynMatchClause.Clause (field0, field1, field2, field3, field4) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            field1 |> ignore
            let field2 = this.TryVisit(field2, this.Visit, fun x -> x.Range)
            if field2.IsSome then field2
            else
            field3 |> ignore
            field4 |> ignore
            None
            


    abstract Visit: SynIndexerArg -> 'T option
    default this.Visit(node: SynIndexerArg) : 'T option =

        match node with
            
        | SynIndexerArg.Two (field0, field1, field2, field3, field4, field5) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            field1 |> ignore
            let field2 = this.TryVisit(field2, this.Visit, fun x -> x.Range)
            if field2.IsSome then field2
            else
            field3 |> ignore
            field4 |> ignore
            field5 |> ignore
            None
            
        | SynIndexerArg.One (field0, field1, field2) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            field1 |> ignore
            field2 |> ignore
            None
            


    abstract Visit: SynTypar -> 'T option
    default this.Visit(node: SynTypar) : 'T option =

        match node with
            
        | SynTypar.Typar (field0, field1, field2) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            field1 |> ignore
            field2 |> ignore
            None
            


    abstract Visit: SynMemberSig -> 'T option
    default this.Visit(node: SynMemberSig) : 'T option =

        match node with
            
        | SynMemberSig.Member (field0, field1, field2) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            field1 |> ignore
            field2 |> ignore
            None
            
        | SynMemberSig.Interface (field0, field1) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            field1 |> ignore
            None
            
        | SynMemberSig.Inherit (field0, field1) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            field1 |> ignore
            None
            
        | SynMemberSig.ValField (field0, field1) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            field1 |> ignore
            None
            
        | SynMemberSig.NestedType (field0, field1) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            field1 |> ignore
            None
            


    abstract Visit: SynStaticOptimizationConstraint -> 'T option
    default this.Visit(node: SynStaticOptimizationConstraint) : 'T option =

        match node with
            
        | SynStaticOptimizationConstraint.WhenTyparTyconEqualsTycon (field0, field1, field2) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            let field1 = this.TryVisit(field1, this.Visit, fun x -> x.Range)
            if field1.IsSome then field1
            else
            field2 |> ignore
            None
            
        | SynStaticOptimizationConstraint.WhenTyparIsStruct (field0, field1) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            field1 |> ignore
            None
            


    abstract Visit: SynTypeDefnRepr -> 'T option
    default this.Visit(node: SynTypeDefnRepr) : 'T option =

        match node with
            
        | SynTypeDefnRepr.ObjectModel (field0, field1, field2) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            let field1 = this.TryVisitList(field1, this.Visit, fun x -> x.Range)
            if field1.IsSome then field1
            else
            field2 |> ignore
            None
            
        | SynTypeDefnRepr.Simple (field0, field1) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            field1 |> ignore
            None
            
        | SynTypeDefnRepr.Exception (field0) -> 
            this.TryVisit(field0, this.Visit, fun x -> x.Range)
            


    abstract Visit: SynMemberDefn -> 'T option
    default this.Visit(node: SynMemberDefn) : 'T option =

        match node with
            
        | SynMemberDefn.Open (field0, field1) -> 
            let field0 = this.TryVisitList(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            field1 |> ignore
            None
            
        | SynMemberDefn.Member (field0, field1) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            field1 |> ignore
            None
            
        | SynMemberDefn.ImplicitCtor (field0, field1, field2, field3, field4) -> 
            field0 |> ignore
            let field1 = this.TryVisitList(field1, this.Visit, fun x -> x.Range)
            if field1.IsSome then field1
            else
            let field2 = this.TryVisit(field2, this.Visit, fun x -> x.Range)
            if field2.IsSome then field2
            else
            field3 |> ignore
            field4 |> ignore
            None
            
        | SynMemberDefn.ImplicitInherit (field0, field1, field2, field3) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            let field1 = this.TryVisit(field1, this.Visit, fun x -> x.Range)
            if field1.IsSome then field1
            else
            field2 |> ignore
            field3 |> ignore
            None
            
        | SynMemberDefn.LetBindings (field0, field1, field2, field3) -> 
            let field0 = this.TryVisitList(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            field1 |> ignore
            field2 |> ignore
            field3 |> ignore
            None
            
        | SynMemberDefn.AbstractSlot (field0, field1, field2) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            field1 |> ignore
            field2 |> ignore
            None
            
        | SynMemberDefn.Interface (field0, field1, field2) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            field1 |> ignore
            field2 |> ignore
            None
            
        | SynMemberDefn.Inherit (field0, field1, field2) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            field1 |> ignore
            field2 |> ignore
            None
            
        | SynMemberDefn.ValField (field0, field1) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            field1 |> ignore
            None
            
        | SynMemberDefn.NestedType (field0, field1, field2) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            field1 |> ignore
            field2 |> ignore
            None
            
        | SynMemberDefn.AutoProperty (field0, field1, field2, field3, field4, field5, field6, field7, field8, field9, field10) -> 
            let field0 = this.TryVisitList(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            field1 |> ignore
            let field2 = this.TryVisit(field2, this.Visit, fun x -> x.Range)
            if field2.IsSome then field2
            else
            field3 |> ignore
            field4 |> ignore
            field5 |> ignore
            field6 |> ignore
            field7 |> ignore
            let field8 = this.TryVisit(field8, this.Visit, fun x -> x.Range)
            if field8.IsSome then field8
            else
            field9 |> ignore
            field10 |> ignore
            None
            


    abstract Visit: SynExceptionDefnRepr -> 'T option
    default this.Visit(node: SynExceptionDefnRepr) : 'T option =

        match node with
            
        | SynExceptionDefnRepr.SynExceptionDefnRepr (field0, field1, field2, field3, field4, field5) -> 
            let field0 = this.TryVisitList(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            let field1 = this.TryVisit(field1, this.Visit, fun x -> x.Range)
            if field1.IsSome then field1
            else
            field2 |> ignore
            field3 |> ignore
            field4 |> ignore
            field5 |> ignore
            None
            


    abstract Visit: SynValTyparDecls -> 'T option
    default this.Visit(node: SynValTyparDecls) : 'T option =

        match node with
            
        | SynValTyparDecls.SynValTyparDecls (field0, field1, field2) -> 
            let field0 = this.TryVisitList(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            field1 |> ignore
            this.TryVisitList(field2, this.Visit, fun x -> x.Range)
            


    abstract Visit: SynValInfo -> 'T option
    default this.Visit(node: SynValInfo) : 'T option =

        match node with
            
        | SynValInfo.SynValInfo (field0, field1) -> 
            field0 |> ignore
            this.TryVisit(field1, this.Visit, fun x -> x.Range)
            


    abstract Visit: SynTypeDefnSigRepr -> 'T option
    default this.Visit(node: SynTypeDefnSigRepr) : 'T option =

        match node with
            
        | SynTypeDefnSigRepr.ObjectModel (field0, field1, field2) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            let field1 = this.TryVisitList(field1, this.Visit, fun x -> x.Range)
            if field1.IsSome then field1
            else
            field2 |> ignore
            None
            
        | SynTypeDefnSigRepr.Simple (field0, field1) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            field1 |> ignore
            None
            
        | SynTypeDefnSigRepr.Exception (field0) -> 
            this.TryVisit(field0, this.Visit, fun x -> x.Range)
            


    abstract Visit: SynArgPats -> 'T option
    default this.Visit(node: SynArgPats) : 'T option =

        match node with
            
        | SynArgPats.Pats (field0) -> 
            this.TryVisitList(field0, this.Visit, fun x -> x.Range)
            
        | SynArgPats.NamePatPairs (field0, field1) -> 
            field0 |> ignore
            field1 |> ignore
            None
            


    abstract Visit: SynMeasure -> 'T option
    default this.Visit(node: SynMeasure) : 'T option =

        match node with
            
        | SynMeasure.Named (field0, field1) -> 
            let field0 = this.TryVisitList(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            field1 |> ignore
            None
            
        | SynMeasure.Product (field0, field1, field2) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            let field1 = this.TryVisit(field1, this.Visit, fun x -> x.Range)
            if field1.IsSome then field1
            else
            field2 |> ignore
            None
            
        | SynMeasure.Seq (field0, field1) -> 
            let field0 = this.TryVisitList(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            field1 |> ignore
            None
            
        | SynMeasure.Divide (field0, field1, field2) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            let field1 = this.TryVisit(field1, this.Visit, fun x -> x.Range)
            if field1.IsSome then field1
            else
            field2 |> ignore
            None
            
        | SynMeasure.Power (field0, field1, field2) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            let field1 = this.TryVisit(field1, this.Visit, fun x -> x.Range)
            if field1.IsSome then field1
            else
            field2 |> ignore
            None
            
        | SynMeasure.One -> None
        | SynMeasure.Anon (field0) -> 
            field0 |> ignore
            None
            
        | SynMeasure.Var (field0, field1) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            field1 |> ignore
            None
            


    abstract Visit: SynRationalConst -> 'T option
    default this.Visit(node: SynRationalConst) : 'T option =

        match node with
            
        | SynRationalConst.Integer (field0) -> 
            field0 |> ignore
            None
            
        | SynRationalConst.Rational (field0, field1, field2) -> 
            field0 |> ignore
            field1 |> ignore
            field2 |> ignore
            None
            
        | SynRationalConst.Negate (field0) -> 
            this.TryVisit(field0, this.Visit, fun x -> x.Range)
            


    abstract Visit: SynSimplePat -> 'T option
    default this.Visit(node: SynSimplePat) : 'T option =

        match node with
            
        | SynSimplePat.Id (field0, field1, field2, field3, field4, field5) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            field1 |> ignore
            field2 |> ignore
            field3 |> ignore
            field4 |> ignore
            field5 |> ignore
            None
            
        | SynSimplePat.Typed (field0, field1, field2) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            let field1 = this.TryVisit(field1, this.Visit, fun x -> x.Range)
            if field1.IsSome then field1
            else
            field2 |> ignore
            None
            
        | SynSimplePat.Attrib (field0, field1, field2) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            let field1 = this.TryVisitList(field1, this.Visit, fun x -> x.Range)
            if field1.IsSome then field1
            else
            field2 |> ignore
            None
            


    abstract Visit: SynField -> 'T option
    default this.Visit(node: SynField) : 'T option =

        match node with
            
        | SynField.Field (field0, field1, field2, field3, field4, field5, field6, field7) -> 
            let field0 = this.TryVisitList(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            field1 |> ignore
            field2 |> ignore
            let field3 = this.TryVisit(field3, this.Visit, fun x -> x.Range)
            if field3.IsSome then field3
            else
            field4 |> ignore
            field5 |> ignore
            field6 |> ignore
            field7 |> ignore
            None
            


    abstract Visit: SynTypeDefnKind -> 'T option
    default this.Visit(node: SynTypeDefnKind) : 'T option =

        match node with
            
        | SynTypeDefnKind.TyconUnspecified -> None
        | SynTypeDefnKind.TyconClass -> None
        | SynTypeDefnKind.TyconInterface -> None
        | SynTypeDefnKind.TyconStruct -> None
        | SynTypeDefnKind.TyconRecord -> None
        | SynTypeDefnKind.TyconUnion -> None
        | SynTypeDefnKind.TyconAbbrev -> None
        | SynTypeDefnKind.TyconHiddenRepr -> None
        | SynTypeDefnKind.TyconAugmentation -> None
        | SynTypeDefnKind.TyconILAssemblyCode -> None
        | SynTypeDefnKind.TyconDelegate (field0, field1) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            this.TryVisit(field1, this.Visit, fun x -> x.Range)
            


    abstract Visit: SynTypeDefnSimpleRepr -> 'T option
    default this.Visit(node: SynTypeDefnSimpleRepr) : 'T option =

        match node with
            
        | SynTypeDefnSimpleRepr.Union (field0, field1, field2) -> 
            field0 |> ignore
            let field1 = this.TryVisitList(field1, this.Visit, fun x -> x.Range)
            if field1.IsSome then field1
            else
            field2 |> ignore
            None
            
        | SynTypeDefnSimpleRepr.Enum (field0, field1) -> 
            let field0 = this.TryVisitList(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            field1 |> ignore
            None
            
        | SynTypeDefnSimpleRepr.Record (field0, field1, field2) -> 
            field0 |> ignore
            let field1 = this.TryVisitList(field1, this.Visit, fun x -> x.Range)
            if field1.IsSome then field1
            else
            field2 |> ignore
            None
            
        | SynTypeDefnSimpleRepr.General (field0, field1, field2, field3, field4, field5, field6, field7) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            field1 |> ignore
            field2 |> ignore
            let field3 = this.TryVisitList(field3, this.Visit, fun x -> x.Range)
            if field3.IsSome then field3
            else
            field4 |> ignore
            field5 |> ignore
            field6 |> ignore
            field7 |> ignore
            None
            
        | SynTypeDefnSimpleRepr.LibraryOnlyILAssembly (field0, field1) -> 
            field0 |> ignore
            field1 |> ignore
            None
            
        | SynTypeDefnSimpleRepr.TypeAbbrev (field0, field1, field2) -> 
            field0 |> ignore
            let field1 = this.TryVisit(field1, this.Visit, fun x -> x.Range)
            if field1.IsSome then field1
            else
            field2 |> ignore
            None
            
        | SynTypeDefnSimpleRepr.None (field0) -> 
            field0 |> ignore
            None
            
        | SynTypeDefnSimpleRepr.Exception (field0) -> 
            this.TryVisit(field0, this.Visit, fun x -> x.Range)
            


    abstract Visit: SynUnionCase -> 'T option
    default this.Visit(node: SynUnionCase) : 'T option =

        match node with
            
        | SynUnionCase.UnionCase (field0, field1, field2, field3, field4, field5) -> 
            let field0 = this.TryVisitList(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            let field1 = this.TryVisit(field1, this.Visit, fun x -> x.Range)
            if field1.IsSome then field1
            else
            let field2 = this.TryVisit(field2, this.Visit, fun x -> x.Range)
            if field2.IsSome then field2
            else
            field3 |> ignore
            field4 |> ignore
            field5 |> ignore
            None
            


    abstract Visit: SynArgInfo -> 'T option
    default this.Visit(node: SynArgInfo) : 'T option =

        match node with
            
        | SynArgInfo.SynArgInfo (field0, field1, field2) -> 
            let field0 = this.TryVisitList(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            field1 |> ignore
            field2 |> ignore
            None
            


    abstract Visit: SynEnumCase -> 'T option
    default this.Visit(node: SynEnumCase) : 'T option =

        match node with
            
        | SynEnumCase.EnumCase (field0, field1, field2, field3, field4) -> 
            let field0 = this.TryVisitList(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            let field1 = this.TryVisit(field1, this.Visit, fun x -> x.Range)
            if field1.IsSome then field1
            else
            let field2 = this.TryVisit(field2, this.Visit, fun x -> x.Range range0)
            if field2.IsSome then field2
            else
            field3 |> ignore
            field4 |> ignore
            None
            


    abstract Visit: SynUnionCaseType -> 'T option
    default this.Visit(node: SynUnionCaseType) : 'T option =

        match node with
            
        | SynUnionCaseType.UnionCaseFields (field0) -> 
            this.TryVisitList(field0, this.Visit, fun x -> x.Range)
            
        | SynUnionCaseType.UnionCaseFullType (field0, field1) -> 
            let field0 = this.TryVisit(field0, this.Visit, fun x -> x.Range)
            if field0.IsSome then field0
            else
            this.TryVisit(field1, this.Visit, fun x -> x.Range)
            

