module FSharp.Compiler.SyntaxTreeVisitor
        
open FSharp.Compiler.AbstractIL.Internal.Library
open FSharp.Compiler.SyntaxTree
open FSharp.Compiler.Range
open FSharp.Compiler.SyntaxTreeExtendedRanges

[<AbstractClass>]
type SyntaxTreeVisitor () =
        

    abstract Visit: ParsedInput -> unit
    default this.Visit(node: ParsedInput) : unit =
        match node with         
        | ParsedInput.ImplFile implFileInput -> 
            this.Visit implFileInput
            
        | ParsedInput.SigFile sigFileInput -> 
            this.Visit sigFileInput          

    abstract Visit: ParsedImplFileInput -> unit
    default this.Visit(node: ParsedImplFileInput) : unit =
        match node with     
        | ParsedImplFileInput.ParsedImplFileInput (_, _, _, _, hashDirectives, modules, _) -> 
            hashDirectives |> List.iter this.Visit
            modules |> List.iter this.Visit

    abstract Visit: ParsedSigFileInput -> unit
    default this.Visit(node: ParsedSigFileInput) : unit =
        match node with
        | ParsedSigFileInput.ParsedSigFileInput (field0, field1, field2, field3, field4) -> 
            field0 |> ignore
            field1 |> ignore
            field2 |> ignore
            field3 |> List.iter this.Visit
            field4 |> List.iter this.Visit
            

    abstract Visit: ParsedHashDirective -> unit
    default this.Visit(node: ParsedHashDirective) : unit =

        match node with
            
        | ParsedHashDirective.ParsedHashDirective (field0, field1, field2) -> 
            field0 |> ignore
            field1 |> ignore
            field2 |> ignore
            

    abstract Visit: SynModuleOrNamespace -> unit
    default this.Visit(node: SynModuleOrNamespace) : unit =

        match node with
            
        | SynModuleOrNamespace.SynModuleOrNamespace (field0, field1, field2, field3, field4, field5, field6, field7) -> 
            field0 |> List.iter this.Visit
            field1 |> ignore
            this.Visit field2
            field3 |> List.iter this.Visit
            field4 |> ignore
            field5 |> List.iter this.Visit
            field6 |> ignore
            field7 |> ignore
            

    abstract Visit: SynModuleOrNamespaceSig -> unit
    default this.Visit(node: SynModuleOrNamespaceSig) : unit =

        match node with
            
        | SynModuleOrNamespaceSig.SynModuleOrNamespaceSig (field0, field1, field2, field3, field4, field5, field6, field7) -> 
            field0 |> List.iter this.Visit
            field1 |> ignore
            this.Visit field2
            field3 |> List.iter this.Visit
            field4 |> ignore
            field5 |> List.iter this.Visit
            field6 |> ignore
            field7 |> ignore
            

    abstract Visit: Ident -> unit
    default this.Visit(node: Ident) : unit =
        ()

    abstract Visit: SynModuleOrNamespaceKind -> unit
    default this.Visit(node: SynModuleOrNamespaceKind) : unit =

        match node with
            
        | SynModuleOrNamespaceKind.NamedModule -> ()
        | SynModuleOrNamespaceKind.AnonModule -> ()
        | SynModuleOrNamespaceKind.DeclaredNamespace -> ()
        | SynModuleOrNamespaceKind.GlobalNamespace -> ()

    abstract Visit: SynModuleDecl -> unit
    default this.Visit(node: SynModuleDecl) : unit =

        match node with
            
        | SynModuleDecl.ModuleAbbrev (field0, field1, field2) -> 
            this.Visit field0
            field1 |> List.iter this.Visit
            field2 |> ignore
            
        | SynModuleDecl.NestedModule (field0, field1, field2, field3, field4) -> 
            this.Visit field0
            field1 |> ignore
            field2 |> List.iter this.Visit
            field3 |> ignore
            field4 |> ignore
            
        | SynModuleDecl.Let (field0, field1, field2) -> 
            field0 |> ignore
            field1 |> List.iter this.Visit
            field2 |> ignore
            
        | SynModuleDecl.DoExpr (field0, field1, field2) -> 
            field0 |> ignore
            this.Visit field1
            field2 |> ignore
            
        | SynModuleDecl.Types (field0, field1) -> 
            field0 |> List.iter this.Visit
            field1 |> ignore
            
        | SynModuleDecl.Exception (field0, field1) -> 
            this.Visit field0
            field1 |> ignore
            
        | SynModuleDecl.Open (field0, field1) -> 
            field0 |> ignore
            field1 |> ignore
            
        | SynModuleDecl.Attributes (field0, field1) -> 
            field0 |> List.iter this.Visit
            field1 |> ignore
            
        | SynModuleDecl.HashDirective (field0, field1) -> 
            this.Visit field0
            field1 |> ignore
            
        | SynModuleDecl.NamespaceFragment (field0) -> 
            this.Visit field0
            

    abstract Visit: SynAttributeList -> unit
    default this.Visit(node: SynAttributeList) : unit =
        ()

    abstract Visit: SynModuleSigDecl -> unit
    default this.Visit(node: SynModuleSigDecl) : unit =

        match node with
            
        | SynModuleSigDecl.ModuleAbbrev (field0, field1, field2) -> 
            this.Visit field0
            field1 |> List.iter this.Visit
            field2 |> ignore
            
        | SynModuleSigDecl.NestedModule (field0, field1, field2, field3) -> 
            this.Visit field0
            field1 |> ignore
            field2 |> List.iter this.Visit
            field3 |> ignore
            
        | SynModuleSigDecl.Val (field0, field1) -> 
            this.Visit field0
            field1 |> ignore
            
        | SynModuleSigDecl.Types (field0, field1) -> 
            field0 |> List.iter this.Visit
            field1 |> ignore
            
        | SynModuleSigDecl.Exception (field0, field1) -> 
            this.Visit field0
            field1 |> ignore
            
        | SynModuleSigDecl.Open (field0, field1) -> 
            field0 |> List.iter this.Visit
            field1 |> ignore
            
        | SynModuleSigDecl.HashDirective (field0, field1) -> 
            this.Visit field0
            field1 |> ignore
            
        | SynModuleSigDecl.NamespaceFragment (field0) -> 
            this.Visit field0
            

    abstract Visit: SynComponentInfo -> unit
    default this.Visit(node: SynComponentInfo) : unit =

        match node with
            
        | SynComponentInfo.ComponentInfo (field0, field1, field2, field3, field4, field5, field6, field7) -> 
            field0 |> List.iter this.Visit
            field1 |> List.iter this.Visit
            field2 |> List.iter this.Visit
            field3 |> List.iter this.Visit
            field4 |> ignore
            field5 |> ignore
            field6 |> ignore
            field7 |> ignore
            

    abstract Visit: SynBinding -> unit
    default this.Visit(node: SynBinding) : unit =

        match node with
            
        | SynBinding.Binding (field0, field1, field2, field3, field4, field5, field6, field7, field8, field9, field10, field11) -> 
            field0 |> ignore
            this.Visit field1
            field2 |> ignore
            field3 |> ignore
            field4 |> List.iter this.Visit
            field5 |> ignore
            this.Visit field6
            this.Visit field7
            field8 |> ignore
            this.Visit field9
            field10 |> ignore
            field11 |> ignore
            

    abstract Visit: SynExpr -> unit
    default this.Visit(node: SynExpr) : unit =

        match node with
            
        | SynExpr.Paren (field0, field1, field2, field3) -> 
            this.Visit field0
            field1 |> ignore
            field2 |> ignore
            field3 |> ignore
            
        | SynExpr.Quote (field0, field1, field2, field3, field4) -> 
            this.Visit field0
            field1 |> ignore
            this.Visit field2
            field3 |> ignore
            field4 |> ignore
            
        | SynExpr.Const (field0, field1) -> 
            this.Visit field0
            field1 |> ignore
            
        | SynExpr.Typed (field0, field1, field2) -> 
            this.Visit field0
            this.Visit field1
            field2 |> ignore
            
        | SynExpr.Tuple (field0, field1, field2, field3) -> 
            field0 |> ignore
            field1 |> List.iter this.Visit
            field2 |> ignore
            field3 |> ignore
            
        | SynExpr.AnonRecd (field0, field1, field2, field3) -> 
            field0 |> ignore
            field1 |> ignore
            field2 |> ignore
            field3 |> ignore
            
        | SynExpr.ArrayOrList (field0, field1, field2) -> 
            field0 |> ignore
            field1 |> List.iter this.Visit
            field2 |> ignore
            
        | SynExpr.Record (field0, field1, field2, field3) -> 
            field0 |> ignore
            field1 |> ignore
            field2 |> ignore
            field3 |> ignore
            
        | SynExpr.New (field0, field1, field2, field3) -> 
            field0 |> ignore
            this.Visit field1
            this.Visit field2
            field3 |> ignore
            
        | SynExpr.ObjExpr (field0, field1, field2, field3, field4, field5) -> 
            this.Visit field0
            field1 |> ignore
            field2 |> List.iter this.Visit
            field3 |> List.iter this.Visit
            field4 |> ignore
            field5 |> ignore
            
        | SynExpr.While (field0, field1, field2, field3) -> 
            field0 |> ignore
            this.Visit field1
            this.Visit field2
            field3 |> ignore
            
        | SynExpr.For (field0, field1, field2, field3, field4, field5, field6) -> 
            field0 |> ignore
            this.Visit field1
            this.Visit field2
            field3 |> ignore
            this.Visit field4
            this.Visit field5
            field6 |> ignore
            
        | SynExpr.ForEach (field0, field1, field2, field3, field4, field5, field6) -> 
            field0 |> ignore
            field1 |> ignore
            field2 |> ignore
            this.Visit field3
            this.Visit field4
            this.Visit field5
            field6 |> ignore
            
        | SynExpr.ArrayOrListOfSeqExpr (field0, field1, field2) -> 
            field0 |> ignore
            this.Visit field1
            field2 |> ignore
            
        | SynExpr.CompExpr (field0, field1, field2, field3) -> 
            field0 |> ignore
            field1 |> ignore
            this.Visit field2
            field3 |> ignore
            
        | SynExpr.Lambda (field0, field1, field2, field3, field4) -> 
            field0 |> ignore
            field1 |> ignore
            this.Visit field2
            this.Visit field3
            field4 |> ignore
            
        | SynExpr.MatchLambda (field0, field1, field2, field3, field4) -> 
            field0 |> ignore
            field1 |> ignore
            field2 |> List.iter this.Visit
            field3 |> ignore
            field4 |> ignore
            
        | SynExpr.Match (field0, field1, field2, field3) -> 
            field0 |> ignore
            this.Visit field1
            field2 |> List.iter this.Visit
            field3 |> ignore
            
        | SynExpr.Do (field0, field1) -> 
            this.Visit field0
            field1 |> ignore
            
        | SynExpr.Assert (field0, field1) -> 
            this.Visit field0
            field1 |> ignore
            
        | SynExpr.App (field0, field1, field2, field3, field4) -> 
            field0 |> ignore
            field1 |> ignore
            this.Visit field2
            this.Visit field3
            field4 |> ignore
            
        | SynExpr.TypeApp (field0, field1, field2, field3, field4, field5, field6) -> 
            this.Visit field0
            field1 |> ignore
            field2 |> List.iter this.Visit
            field3 |> ignore
            field4 |> ignore
            field5 |> ignore
            field6 |> ignore
            
        | SynExpr.LetOrUse (field0, field1, field2, field3, field4) -> 
            field0 |> ignore
            field1 |> ignore
            field2 |> List.iter this.Visit
            this.Visit field3
            field4 |> ignore
            
        | SynExpr.TryWith (field0, field1, field2, field3, field4, field5, field6) -> 
            this.Visit field0
            field1 |> ignore
            field2 |> List.iter this.Visit
            field3 |> ignore
            field4 |> ignore
            field5 |> ignore
            field6 |> ignore
            
        | SynExpr.TryFinally (field0, field1, field2, field3, field4) -> 
            this.Visit field0
            this.Visit field1
            field2 |> ignore
            field3 |> ignore
            field4 |> ignore
            
        | SynExpr.Lazy (field0, field1) -> 
            this.Visit field0
            field1 |> ignore
            
        | SynExpr.Sequential (field0, field1, field2, field3, field4) -> 
            field0 |> ignore
            field1 |> ignore
            this.Visit field2
            this.Visit field3
            field4 |> ignore
            
        | SynExpr.IfThenElse (field0, field1, field2, field3, field4, field5, field6) -> 
            this.Visit field0
            this.Visit field1
            field2 |> ignore
            field3 |> ignore
            field4 |> ignore
            field5 |> ignore
            field6 |> ignore
            
        | SynExpr.Ident (field0) -> 
            this.Visit field0
            
        | SynExpr.LongIdent (field0, field1, field2, field3) -> 
            field0 |> ignore
            field1 |> ignore
            field2 |> ignore
            field3 |> ignore
            
        | SynExpr.LongIdentSet (field0, field1, field2) -> 
            field0 |> ignore
            this.Visit field1
            field2 |> ignore
            
        | SynExpr.DotGet (field0, field1, field2, field3) -> 
            this.Visit field0
            field1 |> ignore
            field2 |> ignore
            field3 |> ignore
            
        | SynExpr.DotSet (field0, field1, field2, field3) -> 
            this.Visit field0
            field1 |> ignore
            this.Visit field2
            field3 |> ignore
            
        | SynExpr.Set (field0, field1, field2) -> 
            this.Visit field0
            this.Visit field1
            field2 |> ignore
            
        | SynExpr.DotIndexedGet (field0, field1, field2, field3) -> 
            this.Visit field0
            field1 |> List.iter this.Visit
            field2 |> ignore
            field3 |> ignore
            
        | SynExpr.DotIndexedSet (field0, field1, field2, field3, field4, field5) -> 
            this.Visit field0
            field1 |> List.iter this.Visit
            this.Visit field2
            field3 |> ignore
            field4 |> ignore
            field5 |> ignore
            
        | SynExpr.NamedIndexedPropertySet (field0, field1, field2, field3) -> 
            field0 |> ignore
            this.Visit field1
            this.Visit field2
            field3 |> ignore
            
        | SynExpr.DotNamedIndexedPropertySet (field0, field1, field2, field3, field4) -> 
            this.Visit field0
            field1 |> ignore
            this.Visit field2
            this.Visit field3
            field4 |> ignore
            
        | SynExpr.TypeTest (field0, field1, field2) -> 
            this.Visit field0
            this.Visit field1
            field2 |> ignore
            
        | SynExpr.Upcast (field0, field1, field2) -> 
            this.Visit field0
            this.Visit field1
            field2 |> ignore
            
        | SynExpr.Downcast (field0, field1, field2) -> 
            this.Visit field0
            this.Visit field1
            field2 |> ignore
            
        | SynExpr.InferredUpcast (field0, field1) -> 
            this.Visit field0
            field1 |> ignore
            
        | SynExpr.InferredDowncast (field0, field1) -> 
            this.Visit field0
            field1 |> ignore
            
        | SynExpr.Null (field0) -> 
            field0 |> ignore
            
        | SynExpr.AddressOf (field0, field1, field2, field3) -> 
            field0 |> ignore
            this.Visit field1
            field2 |> ignore
            field3 |> ignore
            
        | SynExpr.TraitCall (field0, field1, field2, field3) -> 
            field0 |> List.iter this.Visit
            this.Visit field1
            this.Visit field2
            field3 |> ignore
            
        | SynExpr.JoinIn (field0, field1, field2, field3) -> 
            this.Visit field0
            field1 |> ignore
            this.Visit field2
            field3 |> ignore
            
        | SynExpr.ImplicitZero (field0) -> 
            field0 |> ignore
            
        | SynExpr.SequentialOrImplicitYield (field0, field1, field2, field3, field4) -> 
            field0 |> ignore
            this.Visit field1
            this.Visit field2
            this.Visit field3
            field4 |> ignore
            
        | SynExpr.YieldOrReturn (field0, field1, field2) -> 
            field0 |> ignore
            this.Visit field1
            field2 |> ignore
            
        | SynExpr.YieldOrReturnFrom (field0, field1, field2) -> 
            field0 |> ignore
            this.Visit field1
            field2 |> ignore
            
        | SynExpr.LetOrUseBang (field0, field1, field2, field3, field4, field5, field6, field7) -> 
            field0 |> ignore
            field1 |> ignore
            field2 |> ignore
            this.Visit field3
            this.Visit field4
            field5 |> ignore
            this.Visit field6
            field7 |> ignore
            
        | SynExpr.MatchBang (field0, field1, field2, field3) -> 
            field0 |> ignore
            this.Visit field1
            field2 |> List.iter this.Visit
            field3 |> ignore
            
        | SynExpr.DoBang (field0, field1) -> 
            this.Visit field0
            field1 |> ignore
            
        | SynExpr.LibraryOnlyILAssembly (field0, field1, field2, field3, field4) -> 
            field0 |> ignore
            field1 |> List.iter this.Visit
            field2 |> List.iter this.Visit
            field3 |> List.iter this.Visit
            field4 |> ignore
            
        | SynExpr.LibraryOnlyStaticOptimization (field0, field1, field2, field3) -> 
            field0 |> List.iter this.Visit
            this.Visit field1
            this.Visit field2
            field3 |> ignore
            
        | SynExpr.LibraryOnlyUnionCaseFieldGet (field0, field1, field2, field3) -> 
            this.Visit field0
            field1 |> List.iter this.Visit
            field2 |> ignore
            field3 |> ignore
            
        | SynExpr.LibraryOnlyUnionCaseFieldSet (field0, field1, field2, field3, field4) -> 
            this.Visit field0
            field1 |> List.iter this.Visit
            field2 |> ignore
            this.Visit field3
            field4 |> ignore
            
        | SynExpr.ArbitraryAfterError (field0, field1) -> 
            field0 |> ignore
            field1 |> ignore
            
        | SynExpr.FromParseError (field0, field1) -> 
            this.Visit field0
            field1 |> ignore
            
        | SynExpr.DiscardAfterMissingQualificationAfterDot (field0, field1) -> 
            this.Visit field0
            field1 |> ignore
            
        | SynExpr.Fixed (field0, field1) -> 
            this.Visit field0
            field1 |> ignore
            

    abstract Visit: SynTypeDefn -> unit
    default this.Visit(node: SynTypeDefn) : unit =

        match node with
            
        | SynTypeDefn.TypeDefn (field0, field1, field2, field3) -> 
            this.Visit field0
            this.Visit field1
            field2 |> List.iter this.Visit
            field3 |> ignore
            

    abstract Visit: SynExceptionDefn -> unit
    default this.Visit(node: SynExceptionDefn) : unit =

        match node with
            
        | SynExceptionDefn.SynExceptionDefn (field0, field1, field2) -> 
            this.Visit field0
            field1 |> List.iter this.Visit
            field2 |> ignore
            

    abstract Visit: SynValSig -> unit
    default this.Visit(node: SynValSig) : unit =

        match node with
            
        | SynValSig.ValSpfn (field0, field1, field2, field3, field4, field5, field6, field7, field8, field9, field10) -> 
            field0 |> List.iter this.Visit
            this.Visit field1
            this.Visit field2
            this.Visit field3
            this.Visit field4
            field5 |> ignore
            field6 |> ignore
            field7 |> ignore
            field8 |> ignore
            field9 |> ignore
            field10 |> ignore
            

    abstract Visit: SynTypeDefnSig -> unit
    default this.Visit(node: SynTypeDefnSig) : unit =

        match node with
            
        | SynTypeDefnSig.TypeDefnSig (field0, field1, field2, field3) -> 
            this.Visit field0
            this.Visit field1
            field2 |> List.iter this.Visit
            field3 |> ignore
            

    abstract Visit: SynExceptionSig -> unit
    default this.Visit(node: SynExceptionSig) : unit =

        match node with
            
        | SynExceptionSig.SynExceptionSig (field0, field1, field2) -> 
            this.Visit field0
            field1 |> List.iter this.Visit
            field2 |> ignore
            

    abstract Visit: SynTyparDecl -> unit
    default this.Visit(node: SynTyparDecl) : unit =

        match node with
            
        | SynTyparDecl.TyparDecl (field0, field1) -> 
            field0 |> List.iter this.Visit
            this.Visit field1
            

    abstract Visit: SynTypeConstraint -> unit
    default this.Visit(node: SynTypeConstraint) : unit =

        match node with
            
        | SynTypeConstraint.WhereTyparIsValueType (field0, field1) -> 
            this.Visit field0
            field1 |> ignore
            
        | SynTypeConstraint.WhereTyparIsReferenceType (field0, field1) -> 
            this.Visit field0
            field1 |> ignore
            
        | SynTypeConstraint.WhereTyparIsUnmanaged (field0, field1) -> 
            this.Visit field0
            field1 |> ignore
            
        | SynTypeConstraint.WhereTyparSupportsNull (field0, field1) -> 
            this.Visit field0
            field1 |> ignore
            
        | SynTypeConstraint.WhereTyparIsComparable (field0, field1) -> 
            this.Visit field0
            field1 |> ignore
            
        | SynTypeConstraint.WhereTyparIsEquatable (field0, field1) -> 
            this.Visit field0
            field1 |> ignore
            
        | SynTypeConstraint.WhereTyparDefaultsToType (field0, field1, field2) -> 
            this.Visit field0
            this.Visit field1
            field2 |> ignore
            
        | SynTypeConstraint.WhereTyparSubtypeOfType (field0, field1, field2) -> 
            this.Visit field0
            this.Visit field1
            field2 |> ignore
            
        | SynTypeConstraint.WhereTyparSupportsMember (field0, field1, field2) -> 
            field0 |> List.iter this.Visit
            this.Visit field1
            field2 |> ignore
            
        | SynTypeConstraint.WhereTyparIsEnum (field0, field1, field2) -> 
            this.Visit field0
            field1 |> List.iter this.Visit
            field2 |> ignore
            
        | SynTypeConstraint.WhereTyparIsDelegate (field0, field1, field2) -> 
            this.Visit field0
            field1 |> List.iter this.Visit
            field2 |> ignore
            

    abstract Visit: SynBindingKind -> unit
    default this.Visit(node: SynBindingKind) : unit =

        match node with
            
        | SynBindingKind.StandaloneExpression -> ()
        | SynBindingKind.NormalBinding -> ()
        | SynBindingKind.DoBinding -> ()

    abstract Visit: SynValData -> unit
    default this.Visit(node: SynValData) : unit =

        match node with
            
        | SynValData.SynValData (field0, field1, field2) -> 
            field0 |> ignore
            this.Visit field1
            field2 |> ignore
            

    abstract Visit: SynPat -> unit
    default this.Visit(node: SynPat) : unit =

        match node with
            
        | SynPat.Const (field0, field1) -> 
            this.Visit field0
            field1 |> ignore
            
        | SynPat.Wild (field0) -> 
            field0 |> ignore
            
        | SynPat.Named (field0, field1, field2, field3, field4) -> 
            this.Visit field0
            this.Visit field1
            field2 |> ignore
            field3 |> ignore
            field4 |> ignore
            
        | SynPat.Typed (field0, field1, field2) -> 
            this.Visit field0
            this.Visit field1
            field2 |> ignore
            
        | SynPat.Attrib (field0, field1, field2) -> 
            this.Visit field0
            field1 |> List.iter this.Visit
            field2 |> ignore
            
        | SynPat.Or (field0, field1, field2) -> 
            this.Visit field0
            this.Visit field1
            field2 |> ignore
            
        | SynPat.Ands (field0, field1) -> 
            field0 |> List.iter this.Visit
            field1 |> ignore
            
        | SynPat.LongIdent (field0, field1, field2, field3, field4, field5) -> 
            field0 |> ignore
            field1 |> ignore
            field2 |> ignore
            this.Visit field3
            field4 |> ignore
            field5 |> ignore
            
        | SynPat.Tuple (field0, field1, field2) -> 
            field0 |> ignore
            field1 |> List.iter this.Visit
            field2 |> ignore
            
        | SynPat.Paren (field0, field1) -> 
            this.Visit field0
            field1 |> ignore
            
        | SynPat.ArrayOrList (field0, field1, field2) -> 
            field0 |> ignore
            field1 |> List.iter this.Visit
            field2 |> ignore
            
        | SynPat.Record (field0, field1) -> 
            field0 |> ignore
            field1 |> ignore
            
        | SynPat.Null (field0) -> 
            field0 |> ignore
            
        | SynPat.OptionalVal (field0, field1) -> 
            this.Visit field0
            field1 |> ignore
            
        | SynPat.IsInst (field0, field1) -> 
            this.Visit field0
            field1 |> ignore
            
        | SynPat.QuoteExpr (field0, field1) -> 
            this.Visit field0
            field1 |> ignore
            
        | SynPat.DeprecatedCharRange (field0, field1, field2) -> 
            field0 |> ignore
            field1 |> ignore
            field2 |> ignore
            
        | SynPat.InstanceMember (field0, field1, field2, field3, field4) -> 
            this.Visit field0
            this.Visit field1
            field2 |> ignore
            field3 |> ignore
            field4 |> ignore
            
        | SynPat.FromParseError (field0, field1) -> 
            this.Visit field0
            field1 |> ignore
            

    abstract Visit: SynConst -> unit
    default this.Visit(node: SynConst) : unit =

        match node with
            
        | SynConst.Unit -> ()
        | SynConst.Bool (field0) -> 
            field0 |> ignore
            
        | SynConst.SByte (field0) -> 
            field0 |> ignore
            
        | SynConst.Byte (field0) -> 
            field0 |> ignore
            
        | SynConst.Int16 (field0) -> 
            field0 |> ignore
            
        | SynConst.UInt16 (field0) -> 
            field0 |> ignore
            
        | SynConst.Int32 (field0) -> 
            field0 |> ignore
            
        | SynConst.UInt32 (field0) -> 
            field0 |> ignore
            
        | SynConst.Int64 (field0) -> 
            field0 |> ignore
            
        | SynConst.UInt64 (field0) -> 
            field0 |> ignore
            
        | SynConst.IntPtr (field0) -> 
            field0 |> ignore
            
        | SynConst.UIntPtr (field0) -> 
            field0 |> ignore
            
        | SynConst.Single (field0) -> 
            field0 |> ignore
            
        | SynConst.Double (field0) -> 
            field0 |> ignore
            
        | SynConst.Char (field0) -> 
            field0 |> ignore
            
        | SynConst.Decimal (field0) -> 
            field0 |> ignore
            
        | SynConst.UserNum (field0, field1) -> 
            field0 |> ignore
            field1 |> ignore
            
        | SynConst.String (field0, field1) -> 
            field0 |> ignore
            field1 |> ignore
            
        | SynConst.Bytes (field0, field1) -> 
            field0 |> ignore
            field1 |> ignore
            
        | SynConst.UInt16s (field0) -> 
            field0 |> ignore
            
        | SynConst.Measure (field0, field1) -> 
            this.Visit field0
            this.Visit field1
            

    abstract Visit: SynType -> unit
    default this.Visit(node: SynType) : unit =

        match node with
            
        | SynType.LongIdent (field0) -> 
            field0 |> ignore
            
        | SynType.App (field0, field1, field2, field3, field4, field5, field6) -> 
            this.Visit field0
            field1 |> ignore
            field2 |> List.iter this.Visit
            field3 |> ignore
            field4 |> ignore
            field5 |> ignore
            field6 |> ignore
            
        | SynType.LongIdentApp (field0, field1, field2, field3, field4, field5, field6) -> 
            this.Visit field0
            field1 |> ignore
            field2 |> ignore
            field3 |> List.iter this.Visit
            field4 |> ignore
            field5 |> ignore
            field6 |> ignore
            
        | SynType.Tuple (field0, field1, field2) -> 
            field0 |> ignore
            field1 |> ignore
            field2 |> ignore
            
        | SynType.AnonRecd (field0, field1, field2) -> 
            field0 |> ignore
            field1 |> ignore
            field2 |> ignore
            
        | SynType.Array (field0, field1, field2) -> 
            field0 |> ignore
            this.Visit field1
            field2 |> ignore
            
        | SynType.Fun (field0, field1, field2) -> 
            this.Visit field0
            this.Visit field1
            field2 |> ignore
            
        | SynType.Var (field0, field1) -> 
            this.Visit field0
            field1 |> ignore
            
        | SynType.Anon (field0) -> 
            field0 |> ignore
            
        | SynType.WithGlobalConstraints (field0, field1, field2) -> 
            this.Visit field0
            field1 |> List.iter this.Visit
            field2 |> ignore
            
        | SynType.HashConstraint (field0, field1) -> 
            this.Visit field0
            field1 |> ignore
            
        | SynType.MeasureDivide (field0, field1, field2) -> 
            this.Visit field0
            this.Visit field1
            field2 |> ignore
            
        | SynType.MeasurePower (field0, field1, field2) -> 
            this.Visit field0
            this.Visit field1
            field2 |> ignore
            
        | SynType.StaticConstant (field0, field1) -> 
            this.Visit field0
            field1 |> ignore
            
        | SynType.StaticConstantExpr (field0, field1) -> 
            this.Visit field0
            field1 |> ignore
            
        | SynType.StaticConstantNamed (field0, field1, field2) -> 
            this.Visit field0
            this.Visit field1
            field2 |> ignore
            
        | SynType.Paren (field0, field1) -> 
            this.Visit field0
            field1 |> ignore
            

    abstract Visit: SynInterfaceImpl -> unit
    default this.Visit(node: SynInterfaceImpl) : unit =

        match node with
            
        | SynInterfaceImpl.InterfaceImpl (field0, field1, field2) -> 
            this.Visit field0
            field1 |> List.iter this.Visit
            field2 |> ignore
            

    abstract Visit: SynSimplePats -> unit
    default this.Visit(node: SynSimplePats) : unit =

        match node with
            
        | SynSimplePats.SimplePats (field0, field1) -> 
            field0 |> List.iter this.Visit
            field1 |> ignore
            
        | SynSimplePats.Typed (field0, field1, field2) -> 
            this.Visit field0
            this.Visit field1
            field2 |> ignore
            

    abstract Visit: SynMatchClause -> unit
    default this.Visit(node: SynMatchClause) : unit =

        match node with
            
        | SynMatchClause.Clause (field0, field1, field2, field3, field4) -> 
            this.Visit field0
            field1 |> ignore
            this.Visit field2
            field3 |> ignore
            field4 |> ignore
            

    abstract Visit: SynIndexerArg -> unit
    default this.Visit(node: SynIndexerArg) : unit =

        match node with
            
        | SynIndexerArg.Two (field0, field1, field2, field3, field4, field5) -> 
            this.Visit field0
            field1 |> ignore
            this.Visit field2
            field3 |> ignore
            field4 |> ignore
            field5 |> ignore
            
        | SynIndexerArg.One (field0, field1, field2) -> 
            this.Visit field0
            field1 |> ignore
            field2 |> ignore
            

    abstract Visit: SynTypar -> unit
    default this.Visit(node: SynTypar) : unit =

        match node with
            
        | SynTypar.Typar (field0, field1, field2) -> 
            this.Visit field0
            field1 |> ignore
            field2 |> ignore
            

    abstract Visit: SynMemberSig -> unit
    default this.Visit(node: SynMemberSig) : unit =

        match node with
            
        | SynMemberSig.Member (field0, field1, field2) -> 
            this.Visit field0
            field1 |> ignore
            field2 |> ignore
            
        | SynMemberSig.Interface (field0, field1) -> 
            this.Visit field0
            field1 |> ignore
            
        | SynMemberSig.Inherit (field0, field1) -> 
            this.Visit field0
            field1 |> ignore
            
        | SynMemberSig.ValField (field0, field1) -> 
            this.Visit field0
            field1 |> ignore
            
        | SynMemberSig.NestedType (field0, field1) -> 
            this.Visit field0
            field1 |> ignore
            

    abstract Visit: SynStaticOptimizationConstraint -> unit
    default this.Visit(node: SynStaticOptimizationConstraint) : unit =

        match node with
            
        | SynStaticOptimizationConstraint.WhenTyparTyconEqualsTycon (field0, field1, field2) -> 
            this.Visit field0
            this.Visit field1
            field2 |> ignore
            
        | SynStaticOptimizationConstraint.WhenTyparIsStruct (field0, field1) -> 
            this.Visit field0
            field1 |> ignore
            

    abstract Visit: SynTypeDefnRepr -> unit
    default this.Visit(node: SynTypeDefnRepr) : unit =

        match node with
            
        | SynTypeDefnRepr.ObjectModel (field0, field1, field2) -> 
            this.Visit field0
            field1 |> List.iter this.Visit
            field2 |> ignore
            
        | SynTypeDefnRepr.Simple (field0, field1) -> 
            this.Visit field0
            field1 |> ignore
            
        | SynTypeDefnRepr.Exception (field0) -> 
            this.Visit field0
            

    abstract Visit: SynMemberDefn -> unit
    default this.Visit(node: SynMemberDefn) : unit =

        match node with
            
        | SynMemberDefn.Open (field0, field1) -> 
            field0 |> List.iter this.Visit
            field1 |> ignore
            
        | SynMemberDefn.Member (field0, field1) -> 
            this.Visit field0
            field1 |> ignore
            
        | SynMemberDefn.ImplicitCtor (field0, field1, field2, field3, field4) -> 
            field0 |> ignore
            field1 |> List.iter this.Visit
            this.Visit field2
            field3 |> ignore
            field4 |> ignore
            
        | SynMemberDefn.ImplicitInherit (field0, field1, field2, field3) -> 
            this.Visit field0
            this.Visit field1
            field2 |> ignore
            field3 |> ignore
            
        | SynMemberDefn.LetBindings (field0, field1, field2, field3) -> 
            field0 |> List.iter this.Visit
            field1 |> ignore
            field2 |> ignore
            field3 |> ignore
            
        | SynMemberDefn.AbstractSlot (field0, field1, field2) -> 
            this.Visit field0
            field1 |> ignore
            field2 |> ignore
            
        | SynMemberDefn.Interface (field0, field1, field2) -> 
            this.Visit field0
            field1 |> ignore
            field2 |> ignore
            
        | SynMemberDefn.Inherit (field0, field1, field2) -> 
            this.Visit field0
            field1 |> ignore
            field2 |> ignore
            
        | SynMemberDefn.ValField (field0, field1) -> 
            this.Visit field0
            field1 |> ignore
            
        | SynMemberDefn.NestedType (field0, field1, field2) -> 
            this.Visit field0
            field1 |> ignore
            field2 |> ignore
            
        | SynMemberDefn.AutoProperty (field0, field1, field2, field3, field4, field5, field6, field7, field8, field9, field10) -> 
            field0 |> List.iter this.Visit
            field1 |> ignore
            this.Visit field2
            field3 |> ignore
            field4 |> ignore
            field5 |> ignore
            field6 |> ignore
            field7 |> ignore
            this.Visit field8
            field9 |> ignore
            field10 |> ignore
            

    abstract Visit: SynExceptionDefnRepr -> unit
    default this.Visit(node: SynExceptionDefnRepr) : unit =

        match node with
            
        | SynExceptionDefnRepr.SynExceptionDefnRepr (field0, field1, field2, field3, field4, field5) -> 
            field0 |> List.iter this.Visit
            this.Visit field1
            field2 |> ignore
            field3 |> ignore
            field4 |> ignore
            field5 |> ignore
            

    abstract Visit: SynValTyparDecls -> unit
    default this.Visit(node: SynValTyparDecls) : unit =

        match node with
            
        | SynValTyparDecls.SynValTyparDecls (field0, field1, field2) -> 
            field0 |> List.iter this.Visit
            field1 |> ignore
            field2 |> List.iter this.Visit
            

    abstract Visit: SynValInfo -> unit
    default this.Visit(node: SynValInfo) : unit =

        match node with
            
        | SynValInfo.SynValInfo (field0, field1) -> 
            field0 |> ignore
            this.Visit field1
            

    abstract Visit: SynTypeDefnSigRepr -> unit
    default this.Visit(node: SynTypeDefnSigRepr) : unit =

        match node with
            
        | SynTypeDefnSigRepr.ObjectModel (field0, field1, field2) -> 
            this.Visit field0
            field1 |> List.iter this.Visit
            field2 |> ignore
            
        | SynTypeDefnSigRepr.Simple (field0, field1) -> 
            this.Visit field0
            field1 |> ignore
            
        | SynTypeDefnSigRepr.Exception (field0) -> 
            this.Visit field0
            

    abstract Visit: SynArgPats -> unit
    default this.Visit(node: SynArgPats) : unit =

        match node with
            
        | SynArgPats.Pats (field0) -> 
            field0 |> List.iter this.Visit
            
        | SynArgPats.NamePatPairs (field0, field1) -> 
            field0 |> ignore
            field1 |> ignore
            

    abstract Visit: SynMeasure -> unit
    default this.Visit(node: SynMeasure) : unit =

        match node with
            
        | SynMeasure.Named (field0, field1) -> 
            field0 |> List.iter this.Visit
            field1 |> ignore
            
        | SynMeasure.Product (field0, field1, field2) -> 
            this.Visit field0
            this.Visit field1
            field2 |> ignore
            
        | SynMeasure.Seq (field0, field1) -> 
            field0 |> List.iter this.Visit
            field1 |> ignore
            
        | SynMeasure.Divide (field0, field1, field2) -> 
            this.Visit field0
            this.Visit field1
            field2 |> ignore
            
        | SynMeasure.Power (field0, field1, field2) -> 
            this.Visit field0
            this.Visit field1
            field2 |> ignore
            
        | SynMeasure.One -> ()
        | SynMeasure.Anon (field0) -> 
            field0 |> ignore
            
        | SynMeasure.Var (field0, field1) -> 
            this.Visit field0
            field1 |> ignore
            

    abstract Visit: SynRationalConst -> unit
    default this.Visit(node: SynRationalConst) : unit =

        match node with
            
        | SynRationalConst.Integer (field0) -> 
            field0 |> ignore
            
        | SynRationalConst.Rational (field0, field1, field2) -> 
            field0 |> ignore
            field1 |> ignore
            field2 |> ignore
            
        | SynRationalConst.Negate (field0) -> 
            this.Visit field0
            

    abstract Visit: SynSimplePat -> unit
    default this.Visit(node: SynSimplePat) : unit =

        match node with
            
        | SynSimplePat.Id (field0, field1, field2, field3, field4, field5) -> 
            this.Visit field0
            field1 |> ignore
            field2 |> ignore
            field3 |> ignore
            field4 |> ignore
            field5 |> ignore
            
        | SynSimplePat.Typed (field0, field1, field2) -> 
            this.Visit field0
            this.Visit field1
            field2 |> ignore
            
        | SynSimplePat.Attrib (field0, field1, field2) -> 
            this.Visit field0
            field1 |> List.iter this.Visit
            field2 |> ignore
            

    abstract Visit: SynField -> unit
    default this.Visit(node: SynField) : unit =

        match node with
            
        | SynField.Field (field0, field1, field2, field3, field4, field5, field6, field7) -> 
            field0 |> List.iter this.Visit
            field1 |> ignore
            field2 |> ignore
            this.Visit field3
            field4 |> ignore
            field5 |> ignore
            field6 |> ignore
            field7 |> ignore
            

    abstract Visit: SynTypeDefnKind -> unit
    default this.Visit(node: SynTypeDefnKind) : unit =

        match node with
            
        | SynTypeDefnKind.TyconUnspecified -> ()
        | SynTypeDefnKind.TyconClass -> ()
        | SynTypeDefnKind.TyconInterface -> ()
        | SynTypeDefnKind.TyconStruct -> ()
        | SynTypeDefnKind.TyconRecord -> ()
        | SynTypeDefnKind.TyconUnion -> ()
        | SynTypeDefnKind.TyconAbbrev -> ()
        | SynTypeDefnKind.TyconHiddenRepr -> ()
        | SynTypeDefnKind.TyconAugmentation -> ()
        | SynTypeDefnKind.TyconILAssemblyCode -> ()
        | SynTypeDefnKind.TyconDelegate (field0, field1) -> 
            this.Visit field0
            this.Visit field1
            

    abstract Visit: SynTypeDefnSimpleRepr -> unit
    default this.Visit(node: SynTypeDefnSimpleRepr) : unit =

        match node with
            
        | SynTypeDefnSimpleRepr.Union (field0, field1, field2) -> 
            field0 |> ignore
            field1 |> List.iter this.Visit
            field2 |> ignore
            
        | SynTypeDefnSimpleRepr.Enum (field0, field1) -> 
            field0 |> List.iter this.Visit
            field1 |> ignore
            
        | SynTypeDefnSimpleRepr.Record (field0, field1, field2) -> 
            field0 |> ignore
            field1 |> List.iter this.Visit
            field2 |> ignore
            
        | SynTypeDefnSimpleRepr.General (field0, field1, field2, field3, field4, field5, field6, field7) -> 
            this.Visit field0
            field1 |> ignore
            field2 |> ignore
            field3 |> List.iter this.Visit
            field4 |> ignore
            field5 |> ignore
            field6 |> ignore
            field7 |> ignore
            
        | SynTypeDefnSimpleRepr.LibraryOnlyILAssembly (field0, field1) -> 
            field0 |> ignore
            field1 |> ignore
            
        | SynTypeDefnSimpleRepr.TypeAbbrev (field0, field1, field2) -> 
            field0 |> ignore
            this.Visit field1
            field2 |> ignore
            
        | SynTypeDefnSimpleRepr.None (field0) -> 
            field0 |> ignore
            
        | SynTypeDefnSimpleRepr.Exception (field0) -> 
            this.Visit field0
            

    abstract Visit: SynUnionCase -> unit
    default this.Visit(node: SynUnionCase) : unit =

        match node with
            
        | SynUnionCase.UnionCase (field0, field1, field2, field3, field4, field5) -> 
            field0 |> List.iter this.Visit
            this.Visit field1
            this.Visit field2
            field3 |> ignore
            field4 |> ignore
            field5 |> ignore
            

    abstract Visit: SynArgInfo -> unit
    default this.Visit(node: SynArgInfo) : unit =

        match node with
            
        | SynArgInfo.SynArgInfo (field0, field1, field2) -> 
            field0 |> List.iter this.Visit
            field1 |> ignore
            field2 |> ignore
            

    abstract Visit: SynEnumCase -> unit
    default this.Visit(node: SynEnumCase) : unit =

        match node with
            
        | SynEnumCase.EnumCase (field0, field1, field2, field3, field4) -> 
            field0 |> List.iter this.Visit
            this.Visit field1
            this.Visit field2
            field3 |> ignore
            field4 |> ignore
            

    abstract Visit: SynUnionCaseType -> unit
    default this.Visit(node: SynUnionCaseType) : unit =

        match node with
            
        | SynUnionCaseType.UnionCaseFields (field0) -> 
            field0 |> List.iter this.Visit
            
        | SynUnionCaseType.UnionCaseFullType (field0, field1) -> 
            this.Visit field0
            this.Visit field1
            
