module FSharp.Compiler.SyntaxTreeVisitor
        
open FSharp.Compiler.AbstractIL.Internal.Library
open FSharp.Compiler.SyntaxTree
open FSharp.Compiler.Text


//[<AbstractClass>]
//type SyntaxTreeVisitor () =
//    abstract Visit: ParsedInput -> unit
//    default this.Visit(node: ParsedInput) : unit =
//        match node with
//        | ParsedInput.ImplFile (field0) -> 
//            this.Visit field0
            
//        | ParsedInput.SigFile (field0) -> 
//            this.Visit field0
            
//    abstract Visit: ParsedImplFileInput -> unit
//    default this.Visit(node: ParsedImplFileInput) : unit =
//        match node with
//        | ParsedImplFileInput.ParsedImplFileInput (_, _, _, _, field4, field5, _) -> 
//            field4 |> List.iter this.Visit
//            field5 |> List.iter this.Visit
            
//    abstract Visit: ParsedSigFileInput -> unit
//    default this.Visit(node: ParsedSigFileInput) : unit =
//        match node with
//        | ParsedSigFileInput.ParsedSigFileInput (_, _, _, field3, field4) -> 
//            field3 |> List.iter this.Visit
//            field4 |> List.iter this.Visit
            
//    abstract Visit: ParsedHashDirective -> unit
//    default this.Visit(node: ParsedHashDirective) : unit =
//        match node with
//        | ParsedHashDirective.ParsedHashDirective (_, _, _) -> 
//            ()
            
//    abstract Visit: SynModuleOrNamespace -> unit
//    default this.Visit(node: SynModuleOrNamespace) : unit =
//        match node with
//        | SynModuleOrNamespace.SynModuleOrNamespace (field0, _, field2, field3, _, field5, _, _) -> 
//            field0 |> List.iter this.Visit
//            this.Visit field2
//            field3 |> List.iter this.Visit
//            field5 |> List.iter this.Visit
            
//    abstract Visit: SynModuleOrNamespaceSig -> unit
//    default this.Visit(node: SynModuleOrNamespaceSig) : unit =
//        match node with
//        | SynModuleOrNamespaceSig.SynModuleOrNamespaceSig (field0, _, field2, field3, _, field5, _, _) -> 
//            field0 |> List.iter this.Visit
//            this.Visit field2
//            field3 |> List.iter this.Visit
//            field5 |> List.iter this.Visit
            
//    abstract Visit: Ident -> unit
//    default this.Visit(node: Ident) : unit =        ()
//    abstract Visit: SynModuleOrNamespaceKind -> unit
//    default this.Visit(node: SynModuleOrNamespaceKind) : unit =
//        match node with
//        | SynModuleOrNamespaceKind.NamedModule -> ()
//        | SynModuleOrNamespaceKind.AnonModule -> ()
//        | SynModuleOrNamespaceKind.DeclaredNamespace -> ()
//        | SynModuleOrNamespaceKind.GlobalNamespace -> ()
//    abstract Visit: SynModuleDecl -> unit
//    default this.Visit(node: SynModuleDecl) : unit =
//        match node with
//        | SynModuleDecl.ModuleAbbrev (field0, field1, _) -> 
//            this.Visit field0
//            field1 |> List.iter this.Visit
            
//        | SynModuleDecl.NestedModule (field0, _, field2, _, _) -> 
//            this.Visit field0
//            field2 |> List.iter this.Visit
            
//        | SynModuleDecl.Let (_, field1, _) -> 
//            field1 |> List.iter this.Visit
            
//        | SynModuleDecl.DoExpr (_, field1, _) -> 
//            this.Visit field1
            
//        | SynModuleDecl.Types (field0, _) -> 
//            field0 |> List.iter this.Visit
            
//        | SynModuleDecl.Exception (field0, _) -> 
//            this.Visit field0
            
//        | SynModuleDecl.Open (field0, _) -> 
//            this.Visit field0
            
//        | SynModuleDecl.Attributes (field0, _) -> 
//            field0 |> List.iter this.Visit
            
//        | SynModuleDecl.HashDirective (field0, _) -> 
//            this.Visit field0
            
//        | SynModuleDecl.NamespaceFragment (field0) -> 
//            this.Visit field0
            
//    abstract Visit: SynAttributeList -> unit
//    default this.Visit(node: SynAttributeList) : unit =        ()
//    abstract Visit: SynModuleSigDecl -> unit
//    default this.Visit(node: SynModuleSigDecl) : unit =
//        match node with
//        | SynModuleSigDecl.ModuleAbbrev (field0, field1, _) -> 
//            this.Visit field0
//            field1 |> List.iter this.Visit
            
//        | SynModuleSigDecl.NestedModule (field0, _, field2, _) -> 
//            this.Visit field0
//            field2 |> List.iter this.Visit
            
//        | SynModuleSigDecl.Val (field0, _) -> 
//            this.Visit field0
            
//        | SynModuleSigDecl.Types (field0, _) -> 
//            field0 |> List.iter this.Visit
            
//        | SynModuleSigDecl.Exception (field0, _) -> 
//            this.Visit field0
            
//        | SynModuleSigDecl.Open (field0, _) -> 
//            field0 |> List.iter this.Visit
            
//        | SynModuleSigDecl.HashDirective (field0, _) -> 
//            this.Visit field0
            
//        | SynModuleSigDecl.NamespaceFragment (field0) -> 
//            this.Visit field0
            
//    abstract Visit: SynComponentInfo -> unit
//    default this.Visit(node: SynComponentInfo) : unit =
//        match node with
//        | SynComponentInfo.ComponentInfo (field0, field1, field2, field3, _, _, _, _) -> 
//            field0 |> List.iter this.Visit
//            field1 |> List.iter this.Visit
//            field2 |> List.iter this.Visit
//            field3 |> List.iter this.Visit
            
//    abstract Visit: SynBinding -> unit
//    default this.Visit(node: SynBinding) : unit =
//        match node with
//        | SynBinding.Binding (_, field1, _, _, field4, _, field6, field7, _, field9, _, _) -> 
//            this.Visit field1
//            field4 |> List.iter this.Visit
//            this.Visit field6
//            this.Visit field7
//            this.Visit field9
            
//    abstract Visit: SynExpr -> unit
//    default this.Visit(node: SynExpr) : unit =
//        match node with
//        | SynExpr.Paren (field0, _, _, _) -> 
//            this.Visit field0
            
//        | SynExpr.Quote (field0, _, field2, _, _) -> 
//            this.Visit field0
//            this.Visit field2
            
//        | SynExpr.Const (field0, _) -> 
//            this.Visit field0
            
//        | SynExpr.Typed (field0, field1, _) -> 
//            this.Visit field0
//            this.Visit field1
            
//        | SynExpr.Tuple (_, field1, _, _) -> 
//            field1 |> List.iter this.Visit
            
//        | SynExpr.AnonRecd (_, _, _, _) -> 
//            ()
            
//        | SynExpr.ArrayOrList (_, field1, _) -> 
//            field1 |> List.iter this.Visit
            
//        | SynExpr.Record (_, _, _, _) -> 
//            ()
            
//        | SynExpr.New (_, field1, field2, _) -> 
//            this.Visit field1
//            this.Visit field2
            
//        | SynExpr.ObjExpr (field0, _, field2, field3, _, _) -> 
//            this.Visit field0
//            field2 |> List.iter this.Visit
//            field3 |> List.iter this.Visit
            
//        | SynExpr.While (_, field1, field2, _) -> 
//            this.Visit field1
//            this.Visit field2
            
//        | SynExpr.For (_, field1, field2, _, field4, field5, _) -> 
//            this.Visit field1
//            this.Visit field2
//            this.Visit field4
//            this.Visit field5
            
//        | SynExpr.ForEach (_, _, _, field3, field4, field5, _) -> 
//            this.Visit field3
//            this.Visit field4
//            this.Visit field5
            
//        | SynExpr.ArrayOrListOfSeqExpr (_, field1, _) -> 
//            this.Visit field1
            
//        | SynExpr.CompExpr (_, _, field2, _) -> 
//            this.Visit field2
            
//        | SynExpr.Lambda (_, _, field2, field3, _) -> 
//            this.Visit field2
//            this.Visit field3
            
//        | SynExpr.MatchLambda (_, _, field2, _, _) -> 
//            field2 |> List.iter this.Visit
            
//        | SynExpr.Match (_, field1, field2, _) -> 
//            this.Visit field1
//            field2 |> List.iter this.Visit
            
//        | SynExpr.Do (field0, _) -> 
//            this.Visit field0
            
//        | SynExpr.Assert (field0, _) -> 
//            this.Visit field0
            
//        | SynExpr.App (_, _, field2, field3, _) -> 
//            this.Visit field2
//            this.Visit field3
            
//        | SynExpr.TypeApp (field0, _, field2, _, _, _, _) -> 
//            this.Visit field0
//            field2 |> List.iter this.Visit
            
//        | SynExpr.LetOrUse (_, _, field2, field3, _) -> 
//            field2 |> List.iter this.Visit
//            this.Visit field3
            
//        | SynExpr.TryWith (field0, _, field2, _, _, _, _) -> 
//            this.Visit field0
//            field2 |> List.iter this.Visit
            
//        | SynExpr.TryFinally (field0, field1, _, _, _) -> 
//            this.Visit field0
//            this.Visit field1
            
//        | SynExpr.Lazy (field0, _) -> 
//            this.Visit field0
            
//        | SynExpr.Sequential (_, _, field2, field3, _) -> 
//            this.Visit field2
//            this.Visit field3
            
//        | SynExpr.IfThenElse (field0, field1, _, _, _, _, _) -> 
//            this.Visit field0
//            this.Visit field1
            
//        | SynExpr.Ident (field0) -> 
//            this.Visit field0
            
//        | SynExpr.LongIdent (_, field1, _, _) -> 
//            this.Visit field1
            
//        | SynExpr.LongIdentSet (field0, field1, _) -> 
//            this.Visit field0
//            this.Visit field1
            
//        | SynExpr.DotGet (field0, _, field2, _) -> 
//            this.Visit field0
//            this.Visit field2
            
//        | SynExpr.DotSet (field0, field1, field2, _) -> 
//            this.Visit field0
//            this.Visit field1
//            this.Visit field2
            
//        | SynExpr.Set (field0, field1, _) -> 
//            this.Visit field0
//            this.Visit field1
            
//        | SynExpr.DotIndexedGet (field0, field1, _, _) -> 
//            this.Visit field0
//            field1 |> List.iter this.Visit
            
//        | SynExpr.DotIndexedSet (field0, field1, field2, _, _, _) -> 
//            this.Visit field0
//            field1 |> List.iter this.Visit
//            this.Visit field2
            
//        | SynExpr.NamedIndexedPropertySet (field0, field1, field2, _) -> 
//            this.Visit field0
//            this.Visit field1
//            this.Visit field2
            
//        | SynExpr.DotNamedIndexedPropertySet (field0, field1, field2, field3, _) -> 
//            this.Visit field0
//            this.Visit field1
//            this.Visit field2
//            this.Visit field3
            
//        | SynExpr.TypeTest (field0, field1, _) -> 
//            this.Visit field0
//            this.Visit field1
            
//        | SynExpr.Upcast (field0, field1, _) -> 
//            this.Visit field0
//            this.Visit field1
            
//        | SynExpr.Downcast (field0, field1, _) -> 
//            this.Visit field0
//            this.Visit field1
            
//        | SynExpr.InferredUpcast (field0, _) -> 
//            this.Visit field0
            
//        | SynExpr.InferredDowncast (field0, _) -> 
//            this.Visit field0
            
//        | SynExpr.Null (_) -> 
//            ()
            
//        | SynExpr.AddressOf (_, field1, _, _) -> 
//            this.Visit field1
            
//        | SynExpr.TraitCall (field0, field1, field2, _) -> 
//            field0 |> List.iter this.Visit
//            this.Visit field1
//            this.Visit field2
            
//        | SynExpr.JoinIn (field0, _, field2, _) -> 
//            this.Visit field0
//            this.Visit field2
            
//        | SynExpr.ImplicitZero (_) -> 
//            ()
            
//        | SynExpr.SequentialOrImplicitYield (_, field1, field2, field3, _) -> 
//            this.Visit field1
//            this.Visit field2
//            this.Visit field3
            
//        | SynExpr.YieldOrReturn (_, field1, _) -> 
//            this.Visit field1
            
//        | SynExpr.YieldOrReturnFrom (_, field1, _) -> 
//            this.Visit field1
            
//        | SynExpr.LetOrUseBang (_, _, _, field3, field4, _, field6, _) -> 
//            this.Visit field3
//            this.Visit field4
//            this.Visit field6
            
//        | SynExpr.MatchBang (_, field1, field2, _) -> 
//            this.Visit field1
//            field2 |> List.iter this.Visit
            
//        | SynExpr.DoBang (field0, _) -> 
//            this.Visit field0
            
//        | SynExpr.LibraryOnlyILAssembly (_, field1, field2, field3, _) -> 
//            field1 |> List.iter this.Visit
//            field2 |> List.iter this.Visit
//            field3 |> List.iter this.Visit
            
//        | SynExpr.LibraryOnlyStaticOptimization (field0, field1, field2, _) -> 
//            field0 |> List.iter this.Visit
//            this.Visit field1
//            this.Visit field2
            
//        | SynExpr.LibraryOnlyUnionCaseFieldGet (field0, field1, _, _) -> 
//            this.Visit field0
//            field1 |> List.iter this.Visit
            
//        | SynExpr.LibraryOnlyUnionCaseFieldSet (field0, field1, _, field3, _) -> 
//            this.Visit field0
//            field1 |> List.iter this.Visit
//            this.Visit field3
            
//        | SynExpr.ArbitraryAfterError (_, _) -> 
//            ()
            
//        | SynExpr.FromParseError (field0, _) -> 
//            this.Visit field0
            
//        | SynExpr.DiscardAfterMissingQualificationAfterDot (field0, _) -> 
//            this.Visit field0
            
//        | SynExpr.Fixed (field0, _) -> 
//            this.Visit field0
            
//    abstract Visit: SynTypeDefn -> unit
//    default this.Visit(node: SynTypeDefn) : unit =
//        match node with
//        | SynTypeDefn.TypeDefn (field0, field1, field2, _) -> 
//            this.Visit field0
//            this.Visit field1
//            field2 |> List.iter this.Visit
            
//    abstract Visit: SynExceptionDefn -> unit
//    default this.Visit(node: SynExceptionDefn) : unit =
//        match node with
//        | SynExceptionDefn.SynExceptionDefn (field0, field1, _) -> 
//            this.Visit field0
//            field1 |> List.iter this.Visit
            
//    abstract Visit: LongIdentWithDots -> unit
//    default this.Visit(node: LongIdentWithDots) : unit =
//        match node with
//        | LongIdentWithDots.LongIdentWithDots (field0, _) -> 
//            field0 |> List.iter this.Visit
            
//    abstract Visit: SynValSig -> unit
//    default this.Visit(node: SynValSig) : unit =
//        match node with
//        | SynValSig.ValSpfn (field0, field1, field2, field3, field4, _, _, _, _, _, _) -> 
//            field0 |> List.iter this.Visit
//            this.Visit field1
//            this.Visit field2
//            this.Visit field3
//            this.Visit field4
            
//    abstract Visit: SynTypeDefnSig -> unit
//    default this.Visit(node: SynTypeDefnSig) : unit =
//        match node with
//        | SynTypeDefnSig.TypeDefnSig (field0, field1, field2, _) -> 
//            this.Visit field0
//            this.Visit field1
//            field2 |> List.iter this.Visit
            
//    abstract Visit: SynExceptionSig -> unit
//    default this.Visit(node: SynExceptionSig) : unit =
//        match node with
//        | SynExceptionSig.SynExceptionSig (field0, field1, _) -> 
//            this.Visit field0
//            field1 |> List.iter this.Visit
            
//    abstract Visit: SynTyparDecl -> unit
//    default this.Visit(node: SynTyparDecl) : unit =
//        match node with
//        | SynTyparDecl.TyparDecl (field0, field1) -> 
//            field0 |> List.iter this.Visit
//            this.Visit field1
            
//    abstract Visit: SynTypeConstraint -> unit
//    default this.Visit(node: SynTypeConstraint) : unit =
//        match node with
//        | SynTypeConstraint.WhereTyparIsValueType (field0, _) -> 
//            this.Visit field0
            
//        | SynTypeConstraint.WhereTyparIsReferenceType (field0, _) -> 
//            this.Visit field0
            
//        | SynTypeConstraint.WhereTyparIsUnmanaged (field0, _) -> 
//            this.Visit field0
            
//        | SynTypeConstraint.WhereTyparSupportsNull (field0, _) -> 
//            this.Visit field0
            
//        | SynTypeConstraint.WhereTyparIsComparable (field0, _) -> 
//            this.Visit field0
            
//        | SynTypeConstraint.WhereTyparIsEquatable (field0, _) -> 
//            this.Visit field0
            
//        | SynTypeConstraint.WhereTyparDefaultsToType (field0, field1, _) -> 
//            this.Visit field0
//            this.Visit field1
            
//        | SynTypeConstraint.WhereTyparSubtypeOfType (field0, field1, _) -> 
//            this.Visit field0
//            this.Visit field1
            
//        | SynTypeConstraint.WhereTyparSupportsMember (field0, field1, _) -> 
//            field0 |> List.iter this.Visit
//            this.Visit field1
            
//        | SynTypeConstraint.WhereTyparIsEnum (field0, field1, _) -> 
//            this.Visit field0
//            field1 |> List.iter this.Visit
            
//        | SynTypeConstraint.WhereTyparIsDelegate (field0, field1, _) -> 
//            this.Visit field0
//            field1 |> List.iter this.Visit
            
//    abstract Visit: SynBindingKind -> unit
//    default this.Visit(node: SynBindingKind) : unit =
//        match node with
//        | SynBindingKind.StandaloneExpression -> ()
//        | SynBindingKind.NormalBinding -> ()
//        | SynBindingKind.DoBinding -> ()
//    abstract Visit: SynValData -> unit
//    default this.Visit(node: SynValData) : unit =
//        match node with
//        | SynValData.SynValData (_, field1, _) -> 
//            this.Visit field1
            
//    abstract Visit: SynPat -> unit
//    default this.Visit(node: SynPat) : unit =
//        match node with
//        | SynPat.Const (field0, _) -> 
//            this.Visit field0
            
//        | SynPat.Wild (_) -> 
//            ()
            
//        | SynPat.Named (field0, field1, _, _, _) -> 
//            this.Visit field0
//            this.Visit field1
            
//        | SynPat.Typed (field0, field1, _) -> 
//            this.Visit field0
//            this.Visit field1
            
//        | SynPat.Attrib (field0, field1, _) -> 
//            this.Visit field0
//            field1 |> List.iter this.Visit
            
//        | SynPat.Or (field0, field1, _) -> 
//            this.Visit field0
//            this.Visit field1
            
//        | SynPat.Ands (field0, _) -> 
//            field0 |> List.iter this.Visit
            
//        | SynPat.LongIdent (field0, _, _, field3, _, _) -> 
//            this.Visit field0
//            this.Visit field3
            
//        | SynPat.Tuple (_, field1, _) -> 
//            field1 |> List.iter this.Visit
            
//        | SynPat.Paren (field0, _) -> 
//            this.Visit field0
            
//        | SynPat.ArrayOrList (_, field1, _) -> 
//            field1 |> List.iter this.Visit
            
//        | SynPat.Record (_, _) -> 
//            ()
            
//        | SynPat.Null (_) -> 
//            ()
            
//        | SynPat.OptionalVal (field0, _) -> 
//            this.Visit field0
            
//        | SynPat.IsInst (field0, _) -> 
//            this.Visit field0
            
//        | SynPat.QuoteExpr (field0, _) -> 
//            this.Visit field0
            
//        | SynPat.DeprecatedCharRange (_, _, _) -> 
//            ()
            
//        | SynPat.InstanceMember (field0, field1, _, _, _) -> 
//            this.Visit field0
//            this.Visit field1
            
//        | SynPat.FromParseError (field0, _) -> 
//            this.Visit field0
            
//    abstract Visit: SynConst -> unit
//    default this.Visit(node: SynConst) : unit =
//        match node with
//        | SynConst.Unit -> ()
//        | SynConst.Bool (_) -> 
//            ()
            
//        | SynConst.SByte (_) -> 
//            ()
            
//        | SynConst.Byte (_) -> 
//            ()
            
//        | SynConst.Int16 (_) -> 
//            ()
            
//        | SynConst.UInt16 (_) -> 
//            ()
            
//        | SynConst.Int32 (_) -> 
//            ()
            
//        | SynConst.UInt32 (_) -> 
//            ()
            
//        | SynConst.Int64 (_) -> 
//            ()
            
//        | SynConst.UInt64 (_) -> 
//            ()
            
//        | SynConst.IntPtr (_) -> 
//            ()
            
//        | SynConst.UIntPtr (_) -> 
//            ()
            
//        | SynConst.Single (_) -> 
//            ()
            
//        | SynConst.Double (_) -> 
//            ()
            
//        | SynConst.Char (_) -> 
//            ()
            
//        | SynConst.Decimal (_) -> 
//            ()
            
//        | SynConst.UserNum (_, _) -> 
//            ()
            
//        | SynConst.String (_, _) -> 
//            ()
            
//        | SynConst.Bytes (_, _) -> 
//            ()
            
//        | SynConst.UInt16s (_) -> 
//            ()
            
//        | SynConst.Measure (field0, field1) -> 
//            this.Visit field0
//            this.Visit field1
            
//    abstract Visit: SynType -> unit
//    default this.Visit(node: SynType) : unit =
//        match node with
//        | SynType.LongIdent (field0) -> 
//            this.Visit field0
            
//        | SynType.App (field0, _, field2, _, _, _, _) -> 
//            this.Visit field0
//            field2 |> List.iter this.Visit
            
//        | SynType.LongIdentApp (field0, field1, _, field3, _, _, _) -> 
//            this.Visit field0
//            this.Visit field1
//            field3 |> List.iter this.Visit
            
//        | SynType.Tuple (_, _, _) -> 
//            ()
            
//        | SynType.AnonRecd (_, _, _) -> 
//            ()
            
//        | SynType.Array (_, field1, _) -> 
//            this.Visit field1
            
//        | SynType.Fun (field0, field1, _) -> 
//            this.Visit field0
//            this.Visit field1
            
//        | SynType.Var (field0, _) -> 
//            this.Visit field0
            
//        | SynType.Anon (_) -> 
//            ()
            
//        | SynType.WithGlobalConstraints (field0, field1, _) -> 
//            this.Visit field0
//            field1 |> List.iter this.Visit
            
//        | SynType.HashConstraint (field0, _) -> 
//            this.Visit field0
            
//        | SynType.MeasureDivide (field0, field1, _) -> 
//            this.Visit field0
//            this.Visit field1
            
//        | SynType.MeasurePower (field0, field1, _) -> 
//            this.Visit field0
//            this.Visit field1
            
//        | SynType.StaticConstant (field0, _) -> 
//            this.Visit field0
            
//        | SynType.StaticConstantExpr (field0, _) -> 
//            this.Visit field0
            
//        | SynType.StaticConstantNamed (field0, field1, _) -> 
//            this.Visit field0
//            this.Visit field1
            
//        | SynType.Paren (field0, _) -> 
//            this.Visit field0
            
//    abstract Visit: SynInterfaceImpl -> unit
//    default this.Visit(node: SynInterfaceImpl) : unit =
//        match node with
//        | SynInterfaceImpl.InterfaceImpl (field0, field1, _) -> 
//            this.Visit field0
//            field1 |> List.iter this.Visit
            
//    abstract Visit: SynSimplePats -> unit
//    default this.Visit(node: SynSimplePats) : unit =
//        match node with
//        | SynSimplePats.SimplePats (field0, _) -> 
//            field0 |> List.iter this.Visit
            
//        | SynSimplePats.Typed (field0, field1, _) -> 
//            this.Visit field0
//            this.Visit field1
            
//    abstract Visit: SynMatchClause -> unit
//    default this.Visit(node: SynMatchClause) : unit =
//        match node with
//        | SynMatchClause.Clause (field0, _, field2, _, _) -> 
//            this.Visit field0
//            this.Visit field2
            
//    abstract Visit: SynIndexerArg -> unit
//    default this.Visit(node: SynIndexerArg) : unit =
//        match node with
//        | SynIndexerArg.Two (field0, _, field2, _, _, _) -> 
//            this.Visit field0
//            this.Visit field2
            
//        | SynIndexerArg.One (field0, _, _) -> 
//            this.Visit field0
            
//    abstract Visit: SynTypar -> unit
//    default this.Visit(node: SynTypar) : unit =
//        match node with
//        | SynTypar.Typar (field0, _, _) -> 
//            this.Visit field0
            
//    abstract Visit: SynMemberSig -> unit
//    default this.Visit(node: SynMemberSig) : unit =
//        match node with
//        | SynMemberSig.Member (field0, _, _) -> 
//            this.Visit field0
            
//        | SynMemberSig.Interface (field0, _) -> 
//            this.Visit field0
            
//        | SynMemberSig.Inherit (field0, _) -> 
//            this.Visit field0
            
//        | SynMemberSig.ValField (field0, _) -> 
//            this.Visit field0
            
//        | SynMemberSig.NestedType (field0, _) -> 
//            this.Visit field0
            
//    abstract Visit: SynStaticOptimizationConstraint -> unit
//    default this.Visit(node: SynStaticOptimizationConstraint) : unit =
//        match node with
//        | SynStaticOptimizationConstraint.WhenTyparTyconEqualsTycon (field0, field1, _) -> 
//            this.Visit field0
//            this.Visit field1
            
//        | SynStaticOptimizationConstraint.WhenTyparIsStruct (field0, _) -> 
//            this.Visit field0
            
//    abstract Visit: SynTypeDefnRepr -> unit
//    default this.Visit(node: SynTypeDefnRepr) : unit =
//        match node with
//        | SynTypeDefnRepr.ObjectModel (field0, field1, _) -> 
//            this.Visit field0
//            field1 |> List.iter this.Visit
            
//        | SynTypeDefnRepr.Simple (field0, _) -> 
//            this.Visit field0
            
//        | SynTypeDefnRepr.Exception (field0) -> 
//            this.Visit field0
            
//    abstract Visit: SynMemberDefn -> unit
//    default this.Visit(node: SynMemberDefn) : unit =
//        match node with
//        | SynMemberDefn.Open (field0, _) -> 
//            field0 |> List.iter this.Visit
            
//        | SynMemberDefn.Member (field0, _) -> 
//            this.Visit field0
            
//        | SynMemberDefn.ImplicitCtor (_, field1, field2, _, _) -> 
//            field1 |> List.iter this.Visit
//            this.Visit field2
            
//        | SynMemberDefn.ImplicitInherit (field0, field1, _, _) -> 
//            this.Visit field0
//            this.Visit field1
            
//        | SynMemberDefn.LetBindings (field0, _, _, _) -> 
//            field0 |> List.iter this.Visit
            
//        | SynMemberDefn.AbstractSlot (field0, _, _) -> 
//            this.Visit field0
            
//        | SynMemberDefn.Interface (field0, _, _) -> 
//            this.Visit field0
            
//        | SynMemberDefn.Inherit (field0, _, _) -> 
//            this.Visit field0
            
//        | SynMemberDefn.ValField (field0, _) -> 
//            this.Visit field0
            
//        | SynMemberDefn.NestedType (field0, _, _) -> 
//            this.Visit field0
            
//        | SynMemberDefn.AutoProperty (field0, _, field2, _, _, _, _, _, field8, _, _) -> 
//            field0 |> List.iter this.Visit
//            this.Visit field2
//            this.Visit field8
            
//    abstract Visit: SynExceptionDefnRepr -> unit
//    default this.Visit(node: SynExceptionDefnRepr) : unit =
//        match node with
//        | SynExceptionDefnRepr.SynExceptionDefnRepr (field0, field1, _, _, _, _) -> 
//            field0 |> List.iter this.Visit
//            this.Visit field1
            
//    abstract Visit: SynValTyparDecls -> unit
//    default this.Visit(node: SynValTyparDecls) : unit =
//        match node with
//        | SynValTyparDecls.SynValTyparDecls (field0, _, field2) -> 
//            field0 |> List.iter this.Visit
//            field2 |> List.iter this.Visit
            
//    abstract Visit: SynValInfo -> unit
//    default this.Visit(node: SynValInfo) : unit =
//        match node with
//        | SynValInfo.SynValInfo (_, field1) -> 
//            this.Visit field1
            
//    abstract Visit: SynTypeDefnSigRepr -> unit
//    default this.Visit(node: SynTypeDefnSigRepr) : unit =
//        match node with
//        | SynTypeDefnSigRepr.ObjectModel (field0, field1, _) -> 
//            this.Visit field0
//            field1 |> List.iter this.Visit
            
//        | SynTypeDefnSigRepr.Simple (field0, _) -> 
//            this.Visit field0
            
//        | SynTypeDefnSigRepr.Exception (field0) -> 
//            this.Visit field0
            
//    abstract Visit: SynArgPats -> unit
//    default this.Visit(node: SynArgPats) : unit =
//        match node with
//        | SynArgPats.Pats (field0) -> 
//            field0 |> List.iter this.Visit
            
//        | SynArgPats.NamePatPairs (_, _) -> 
//            ()
            
//    abstract Visit: SynMeasure -> unit
//    default this.Visit(node: SynMeasure) : unit =
//        match node with
//        | SynMeasure.Named (field0, _) -> 
//            field0 |> List.iter this.Visit
            
//        | SynMeasure.Product (field0, field1, _) -> 
//            this.Visit field0
//            this.Visit field1
            
//        | SynMeasure.Seq (field0, _) -> 
//            field0 |> List.iter this.Visit
            
//        | SynMeasure.Divide (field0, field1, _) -> 
//            this.Visit field0
//            this.Visit field1
            
//        | SynMeasure.Power (field0, field1, _) -> 
//            this.Visit field0
//            this.Visit field1
            
//        | SynMeasure.One -> ()
//        | SynMeasure.Anon (_) -> 
//            ()
            
//        | SynMeasure.Var (field0, _) -> 
//            this.Visit field0
            
//    abstract Visit: SynRationalConst -> unit
//    default this.Visit(node: SynRationalConst) : unit =
//        match node with
//        | SynRationalConst.Integer (_) -> 
//            ()
            
//        | SynRationalConst.Rational (_, _, _) -> 
//            ()
            
//        | SynRationalConst.Negate (field0) -> 
//            this.Visit field0
            
//    abstract Visit: SynSimplePat -> unit
//    default this.Visit(node: SynSimplePat) : unit =
//        match node with
//        | SynSimplePat.Id (field0, _, _, _, _, _) -> 
//            this.Visit field0
            
//        | SynSimplePat.Typed (field0, field1, _) -> 
//            this.Visit field0
//            this.Visit field1
            
//        | SynSimplePat.Attrib (field0, field1, _) -> 
//            this.Visit field0
//            field1 |> List.iter this.Visit
            
//    abstract Visit: SynField -> unit
//    default this.Visit(node: SynField) : unit =
//        match node with
//        | SynField.Field (field0, _, _, field3, _, _, _, _) -> 
//            field0 |> List.iter this.Visit
//            this.Visit field3
            
//    abstract Visit: SynTypeDefnKind -> unit
//    default this.Visit(node: SynTypeDefnKind) : unit =
//        match node with
//        | SynTypeDefnKind.TyconUnspecified -> ()
//        | SynTypeDefnKind.TyconClass -> ()
//        | SynTypeDefnKind.TyconInterface -> ()
//        | SynTypeDefnKind.TyconStruct -> ()
//        | SynTypeDefnKind.TyconRecord -> ()
//        | SynTypeDefnKind.TyconUnion -> ()
//        | SynTypeDefnKind.TyconAbbrev -> ()
//        | SynTypeDefnKind.TyconHiddenRepr -> ()
//        | SynTypeDefnKind.TyconAugmentation -> ()
//        | SynTypeDefnKind.TyconILAssemblyCode -> ()
//        | SynTypeDefnKind.TyconDelegate (field0, field1) -> 
//            this.Visit field0
//            this.Visit field1
            
//    abstract Visit: SynTypeDefnSimpleRepr -> unit
//    default this.Visit(node: SynTypeDefnSimpleRepr) : unit =
//        match node with
//        | SynTypeDefnSimpleRepr.Union (_, field1, _) -> 
//            field1 |> List.iter this.Visit
            
//        | SynTypeDefnSimpleRepr.Enum (field0, _) -> 
//            field0 |> List.iter this.Visit
            
//        | SynTypeDefnSimpleRepr.Record (_, field1, _) -> 
//            field1 |> List.iter this.Visit
            
//        | SynTypeDefnSimpleRepr.General (field0, _, _, field3, _, _, _, _) -> 
//            this.Visit field0
//            field3 |> List.iter this.Visit
            
//        | SynTypeDefnSimpleRepr.LibraryOnlyILAssembly (_, _) -> 
//            ()
            
//        | SynTypeDefnSimpleRepr.TypeAbbrev (_, field1, _) -> 
//            this.Visit field1
            
//        | SynTypeDefnSimpleRepr.None (_) -> 
//            ()
            
//        | SynTypeDefnSimpleRepr.Exception (field0) -> 
//            this.Visit field0
            
//    abstract Visit: SynUnionCase -> unit
//    default this.Visit(node: SynUnionCase) : unit =
//        match node with
//        | SynUnionCase.UnionCase (field0, field1, field2, _, _, _) -> 
//            field0 |> List.iter this.Visit
//            this.Visit field1
//            this.Visit field2
            
//    abstract Visit: SynArgInfo -> unit
//    default this.Visit(node: SynArgInfo) : unit =
//        match node with
//        | SynArgInfo.SynArgInfo (field0, _, _) -> 
//            field0 |> List.iter this.Visit
            
//    abstract Visit: SynEnumCase -> unit
//    default this.Visit(node: SynEnumCase) : unit =
//        match node with
//        | SynEnumCase.EnumCase (field0, field1, field2, _, _) -> 
//            field0 |> List.iter this.Visit
//            this.Visit field1
//            this.Visit field2
            
//    abstract Visit: SynUnionCaseType -> unit
//    default this.Visit(node: SynUnionCaseType) : unit =
//        match node with
//        | SynUnionCaseType.UnionCaseFields (field0) -> 
//            field0 |> List.iter this.Visit
            
//        | SynUnionCaseType.UnionCaseFullType (field0, field1) -> 
//            this.Visit field0
//            this.Visit field1