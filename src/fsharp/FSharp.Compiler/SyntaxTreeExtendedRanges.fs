module FSharp.Compiler.SyntaxTreeExtendedRanges

open FSharp.Compiler.AbstractIL.Internal.Library
open FSharp.Compiler.SyntaxTree
open FSharp.Compiler.Range
    
let isZeroRange (r: range) =
    posEq r.Start r.End

//type SynModuleOrNamespace with

//    member this.Range =
//        match this with
//        | SynModuleOrNamespace (longId=longId;range=m) -> 
//            match longId with
//            | [] -> m
//            | _ ->
//                let longIdRange =
//                    longId
//                    |> List.map (fun x -> x.idRange)
//                    |> List.reduce unionRanges
//                unionRanges longIdRange m

//type ParsedImplFileInput with

//    member this.Range =
//        match this with
//        | ParsedImplFileInput(modules=modules) ->
//            match modules with
//            | [] -> range0
//            | _ ->
//                modules
//                |> List.map (fun x -> x.Range)
//                |> List.reduce (unionRanges)


//type SynModuleOrNamespaceSig with

//    member this.Range =
//        match this with
//        | SynModuleOrNamespaceSig (longId=longId;range=m) -> 
//            match longId with
//            | [] -> m
//            | _ ->
//                let longIdRange =
//                    longId
//                    |> List.map (fun x -> x.idRange)
//                    |> List.reduce unionRanges
//                unionRanges longIdRange m

//type ParsedSigFileInput with

//    member this.Range =
//        match this with
//        | ParsedSigFileInput(modules=modules) ->
//            match modules with
//            | [] -> range0
//            | _ ->
//                modules
//                |> List.map (fun x -> x.Range)
//                |> List.reduce (unionRanges)

//type ParsedInput with

//    member this.Range =
//        match this with
//        | ParsedInput.ImplFile input -> input.Range
//        | ParsedInput.SigFile input -> input.Range

//type ParsedHashDirective with

//    member this.Range =
//        match this with
//        | ParsedHashDirective (_, _, m) -> m

//type SynTypeConstraint with

//    member this.Range =
//        match this with
//        | SynTypeConstraint.WhereTyparIsValueType (_, m) -> m
//        | SynTypeConstraint.WhereTyparIsReferenceType (_, m) -> m
//        | SynTypeConstraint.WhereTyparIsUnmanaged (_, m) -> m
//        | SynTypeConstraint.WhereTyparSupportsNull (_, m) -> m
//        | SynTypeConstraint.WhereTyparIsComparable (_, m) -> m
//        | SynTypeConstraint.WhereTyparIsEquatable (_, m) -> m
//        | SynTypeConstraint.WhereTyparDefaultsToType (_, _, m) -> m
//        | SynTypeConstraint.WhereTyparSubtypeOfType (_, _, m) -> m
//        | SynTypeConstraint.WhereTyparSupportsMember (_, _, m) -> m
//        | SynTypeConstraint.WhereTyparIsEnum (_, _, m) -> m
//        | SynTypeConstraint.WhereTyparIsDelegate (_, _, m) -> m

//type SynMemberSig with

//    member this.Range =
//        match this with
//        | SynMemberSig.Member (_, _, m) -> m
//        | SynMemberSig.Interface (_, m) -> m
//        | SynMemberSig.Inherit (_, m) -> m
//        | SynMemberSig.ValField (_, m) -> m
//        | SynMemberSig.NestedType (_, m) -> m

//type SynValSig with

//    member this.Range =
//        match this with
//        | ValSpfn (_, _, _, _, _, _, _, _, _, _, m) -> m

//type SynField with

//    member this.Range =
//        match this with
//        | Field (_, _, _, _, _, _, _, m) -> m

//type SynTypeDefnSig with

//    member this.Range =
//        match this with
//        | TypeDefnSig (_, _, _, m) -> m

//type SynMeasure with

//    member this.Range =
//        match this with
//        | SynMeasure.Named (range=m) -> m
//        | SynMeasure.Product (range=m) -> m
//        | SynMeasure.Seq (range=m) -> m
//        | SynMeasure.Divide (range=m) -> m
//        | SynMeasure.Power (range=m) -> m
//        | SynMeasure.One -> range0
//        | SynMeasure.Anon (range=m) -> m
//        | SynMeasure.Var (range=m) -> m

//type SynRationalConst with

//    member this.Range =
//        match this with
//        | SynRationalConst.Integer _ -> range0
//        | SynRationalConst.Rational (range=m) -> m
//        | SynRationalConst.Negate rationalConst -> rationalConst.Range

//type SynConst with

//    member this.Range =
//        this.Range range0

//type SynArgInfo with

//    member this.Range =
//        match this with
//        | SynArgInfo (attribs, _, idOpt) ->
//            let ranges =
//                attribs
//                |> List.map (fun x -> x.Range)
//                |> List.append (match idOpt with | Some id -> [id.idRange] | _ -> [])

//            match ranges with
//            | [] -> range0
//            | _ ->
//                ranges
//                |> List.reduce unionRanges

//type SynValInfo with

//    member this.Range =
//        match this with
//        | SynValInfo (argInfos, argInfo) ->
//            match argInfos with
//            | [] -> range0
//            | _ ->
//                let result =
//                    argInfos
//                    |> List.reduce (@)
//                    |> List.append [argInfo]
//                    |> List.map (fun x -> x.Range)
//                    |> List.filter (fun x -> not (isZeroRange x))
//                match result with
//                | [] -> range0
//                | result ->
//                    result
//                    |> List.reduce unionRanges

//type SynTypeDefnKind with

//    member this.Range =
//        match this with
//        | TyconUnspecified
//        | TyconClass
//        | TyconInterface
//        | TyconStruct
//        | TyconRecord
//        | TyconUnion
//        | TyconAbbrev
//        | TyconHiddenRepr
//        | TyconAugmentation
//        | TyconILAssemblyCode ->
//            range0
//        | TyconDelegate (ty, valInfo) ->
//            let valInfoPossibleRange = valInfo.Range
//            if isZeroRange valInfoPossibleRange then
//                ty.Range
//            else
//                unionRanges ty.Range valInfoPossibleRange

//type SynTyparDecl with

//    member this.Range =
//        match this with
//        | TyparDecl (attribs, typar) ->
//            match attribs with
//            | [] -> typar.Range
//            | _ ->
//                attribs
//                |> List.map (fun x -> x.Range)
//                |> List.append [typar.Range]
//                |> List.reduce unionRanges

//type SynValTyparDecls with

//    member this.Range =
//        match this with
//        | SynValTyparDecls (typarDecls, _, constraints) ->
//            match typarDecls with
//            | [] -> range0
//            | _ ->
//                typarDecls
//                |> List.map (fun x -> x.Range)
//                |> List.append (constraints |> List.map (fun x -> x.Range))
//                |> List.reduce unionRanges

//type SynSimplePat with

//    member this.Range =
//        match this with
//        | SynSimplePat.Id (range=m) -> m
//        | SynSimplePat.Typed (range=m) -> m
//        | SynSimplePat.Attrib (range=m) -> m

//type SynSimplePats with

//    member this.Range =
//        match this with
//        | SynSimplePats.SimplePats (range=m) -> m
//        | SynSimplePats.Typed (range=m) -> m

//type SynValData with

//    member this.Range =
//        match this with
//        | SynValData (_, valInfo, idOpt) ->
//            match idOpt with
//            | Some id ->
//                let valInfoPossibleRange = valInfo.Range
//                if isZeroRange valInfoPossibleRange then
//                    id.idRange
//                else
//                    unionRanges id.idRange valInfoPossibleRange
//            | _ -> 
//                valInfo.Range

//type SynBindingReturnInfo with

//    member this.Range =
//        match this with
//        | SynBindingReturnInfo (_, m, _) -> m

//type SynArgPats with

//    member this.Range =
//        match this with
//        | Pats pats ->
//            match pats with
//            | [] -> range0
//            | _ ->
//                pats
//                |> List.map (fun x -> x.Range)
//                |> List.reduce unionRanges

//        | NamePatPairs (_, m) -> m

//type SynInterfaceImpl with

//    member this.Range =
//        match this with
//        | InterfaceImpl (_, _, m) -> m

//type SynSimplePatAlternativeIdInfo with

//    member this.Range =
//        match this with
//        | Undecided id -> id.idRange
//        | Decided id -> id.idRange

//type SynStaticOptimizationConstraint with

//    member this.Range =
//        match this with
//        | WhenTyparTyconEqualsTycon (_, _, m) -> m
//        | WhenTyparIsStruct (_, m) -> m

//type SynUnionCaseType with

//    member this.Range =
//        match this with
//        | UnionCaseFields cases ->
//            match cases with
//            | [] -> range0
//            | _ ->
//                cases
//                |> List.map (fun x -> x.Range)
//                |> List.reduce unionRanges

//        | UnionCaseFullType (ty, valInfo) ->
//            let valInfoPossibleRange = valInfo.Range
//            if isZeroRange valInfoPossibleRange then
//                ty.Range
//            else
//                unionRanges ty.Range valInfoPossibleRange

//type SynModuleOrNamespaceKind with

//    member this.Range = range0

//type SynBinding with

//    member this.Range = 
//        match this with
//        | Binding (range=m) -> m

//type SynBindingKind with

//    member this.Range = range0

//type SynExceptionSig with

//    member this.Range =
//        match this with
//        | SynExceptionSig (range=m) -> m

//type Ident with

//    member this.Range = this.idRange            

//let longIdentRange (longId: LongIdent) =
//    longId
//    |> List.map (fun x -> x.idRange)
//    |> List.reduce unionRanges