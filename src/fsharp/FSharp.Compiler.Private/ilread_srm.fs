module rec Microsoft.FSharp.Compiler.AbstractIL.ILReader

open System
open System.IO
open System.Linq
open System.Collections.Generic
open System.Collections.Immutable
open System.Reflection
open System.Reflection.PortableExecutable
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open Microsoft.FSharp.Compiler.AbstractIL.IL  
open Microsoft.FSharp.Compiler.AbstractIL.Internal.Library
open Microsoft.FSharp.Compiler.AbstractIL.Internal.BinaryConstants

type MetadataOnlyFlag = Microsoft.FSharp.Compiler.AbstractIL.ILBinaryReader.MetadataOnlyFlag

type MetadataReader with

    member this.TryGetString(handle: StringHandle) =
        if handle.IsNil then ValueNone
        else ValueSome(this.GetString(handle))

[<Sealed>]
type cenv(
            peReader: PEReader, 
            mdReader: MetadataReader, 
            metadataOnlyFlag: MetadataOnlyFlag, 
            sigTyProvider: ISignatureTypeProvider<ILType, unit>,
            localSigTyProvider: ISignatureTypeProvider<ILLocal, unit>) =

    let typeDefCache = Dictionary()
    let typeRefCache = Dictionary()
    let typeSpecCache = Dictionary()
    let asmRefCache = Dictionary()

    let isILTypeCacheEnabled = false

    member __.IsMetadataOnly = metadataOnlyFlag = MetadataOnlyFlag.Yes

    member __.PEReader = peReader

    member __.MetadataReader = mdReader

    member __.SignatureTypeProvider = sigTyProvider

    member __.LocalSignatureTypeProvider = localSigTyProvider

    member __.CacheILType(typeDefHandle: TypeDefinitionHandle, ilType: ILType) =
        if isILTypeCacheEnabled then
            typeDefCache.Add(typeDefHandle, ilType)

    member __.CacheILType(typeRefHandle: TypeReferenceHandle, ilType: ILType) =
        if isILTypeCacheEnabled then
            typeRefCache.Add(typeRefHandle, ilType)

    member __.CacheILType(typeSpecHandle: TypeSpecificationHandle, ilType: ILType) =
        if isILTypeCacheEnabled then
            typeSpecCache.Add(typeSpecHandle, ilType)

    member __.CacheILAssemblyRef(asmRefHandle: AssemblyReferenceHandle, ilAsmRef: ILAssemblyRef) =
        asmRefCache.Add(asmRefHandle, ilAsmRef)
        
    member __.TryGetCachedILType(typeDefHandle) =
        match typeDefCache.TryGetValue(typeDefHandle) with
        | true, ilType -> ValueSome(ilType)
        | _ -> ValueNone

    member __.TryGetCachedILType(typeRefHandle) =
        match typeRefCache.TryGetValue(typeRefHandle) with
        | true, ilType -> ValueSome(ilType)
        | _ -> ValueNone
   
    member __.TryGetCachedILType(typeSpecHandle) =
        match typeSpecCache.TryGetValue(typeSpecHandle) with
        | true, ilType -> ValueSome(ilType)
        | _ -> ValueNone

    member __.TryGetCachedILAssemblyRef(asmRefHandle: AssemblyReferenceHandle) =
        match asmRefCache.TryGetValue(asmRefHandle) with
        | true, ilAsmRef -> ValueSome(ilAsmRef)
        | _ -> ValueNone

let mkVersionTuple (v: Version) =
    (uint16 v.Major, uint16 v.Minor, uint16 v.Build, uint16 v.Revision)
    
let mkILMemberAccess (attributes: TypeAttributes) =
    if int (attributes &&& TypeAttributes.Public) <> 0 then
        ILMemberAccess.Public
    elif int (attributes &&& TypeAttributes.NestedFamily) <> 0 then
        ILMemberAccess.Family
    elif int (attributes &&& TypeAttributes.NestedFamANDAssem) <> 0 then
        ILMemberAccess.FamilyAndAssembly
    elif int (attributes &&& TypeAttributes.NestedFamORAssem) <> 0 then
        ILMemberAccess.FamilyOrAssembly
    elif int (attributes &&& TypeAttributes.NestedAssembly) <> 0 then
        ILMemberAccess.Assembly
    else
        ILMemberAccess.Private

let mkILTypeDefAccess (attributes: TypeAttributes) =
    let ilMemberAccess = mkILMemberAccess attributes
    match ilMemberAccess with
    | ILMemberAccess.Public -> ILTypeDefAccess.Public
    | ILMemberAccess.Private ->
        if int (attributes &&& TypeAttributes.NestedPrivate) <> 0 then
            ILTypeDefAccess.Nested(ILMemberAccess.Private)
        else
            ILTypeDefAccess.Private
    | _ ->
        ILTypeDefAccess.Nested(ilMemberAccess)

let mkILAssemblyLongevity (flags: AssemblyFlags) =
    let  masked = int flags &&& 0x000e
    if   masked = 0x0000 then ILAssemblyLongevity.Unspecified
    elif masked = 0x0002 then ILAssemblyLongevity.Library
    elif masked = 0x0004 then ILAssemblyLongevity.PlatformAppDomain
    elif masked = 0x0006 then ILAssemblyLongevity.PlatformProcess
    elif masked = 0x0008 then ILAssemblyLongevity.PlatformSystem
    else                      ILAssemblyLongevity.Unspecified

let mkILTypeDefLayoutInfo (layout: TypeLayout) =
    if layout.IsDefault then
        { Size = None; Pack = None }
    else
        { Size = Some(layout.Size); Pack = Some(uint16 layout.PackingSize) }

let mkILTypeDefLayout (attributes: TypeAttributes) (layout: TypeLayout) =
    match attributes &&& TypeAttributes.LayoutMask with
    | TypeAttributes.SequentialLayout ->
        ILTypeDefLayout.Sequential(mkILTypeDefLayoutInfo layout)
    | TypeAttributes.ExplicitLayout ->
        ILTypeDefLayout.Explicit(mkILTypeDefLayoutInfo layout)
    | _ ->
        ILTypeDefLayout.Auto

let mkILSecurityAction (declSecurityAction: DeclarativeSecurityAction) =
    match declSecurityAction with
    | DeclarativeSecurityAction.Demand -> ILSecurityAction.Demand
    | DeclarativeSecurityAction.Assert -> ILSecurityAction.Assert
    | DeclarativeSecurityAction.Deny -> ILSecurityAction.Deny
    | DeclarativeSecurityAction.PermitOnly -> ILSecurityAction.PermitOnly
    | DeclarativeSecurityAction.LinkDemand -> ILSecurityAction.LinkCheck
    | DeclarativeSecurityAction.InheritanceDemand -> ILSecurityAction.InheritCheck
    | DeclarativeSecurityAction.RequestMinimum -> ILSecurityAction.ReqMin
    | DeclarativeSecurityAction.RequestOptional -> ILSecurityAction.ReqOpt
    | DeclarativeSecurityAction.RequestRefuse -> ILSecurityAction.ReqRefuse
    | _ ->
        // Comment below is from System.Reflection.Metadata
        // Wait for an actual need before exposing these. They all have ilasm keywords, but some are missing from the CLI spec and 
        // and none are defined in System.Security.Permissions.SecurityAction.
        //Request = 0x0001,
        //PrejitGrant = 0x000B,
        //PrejitDeny = 0x000C,
        //NonCasDemand = 0x000D,
        //NonCasLinkDemand = 0x000E,
        //NonCasInheritanceDemand = 0x000F,
        match int declSecurityAction with
        | 0x0001 -> ILSecurityAction.Request
        | 0x000b -> ILSecurityAction.PreJitGrant
        | 0x000c -> ILSecurityAction.PreJitDeny
        | 0x000d -> ILSecurityAction.NonCasDemand
        | 0x000e -> ILSecurityAction.NonCasLinkDemand
        | 0x000f -> ILSecurityAction.NonCasInheritance
        | 0x0010 -> ILSecurityAction.LinkDemandChoice
        | 0x0011 -> ILSecurityAction.InheritanceDemandChoice
        | 0x0012 -> ILSecurityAction.DemandChoice
        | x -> failwithf "Invalid DeclarativeSecurityAction: %i" x

let mkILThisConvention (sigHeader: SignatureHeader) =
    if sigHeader.IsInstance then
        if sigHeader.HasExplicitThis then ILThisConvention.InstanceExplicit
        else ILThisConvention.Instance
    else ILThisConvention.Static
    
let mkILCallingConv (sigHeader: SignatureHeader) =
    let ilThisConvention = mkILThisConvention sigHeader

    let ilArgConvention =
        match sigHeader.CallingConvention with
        | SignatureCallingConvention.Default -> ILArgConvention.Default
        | SignatureCallingConvention.CDecl -> ILArgConvention.CDecl
        | SignatureCallingConvention.StdCall -> ILArgConvention.StdCall
        | SignatureCallingConvention.ThisCall -> ILArgConvention.ThisCall
        | SignatureCallingConvention.FastCall -> ILArgConvention.FastCall
        | SignatureCallingConvention.VarArgs -> ILArgConvention.VarArg
        | _ -> failwithf "Invalid Signature Calling Convention: %A" sigHeader.CallingConvention

    // Optimize allocations.
    if ilThisConvention = ILThisConvention.Instance && ilArgConvention = ILArgConvention.Default then
        ILCallingConv.Instance
    elif ilThisConvention = ILThisConvention.Static && ilArgConvention = ILArgConvention.Default then
        ILCallingConv.Static
    else
        ILCallingConv.Callconv(ilThisConvention, ilArgConvention)

let mkPInvokeCallingConvention (methImportAttributes: MethodImportAttributes) =
    match methImportAttributes &&& MethodImportAttributes.CallingConventionMask with
    | MethodImportAttributes.CallingConventionCDecl ->
        PInvokeCallingConvention.Cdecl
    | MethodImportAttributes.CallingConventionStdCall ->
        PInvokeCallingConvention.Stdcall
    | MethodImportAttributes.CallingConventionThisCall ->
        PInvokeCallingConvention.Thiscall
    | MethodImportAttributes.CallingConventionFastCall ->
        PInvokeCallingConvention.Fastcall
    | MethodImportAttributes.CallingConventionWinApi ->
        PInvokeCallingConvention.WinApi
    | _ ->
        PInvokeCallingConvention.None

let mkPInvokeCharEncoding (methImportAttributes: MethodImportAttributes) =
    match methImportAttributes &&& MethodImportAttributes.CharSetMask with
    | MethodImportAttributes.CharSetAnsi ->
        PInvokeCharEncoding.Ansi
    | MethodImportAttributes.CharSetUnicode ->
        PInvokeCharEncoding.Unicode
    | MethodImportAttributes.CharSetAuto ->
        PInvokeCharEncoding.Auto
    | _ ->
        PInvokeCharEncoding.None

let mkPInvokeThrowOnUnmappableChar (methImportAttrs: MethodImportAttributes) =
    match methImportAttrs &&& MethodImportAttributes.ThrowOnUnmappableCharMask with
    | MethodImportAttributes.ThrowOnUnmappableCharEnable ->
        PInvokeThrowOnUnmappableChar.Enabled
    | MethodImportAttributes.ThrowOnUnmappableCharDisable ->
        PInvokeThrowOnUnmappableChar.Disabled
    | _ -> 
        PInvokeThrowOnUnmappableChar.UseAssembly

let mkPInvokeCharBestFit (methImportAttrs: MethodImportAttributes) =
    match methImportAttrs &&& MethodImportAttributes.BestFitMappingMask with
    | MethodImportAttributes.BestFitMappingEnable ->
        PInvokeCharBestFit.Enabled
    | MethodImportAttributes.BestFitMappingDisable ->
        PInvokeCharBestFit.Disabled
    | _ ->
        PInvokeCharBestFit.UseAssembly

let mkILTypeFunctionPointer (sigHeader: SignatureHeader) argTypes returnType =
    let callingSig =
        {
            CallingConv = mkILCallingConv sigHeader
            ArgTypes = argTypes
            ReturnType = returnType
        }
    ILType.FunctionPointer(callingSig)

let mkILTypeTypeVar index =
    ILType.TypeVar(uint16 index)

let mkILTypeModified isRequired typeRef unmodifiedType =
    ILType.Modified(isRequired, typeRef, unmodifiedType)
        
let mkILTypePrimitive (primitiveTypeCode: PrimitiveTypeCode) (ilg: ILGlobals) =
    match primitiveTypeCode with
    | PrimitiveTypeCode.Boolean -> ilg.typ_Bool
    | PrimitiveTypeCode.Byte -> ilg.typ_Byte
    | PrimitiveTypeCode.Char -> ilg.typ_Char
    | PrimitiveTypeCode.Double -> ilg.typ_Double
    | PrimitiveTypeCode.Int16 -> ilg.typ_Int16
    | PrimitiveTypeCode.Int32 -> ilg.typ_Int32
    | PrimitiveTypeCode.Int64 -> ilg.typ_Int64
    | PrimitiveTypeCode.IntPtr -> ilg.typ_IntPtr
    | PrimitiveTypeCode.Object -> ilg.typ_Object
    | PrimitiveTypeCode.SByte -> ilg.typ_SByte
    | PrimitiveTypeCode.Single -> ilg.typ_Single
    | PrimitiveTypeCode.String -> ilg.typ_String
    | PrimitiveTypeCode.TypedReference -> ilg.typ_TypedReference
    | PrimitiveTypeCode.UInt16 -> ilg.typ_UInt16
    | PrimitiveTypeCode.UInt32 -> ilg.typ_UInt32
    | PrimitiveTypeCode.UInt64 -> ilg.typ_UInt64
    | PrimitiveTypeCode.UIntPtr -> ilg.typ_UIntPtr
    | PrimitiveTypeCode.Void -> ILType.Void
    | _ -> failwithf "Invalid Primitive Type Code: %A" primitiveTypeCode

let mkILTypeGeneric typeRef boxity typeArgs =
    let ilTypeSpec = ILTypeSpec.Create(typeRef, typeArgs)
    mkILTy boxity ilTypeSpec

let mkILTypeArray elementType (shape: ArrayShape) =
    let lowerBounds = shape.LowerBounds
    let sizes = shape.Sizes
    let rank = shape.Rank
    let shape = 
        let dim i =
          (if i < lowerBounds.Length then Some (Seq.item i lowerBounds) else None), 
          (if i < sizes.Length then Some (Seq.item i sizes) else None)
        ILArrayShape (List.init rank dim)
    mkILArrTy (elementType, shape)

[<Sealed>]
type SignatureTypeProvider(ilg: ILGlobals) =

    member val cenv : cenv = Unchecked.defaultof<_> with get, set

    interface ISignatureTypeProvider<ILType, unit> with

        member __.GetFunctionPointerType(si) =
            mkILTypeFunctionPointer si.Header (si.ParameterTypes |> Seq.toList) si.ReturnType

        member __.GetGenericMethodParameter(_, index) =
            mkILTypeTypeVar index

        member __.GetGenericTypeParameter(_, index) =
            mkILTypeTypeVar index

        member __.GetModifiedType(modifier, unmodifiedType, isRequired) =
            mkILTypeModified isRequired modifier.TypeRef unmodifiedType

        member __.GetPinnedType(elementType) = elementType

        member this.GetTypeFromSpecification(_, _, typeSpecHandle, _) =
            readILTypeFromTypeSpecification this.cenv typeSpecHandle
            
    interface ISimpleTypeProvider<ILType> with

        member __.GetPrimitiveType(typeCode) =
            mkILTypePrimitive typeCode ilg
            
        member this.GetTypeFromDefinition(_, typeDefHandle, _) =
            readILTypeFromTypeDefinition this.cenv typeDefHandle

        member this.GetTypeFromReference(_, typeRefHandle, _) =
            readILTypeFromTypeReference this.cenv typeRefHandle

    interface IConstructedTypeProvider<ILType> with

        member __.GetGenericInstantiation(genericType, typeArgs) =
            mkILTypeGeneric genericType.TypeRef genericType.Boxity (typeArgs |> List.ofSeq)

        member __.GetArrayType(elementType, shape) =
            mkILTypeArray elementType shape

        member __.GetByReferenceType(elementType) =
            ILType.Byref(elementType)

        member __.GetPointerType(elementType) =
            ILType.Ptr(elementType)

    interface ISZArrayTypeProvider<ILType> with

        member __.GetSZArrayType(elementType) =
            mkILArr1DTy elementType

[<Sealed>]
type LocalSignatureTypeProvider(ilg: ILGlobals) =

    member val cenv : cenv = Unchecked.defaultof<_> with get, set

    interface ISignatureTypeProvider<ILLocal, unit> with

        member __.GetFunctionPointerType(si) =
            {
                IsPinned = false
                Type = mkILTypeFunctionPointer si.Header (si.ParameterTypes |> Seq.map (fun x -> x.Type) |> Seq.toList) si.ReturnType.Type
                DebugInfo = None
            }

        member __.GetGenericMethodParameter(_, index) =
            {
                IsPinned = false
                Type = mkILTypeTypeVar index
                DebugInfo = None
            }

        member __.GetGenericTypeParameter(_, index) =
            {
                IsPinned = false
                Type = mkILTypeTypeVar index
                DebugInfo = None
            }

        member __.GetModifiedType(modifier, unmodifiedType, isRequired) =
            {
                IsPinned = false
                Type = mkILTypeModified isRequired modifier.Type.TypeRef unmodifiedType.Type
                DebugInfo = None
            }

        member __.GetPinnedType(elementType) =
            {
                IsPinned = true
                Type = elementType.Type
                DebugInfo = None
            }

        member this.GetTypeFromSpecification(_, _, typeSpecHandle, _) =
            {
                IsPinned = false
                Type = readILTypeFromTypeSpecification this.cenv typeSpecHandle
                DebugInfo = None
            }
            
    interface ISimpleTypeProvider<ILLocal> with

        member __.GetPrimitiveType(typeCode) =
            {
                IsPinned = false
                Type = mkILTypePrimitive typeCode ilg
                DebugInfo = None
            }
            
        member this.GetTypeFromDefinition(_, typeDefHandle, _) =
            {
                IsPinned = false
                Type = readILTypeFromTypeDefinition this.cenv typeDefHandle
                DebugInfo = None
            }    

        member this.GetTypeFromReference(_, typeRefHandle, _) =
            {
                IsPinned = false
                Type = readILTypeFromTypeReference this.cenv typeRefHandle
                DebugInfo = None
            } 
            
    interface IConstructedTypeProvider<ILLocal> with

        member __.GetGenericInstantiation(genericType, typeArgs) =
            {
                IsPinned = false
                Type = mkILTypeGeneric genericType.Type.TypeRef genericType.Type.Boxity (typeArgs |> Seq.map (fun x -> x.Type) |> List.ofSeq)
                DebugInfo = None
            }

        member __.GetArrayType(elementType, shape) =
            {
                IsPinned = false
                Type = mkILTypeArray elementType.Type shape
                DebugInfo = None
            }

        member __.GetByReferenceType(elementType) =
            {
                IsPinned = false
                Type = ILType.Byref(elementType.Type)
                DebugInfo = None
            }

        member __.GetPointerType(elementType) =
            {
                IsPinned = false
                Type = ILType.Ptr(elementType.Type)
                DebugInfo = None
            }

    interface ISZArrayTypeProvider<ILLocal> with

        member __.GetSZArrayType(elementType) =
            {
                IsPinned = false
                Type =  mkILArr1DTy elementType.Type
                DebugInfo = None
            }

let rec readILScopeRef (cenv: cenv) (handle: EntityHandle) =
    let mdReader = cenv.MetadataReader

    match handle.Kind with
    | HandleKind.AssemblyFile ->
        let asmFile = mdReader.GetAssemblyFile(AssemblyFileHandle.op_Explicit(handle))
        ILScopeRef.Module(readILModuleRefFromAssemblyFile cenv asmFile)

    | HandleKind.AssemblyReference ->
        ILScopeRef.Assembly(readILAssemblyRefFromAssemblyReference cenv (AssemblyReferenceHandle.op_Explicit(handle)))

    | HandleKind.ModuleReference ->
        let modRef = mdReader.GetModuleReference(ModuleReferenceHandle.op_Explicit(handle))
        ILScopeRef.Module(readILModuleRefFromModuleReference cenv modRef)

    | HandleKind.TypeReference ->
        let typeRef = mdReader.GetTypeReference(TypeReferenceHandle.op_Explicit(handle))
        readILScopeRef cenv typeRef.ResolutionScope

    | HandleKind.ExportedType ->
        let exportedTy = mdReader.GetExportedType(ExportedTypeHandle.op_Explicit(handle))
        readILScopeRef cenv exportedTy.Implementation

    | HandleKind.ModuleDefinition ->
        ILScopeRef.Local

    | _ ->
        failwithf "Invalid Handle Kind: %A" handle.Kind

let readILAssemblyRefFromAssemblyReferenceUncached (cenv: cenv) (asmRefHandle: AssemblyReferenceHandle) =
    let mdReader = cenv.MetadataReader

    let asmRef = mdReader.GetAssemblyReference(asmRefHandle)
    let name = mdReader.GetString(asmRef.Name)
    let flags = asmRef.Flags

    let hash = 
        let hashValue = asmRef.HashValue
        if hashValue.IsNil then None
        else Some(mdReader.GetBlobBytes(hashValue))

    let publicKey =
        if asmRef.PublicKeyOrToken.IsNil then None
        else 
            let bytes = mdReader.GetBlobBytes(asmRef.PublicKeyOrToken)
            let publicKey = 
                if int (flags &&& AssemblyFlags.PublicKey) <> 0 then
                    PublicKey(bytes)
                else
                    PublicKeyToken(bytes)
            Some(publicKey)

    let retargetable = int (flags &&& AssemblyFlags.Retargetable) <> 0

    let version = Some(mkVersionTuple asmRef.Version)

    let locale =
        let locale = mdReader.GetString(asmRef.Culture)
        if String.IsNullOrWhiteSpace(locale) then None
        else Some(locale)

    ILAssemblyRef.Create(name, hash, publicKey, retargetable, version, locale)

let readILAssemblyRefFromAssemblyReference (cenv: cenv) (asmRefHandle: AssemblyReferenceHandle) =
    match cenv.TryGetCachedILAssemblyRef(asmRefHandle) with
    | ValueSome(ilAsmRef) -> ilAsmRef
    | _ ->
        let ilAsmRef = readILAssemblyRefFromAssemblyReferenceUncached cenv asmRefHandle
        cenv.CacheILAssemblyRef(asmRefHandle, ilAsmRef)
        ilAsmRef

let readILModuleRefFromAssemblyFile (cenv: cenv) (asmFile: AssemblyFile) =
    let mdReader = cenv.MetadataReader

    let name = mdReader.GetString(asmFile.Name)

    let hash = 
        let hashValue = asmFile.HashValue
        if hashValue.IsNil then None
        else Some(mdReader.GetBlobBytes(hashValue))

    ILModuleRef.Create(name, asmFile.ContainsMetadata, hash) 

let readILType (cenv: cenv) (handle: EntityHandle) : ILType =
    match handle.Kind with
    | HandleKind.TypeReference ->
        readILTypeFromTypeReference cenv (TypeReferenceHandle.op_Explicit(handle))

    | HandleKind.TypeDefinition ->
        readILTypeFromTypeDefinition cenv (TypeDefinitionHandle.op_Explicit(handle))

    | HandleKind.TypeSpecification ->
        readILTypeFromTypeSpecification cenv (TypeSpecificationHandle.op_Explicit(handle))

    | _ ->
        failwithf "Invalid Handle Kind: %A" handle.Kind

let readILModuleRefFromModuleReference (cenv: cenv) (modRef: ModuleReference) =
    let name = cenv.MetadataReader.GetString(modRef.Name)
    ILModuleRef.Create(name, hasMetadata = true, hash = None)

let readILTypeRefFromTypeReference (cenv: cenv) (typeRef: TypeReference) =
    let mdReader = cenv.MetadataReader

    let ilScopeRef = readILScopeRef cenv typeRef.ResolutionScope

    let name = mdReader.GetString(typeRef.Name)
    let namespac = mdReader.GetString(typeRef.Namespace)

    ILTypeRef.Create(ilScopeRef, [], namespac + "." + name)

let readILTypeFromTypeReference (cenv: cenv) (typeRefHandle: TypeReferenceHandle) =
    match cenv.TryGetCachedILType(typeRefHandle) with
    | ValueSome(ilType) -> ilType
    | _ ->
        let mdReader = cenv.MetadataReader

        let typeRef = mdReader.GetTypeReference(typeRefHandle)
        let ilTypeRef = readILTypeRefFromTypeReference cenv typeRef
        let ilTypeSpec = ILTypeSpec.Create(ilTypeRef, ILGenericArgs.Empty)

        let ilType = mkILTy AsObject (* AsObject probably not nok *) ilTypeSpec
        cenv.CacheILType(typeRefHandle, ilType)
        ilType

let rec readILTypeRefFromTypeDefinition (cenv: cenv) (typeDef: TypeDefinition) : ILTypeRef =
    let mdReader = cenv.MetadataReader

    let enclosing =
        if typeDef.IsNested then
            let parentTypeDefHandle = typeDef.GetDeclaringType()
            let parentTypeDef = mdReader.GetTypeDefinition(parentTypeDefHandle)
            let ilTypeRef = readILTypeRefFromTypeDefinition cenv parentTypeDef
            ilTypeRef.Enclosing @ [ ilTypeRef.Name ]
        else
            []
  
    let name =
        let name = mdReader.GetString(typeDef.Name)
        if enclosing.Length > 0 then 
            name
        else
            let namespac = mdReader.GetString(typeDef.Namespace)
            namespac + "." + name

    ILTypeRef.Create(ILScopeRef.Local, enclosing, name)

let readILTypeFromTypeDefinition (cenv: cenv) (typeDefHandle: TypeDefinitionHandle) =
    match cenv.TryGetCachedILType(typeDefHandle) with
    | ValueSome(ilType) -> ilType
    | _ ->
        let mdReader = cenv.MetadataReader

        let typeDef = mdReader.GetTypeDefinition(typeDefHandle)
        let ilTypeRef = readILTypeRefFromTypeDefinition cenv typeDef

        let ilGenericArgs = 
            // TODO: Confirm if this is right?
            typeDef.GetGenericParameters()
            |> Seq.mapi (fun i _ -> mkILTyvarTy (uint16 i))
            |> List.ofSeq

        let ilTypeSpec = ILTypeSpec.Create(ilTypeRef, ilGenericArgs)

        let boxity = 
            if int (typeDef.Attributes &&& TypeAttributes.Class) <> 0 then AsObject
            else AsValue

        let ilType = mkILTy boxity ilTypeSpec
        cenv.CacheILType(typeDefHandle, ilType)
        ilType

let readILTypeFromTypeSpecification (cenv: cenv) (typeSpecHandle: TypeSpecificationHandle) =
    match cenv.TryGetCachedILType(typeSpecHandle) with
    | ValueSome(ilType) -> ilType
    | _ ->
        let mdReader = cenv.MetadataReader

        let typeSpec = mdReader.GetTypeSpecification(typeSpecHandle)

        let ilType = typeSpec.DecodeSignature(cenv.SignatureTypeProvider, ())
        cenv.CacheILType(typeSpecHandle, ilType)
        ilType

let readILGenericParameterDef (cenv: cenv) (genParamHandle: GenericParameterHandle) : ILGenericParameterDef =
    let mdReader = cenv.MetadataReader

    let genParam = mdReader.GetGenericParameter(genParamHandle)
    let attributes = genParam.Attributes

    let constraints =
        genParam.GetConstraints()
        |> Seq.map (fun genParamCnstrHandle ->
            let genParamCnstr = mdReader.GetGenericParameterConstraint(genParamCnstrHandle)
            readILType cenv genParamCnstr.Type
        )
        |> List.ofSeq     

    let variance = 
        if int (attributes &&& GenericParameterAttributes.Covariant) <> 0 then
            ILGenericVariance.CoVariant
        elif int (attributes &&& GenericParameterAttributes.Contravariant) <> 0 then
            ILGenericVariance.ContraVariant
        else
            ILGenericVariance.NonVariant

    {
        Name = mdReader.GetString(genParam.Name)
        Constraints = constraints
        Variance = variance
        HasReferenceTypeConstraint = int (attributes &&& GenericParameterAttributes.ReferenceTypeConstraint) <> 0
        HasNotNullableValueTypeConstraint = int (attributes &&& GenericParameterAttributes.NotNullableValueTypeConstraint) <> 0
        HasDefaultConstructorConstraint = int (attributes &&& GenericParameterAttributes.DefaultConstructorConstraint) <> 0
        CustomAttrsStored = readILAttributesStored cenv (genParam.GetCustomAttributes())
        MetadataIndex = MetadataTokens.GetRowNumber(GenericParameterHandle.op_Implicit(genParamHandle))
    }

let readILGenericParameterDefs (cenv: cenv) (genParamHandles: GenericParameterHandleCollection) =
    genParamHandles
    |> Seq.map (readILGenericParameterDef cenv)
    |> List.ofSeq

let rec readILMethodSpec (cenv: cenv) (handle: EntityHandle) : ILMethodSpec =
    let mdReader = cenv.MetadataReader
    match handle.Kind with
    | HandleKind.MemberReference ->
        let memberRef = mdReader.GetMemberReference(MemberReferenceHandle.op_Explicit(handle))
        let si = memberRef.DecodeMethodSignature(cenv.SignatureTypeProvider, ())
        
        let name = mdReader.GetString(memberRef.Name)
        let enclILTy = readILType cenv memberRef.Parent
        let ilCallingConv = mkILCallingConv si.Header
        let genericArity = 0

        let ilMethodRef = ILMethodRef.Create(enclILTy.TypeRef, ilCallingConv, name, genericArity, si.ParameterTypes |> List.ofSeq, si.ReturnType)

        ILMethodSpec.Create(enclILTy, ilMethodRef, [])

    | HandleKind.MethodDefinition ->
        let methodDefHandle = MethodDefinitionHandle.op_Explicit(handle)
        let methodDef = mdReader.GetMethodDefinition(methodDefHandle)
        let si = methodDef.DecodeSignature(cenv.SignatureTypeProvider, ())

        let name = mdReader.GetString(methodDef.Name)
        let enclILTy = readILTypeFromTypeDefinition cenv (methodDef.GetDeclaringType())
        let ilCallingConv =
            if int (methodDef.Attributes &&& MethodAttributes.Static) <> 0 then
                ILCallingConv.Static
            else
                ILCallingConv.Instance
        let genericArity = si.GenericParameterCount

        let ilMethodRef = ILMethodRef.Create(enclILTy.TypeRef, ilCallingConv, name, genericArity, si.ParameterTypes |> List.ofSeq, si.ReturnType)

        let ilGenericArgs =
            // TODO: Confirm if this is right?
            let enclILGenericArgCount = enclILTy.GenericArgs.Length
            methodDef.GetGenericParameters()
            |> Seq.mapi (fun i _ -> mkILTyvarTy (uint16 (enclILGenericArgCount + i)))
            |> List.ofSeq
        
        ILMethodSpec.Create(enclILTy, ilMethodRef, ilGenericArgs)

    | _ ->
        failwithf "Invalid Entity Handle Kind: %A" handle.Kind

let readILSecurityDecl (cenv: cenv) (declSecurityAttributeHandle: DeclarativeSecurityAttributeHandle) =
    let mdReader = cenv.MetadataReader

    let declSecurityAttribute = mdReader.GetDeclarativeSecurityAttribute(declSecurityAttributeHandle)

    let bytes = mdReader.GetBlobBytes(declSecurityAttribute.PermissionSet)
    ILSecurityDecl(mkILSecurityAction declSecurityAttribute.Action, bytes)

let readILSecurityDeclsStored (cenv: cenv) (declSecurityAttributeHandles: DeclarativeSecurityAttributeHandleCollection) =
    mkILSecurityDeclsReader (fun _ ->
        let securityDeclsArray = Array.zeroCreate declSecurityAttributeHandles.Count
        let mutable i = 0
        for declSecurityAttributeHandle in declSecurityAttributeHandles do
            securityDeclsArray.[i] <- readILSecurityDecl cenv declSecurityAttributeHandle
            i <- i + 1
        securityDeclsArray
    )

let readILAttribute (cenv: cenv) (customAttrHandle: CustomAttributeHandle) =
    let mdReader = cenv.MetadataReader
    let customAttr = mdReader.GetCustomAttribute(customAttrHandle)

    let bytes = 
        if customAttr.Value.IsNil then [||]
        else mdReader.GetBlobBytes(customAttr.Value)

    let elements = [] // Why are we not putting elements in here?
    ILAttribute.Encoded(readILMethodSpec cenv customAttr.Constructor, bytes, elements)

let readILAttributesStored (cenv: cenv) (customAttrs: CustomAttributeHandleCollection) =
    mkILCustomAttrsReader (fun _ ->
        let customAttrsArray = Array.zeroCreate customAttrs.Count
        let mutable i = 0
        for customAttrHandle in customAttrs do
            customAttrsArray.[i] <- readILAttribute cenv customAttrHandle
            i <- i + 1
        customAttrsArray
    )

let rec readILNestedExportedTypes (cenv: cenv) (handle: EntityHandle) =
    let mdReader = cenv.MetadataReader
    match handle.Kind with
    | HandleKind.TypeDefinition ->
        let typeDefHandle = TypeDefinitionHandle.op_Explicit(handle)
        let typeDef = mdReader.GetTypeDefinition(typeDefHandle)

        let nestedLazy =
            lazy
                typeDef.GetNestedTypes()
                |> Seq.map (fun typeDefHandle ->
                    let typeDef = mdReader.GetTypeDefinition(typeDefHandle)
                
                    {
                        Name = mdReader.GetString(typeDef.Name)
                        Access = mkILMemberAccess typeDef.Attributes
                        Nested = readILNestedExportedTypes cenv (TypeDefinitionHandle.op_Implicit(typeDefHandle))
                        CustomAttrsStored = readILAttributesStored cenv (typeDef.GetCustomAttributes())
                        MetadataIndex = MetadataTokens.GetRowNumber(TypeDefinitionHandle.op_Implicit(typeDefHandle))
                    }
                )
                |> List.ofSeq

        mkILNestedExportedTypesLazy nestedLazy

    | _ ->
        mkILNestedExportedTypes []

let readILExportedType (cenv: cenv) (exportedTyHandle: ExportedTypeHandle) =
    let mdReader = cenv.MetadataReader
    let exportedTy = mdReader.GetExportedType(exportedTyHandle)

    {
        ScopeRef = readILScopeRef cenv exportedTy.Implementation
        Name = mdReader.GetString(exportedTy.Name)
        Attributes = exportedTy.Attributes
        Nested = readILNestedExportedTypes cenv exportedTy.Implementation
        CustomAttrsStored = readILAttributesStored cenv (exportedTy.GetCustomAttributes())
        MetadataIndex = MetadataTokens.GetRowNumber(ExportedTypeHandle.op_Implicit(exportedTyHandle))
    }

let readILExportedTypes (cenv: cenv) (exportedTys: ExportedTypeHandleCollection) =
    let f =
        lazy
            [
                for exportedTyHandle in exportedTys do
                    yield readILExportedType cenv exportedTyHandle
            ]
    mkILExportedTypesLazy f

let readILAssemblyManifest (cenv: cenv) (entryPointToken: int) =
    let mdReader = cenv.MetadataReader

    let asmDef = mdReader.GetAssemblyDefinition()

    let publicKey =
        let bytes = 
            asmDef.PublicKey
            |> mdReader.GetBlobBytes
        if bytes.Length = 0 then None
        else Some(bytes)

    let locale =
        let str =
            asmDef.Culture
            |> mdReader.GetString
        if str.Length = 0 then None
        else Some(str)

    let flags = asmDef.Flags

    let entrypointElsewhere =
        let handle = MetadataTokens.EntityHandle(entryPointToken)
        if handle.IsNil then None
        else
            match handle.Kind with
            | HandleKind.AssemblyFile -> 
                let asmFile = mdReader.GetAssemblyFile(AssemblyFileHandle.op_Explicit(handle))
                Some(readILModuleRefFromAssemblyFile cenv asmFile)
            | _ -> None

    {
        Name = mdReader.GetString(asmDef.Name)
        AuxModuleHashAlgorithm = int asmDef.HashAlgorithm
        SecurityDeclsStored = readILSecurityDeclsStored cenv (asmDef.GetDeclarativeSecurityAttributes())
        PublicKey = publicKey
        Version = Some(mkVersionTuple asmDef.Version)
        Locale = locale
        CustomAttrsStored = readILAttributesStored cenv (asmDef.GetCustomAttributes())
        AssemblyLongevity = mkILAssemblyLongevity flags
        DisableJitOptimizations = int (flags &&& AssemblyFlags.DisableJitCompileOptimizer) <> 0
        JitTracking = int (flags &&& AssemblyFlags.EnableJitCompileTracking) <> 0
        IgnoreSymbolStoreSequencePoints = (int flags &&& 0x2000) <> 0 // Not listed in AssemblyFlags
        Retargetable = int (flags &&& AssemblyFlags.Retargetable) <> 0
        ExportedTypes = readILExportedTypes cenv mdReader.ExportedTypes
        EntrypointElsewhere = entrypointElsewhere
        MetadataIndex = 1 // always one
    }

let readILNativeResources (peReader: PEReader) =
    peReader.PEHeaders.SectionHeaders
    |> Seq.choose (fun s ->
        // TODO: Is this right?
        if s.Name.Equals(".rsrc", StringComparison.OrdinalIgnoreCase) then
            let memBlock = peReader.GetSectionData(s.VirtualAddress)
            let bytes = memBlock.GetContent().ToArray() // We should never do this. ILNativeResource.Out should just take an immutable array...
            ILNativeResource.Out(bytes)
            |> Some
        else
            None
    )
    |> Seq.toList

let tryReadILFieldInit (cenv: cenv) (constantHandle: ConstantHandle) =
    if constantHandle.IsNil then None
    else
        let mdReader = cenv.MetadataReader

        let constant = mdReader.GetConstant(constantHandle)
        let blobReader = mdReader.GetBlobReader(constant.Value)
        match constant.TypeCode with
        | ConstantTypeCode.Boolean -> ILFieldInit.Bool(blobReader.ReadBoolean()) |> Some
        | ConstantTypeCode.Byte -> ILFieldInit.UInt8(blobReader.ReadByte()) |> Some
        | ConstantTypeCode.Char -> ILFieldInit.Char(blobReader.ReadChar() |> uint16) |> Some // Why does ILFieldInit.Char not just take a char?
        | ConstantTypeCode.Double -> ILFieldInit.Double(blobReader.ReadDouble()) |> Some
        | ConstantTypeCode.Int16 -> ILFieldInit.Int16(blobReader.ReadInt16()) |> Some
        | ConstantTypeCode.Int32 -> ILFieldInit.Int32(blobReader.ReadInt32()) |> Some
        | ConstantTypeCode.Int64 -> ILFieldInit.Int64(blobReader.ReadInt64()) |> Some
        | ConstantTypeCode.SByte -> ILFieldInit.Int8(blobReader.ReadSByte()) |> Some
        | ConstantTypeCode.Single -> ILFieldInit.Single(blobReader.ReadSingle()) |> Some
        | ConstantTypeCode.String -> ILFieldInit.String(blobReader.ReadUTF16(blobReader.Length)) |> Some
        | ConstantTypeCode.UInt16 -> ILFieldInit.UInt16(blobReader.ReadUInt16()) |> Some
        | ConstantTypeCode.UInt32 -> ILFieldInit.UInt32(blobReader.ReadUInt32()) |> Some
        | ConstantTypeCode.UInt64 -> ILFieldInit.UInt64(blobReader.ReadUInt64()) |> Some
        | ConstantTypeCode.NullReference -> ILFieldInit.Null |> Some
        | _ -> (* possible warning? *) None

let ilNativeTypeLookup = (ILNativeTypeMap.Value |> Seq.map (fun x -> x)).ToDictionary((fun (key, _) -> key), fun (_, value) -> value) // This looks terrible. Cleanup later.
let ilVariantTypeMap = (ILVariantTypeMap.Value |> Seq.map (fun x -> x)).ToDictionary((fun (_, key) -> key), fun (value, _) -> value) // This looks terrible. Cleanup later.

let rec mkILVariantType (kind: int) =
    match ilVariantTypeMap.TryGetValue(kind) with
    | true, ilVariantType -> ilVariantType
    | _ ->
        match kind with
        | _ when (kind &&& vt_ARRAY) <> 0 -> ILNativeVariant.Array(mkILVariantType (kind &&& (~~~vt_ARRAY)))
        | _ when (kind &&& vt_VECTOR) <> 0 -> ILNativeVariant.Vector(mkILVariantType (kind &&& (~~~vt_VECTOR)))
        | _ when (kind &&& vt_BYREF) <> 0 -> ILNativeVariant.Byref(mkILVariantType (kind &&& (~~~vt_BYREF)))
        | _ -> (* possible warning? *) ILNativeVariant.Empty

let rec readILNativeType (cenv: cenv) (reader: byref<BlobReader>) =
    let kind = reader.ReadByte()
    match ilNativeTypeLookup.TryGetValue(kind) with
    | true, ilNativeType -> ilNativeType
    | _ ->
        match kind with
        | 0x0uy -> ILNativeType.Empty
        | _ when kind = nt_FIXEDSYSSTRING -> ILNativeType.FixedSysString(reader.ReadCompressedInteger())
        | _ when kind = nt_FIXEDARRAY -> ILNativeType.FixedArray(reader.ReadCompressedInteger())

        | _ when kind = nt_SAFEARRAY ->
            if reader.RemainingBytes = 0 then
                ILNativeType.SafeArray(ILNativeVariant.Empty, None)
            else
                let variantKind = reader.ReadCompressedInteger()
                let ilVariantType = mkILVariantType variantKind
                if reader.RemainingBytes = 0 then
                    ILNativeType.SafeArray(ilVariantType, None)
                else
                    let s = reader.ReadUTF16(reader.Length)
                    ILNativeType.SafeArray(ilVariantType, Some(s))

        | _ when kind = nt_ARRAY ->
            if reader.RemainingBytes = 0 then
                ILNativeType.Array(None, None)
            else
                let nt = 
                    let oldReader = reader
                    let u = reader.ReadCompressedInteger() // What is 'u'?
                    if u = int nt_MAX then // What is this doing?
                        ILNativeType.Empty
                    else
                        // NOTE: go back to start and read native type
                        reader <- oldReader
                        readILNativeType cenv &reader
              
                if reader.RemainingBytes = 0 then
                    ILNativeType.Array(Some(nt), None)
                else
                    let pnum = reader.ReadCompressedInteger()
                    if reader.RemainingBytes = 0 then
                        ILNativeType.Array(Some(nt), Some(pnum, None))
                    else
                        let additive = reader.ReadCompressedInteger()
                        ILNativeType.Array(Some(nt), Some(pnum, Some(additive)))

        | _ when kind = nt_CUSTOMMARSHALER ->
            let guid = reader.ReadBytes(reader.ReadCompressedInteger())
            let nativeTypeName = reader.ReadUTF16(reader.ReadCompressedInteger())
            let custMarshallerName = reader.ReadUTF16(reader.ReadCompressedInteger())
            let cookieString = reader.ReadBytes(reader.ReadCompressedInteger())
            ILNativeType.Custom(guid, nativeTypeName, custMarshallerName, cookieString)

        | _ -> ILNativeType.Empty

let tryReadILNativeType (cenv: cenv) (marshalDesc: BlobHandle) =
    if marshalDesc.IsNil then None
    else
        let mdReader = cenv.MetadataReader

        let mutable (* it doesn't have to be mutable, but it's best practice for .NET structs *) reader = mdReader.GetBlobReader(marshalDesc)
        Some(readILNativeType cenv &reader)

let readILParameter (cenv: cenv) (paramTypes: ImmutableArray<ILType>) (returnType: ILType) (paramHandle: ParameterHandle) : ILParameter =
    let mdReader = cenv.MetadataReader

    let param = mdReader.GetParameter(paramHandle)

    let nameOpt = mdReader.TryGetString(param.Name)
    let typ = 
        if param.SequenceNumber = 0 then returnType
        else paramTypes.[param.SequenceNumber - 1]

    let defaul =
        if int (param.Attributes &&& ParameterAttributes.HasDefault) <> 0 then
            tryReadILFieldInit cenv (param.GetDefaultValue())
        else
            None

    let marshal =
        if int (param.Attributes &&& ParameterAttributes.HasFieldMarshal) <> 0 then 
            tryReadILNativeType cenv (param.GetMarshallingDescriptor())
        else
            None

    {
        Name = match nameOpt with | ValueNone -> None | ValueSome(name) -> Some(name)
        Type = typ
        Default = defaul
        Marshal = marshal
        IsIn = int (param.Attributes &&& ParameterAttributes.In) <> 0
        IsOut = int (param.Attributes &&& ParameterAttributes.Out) <> 0
        IsOptional = int (param.Attributes &&& ParameterAttributes.Optional) <> 0
        CustomAttrsStored = readILAttributesStored cenv (param.GetCustomAttributes())
        MetadataIndex = NoMetadataIdx
    }

let readILParameters (cenv: cenv) paramTypes returnType (paramHandles: ParameterHandleCollection) =
    paramHandles
    |> Seq.map (readILParameter cenv paramTypes returnType)
    |> List.ofSeq

let readILCode (cenv: cenv) : ILCode =
    {
        Labels = System.Collections.Generic.Dictionary() // TODO
        Instrs = [||] // TODO
        Exceptions = [] // TODO
        Locals = [] // TODO
    }

let readILMethodBody (cenv: cenv) (methDef: MethodDefinition) : ILMethodBody =
    let peReader = cenv.PEReader
    let mdReader = cenv.MetadataReader

    let methBodyBlock = peReader.GetMethodBody(methDef.RelativeVirtualAddress)

    let ilLocals =
        if methBodyBlock.LocalSignature.IsNil then []
        else
            let si = mdReader.GetStandaloneSignature(methBodyBlock.LocalSignature)
            si.DecodeLocalSignature(cenv.LocalSignatureTypeProvider, ())
            |> List.ofSeq
    
    {
        IsZeroInit = methBodyBlock.LocalVariablesInitialized
        MaxStack = methBodyBlock.MaxStack
        NoInlining = int (methDef.ImplAttributes &&& MethodImplAttributes.NoInlining) <> 0
        AggressiveInlining = int (methDef.ImplAttributes &&& MethodImplAttributes.AggressiveInlining) <> 0
        Locals = ilLocals
        Code = readILCode cenv // TODO:
        SourceMarker = None // TODO: Do we need to read a source marker?
    }

let readMethodBody (cenv: cenv) (methDef: MethodDefinition) =
    let mdReader = cenv.MetadataReader
    let attrs = methDef.Attributes
    let implAttrs = methDef.ImplAttributes

    let isPInvoke = int (attrs &&& MethodAttributes.PinvokeImpl) <> 0
    let codeType = int (implAttrs &&& MethodImplAttributes.CodeTypeMask)

    if codeType = 0x01 && isPInvoke then
        MethodBody.Native
    elif isPInvoke then
        let import = methDef.GetImport()
        let importAttrs = import.Attributes
        let pInvokeMethod : PInvokeMethod =
            {
                Where = readILModuleRefFromModuleReference cenv (mdReader.GetModuleReference(import.Module))
                Name = mdReader.GetString(import.Name)
                CallingConv = mkPInvokeCallingConvention importAttrs
                CharEncoding = mkPInvokeCharEncoding importAttrs
                NoMangle = int (importAttrs &&& MethodImportAttributes.ExactSpelling) <> 0
                LastError = int (importAttrs &&& MethodImportAttributes.SetLastError) <> 0
                ThrowOnUnmappableChar = mkPInvokeThrowOnUnmappableChar importAttrs
                CharBestFit = mkPInvokeCharBestFit importAttrs
            }
        MethodBody.PInvoke(pInvokeMethod)
    elif codeType <> 0x00 || int (attrs &&& MethodAttributes.Abstract) <> 0 || int (implAttrs &&& MethodImplAttributes.InternalCall) <> 0 || int (implAttrs &&& MethodImplAttributes.Unmanaged) <> 0 then
        MethodBody.Abstract
    elif not cenv.IsMetadataOnly then
        MethodBody.IL(readILMethodBody cenv methDef)
    else
        MethodBody.NotAvailable

let readILMethodDef (cenv: cenv) (methDefHandle: MethodDefinitionHandle) =
    let mdReader = cenv.MetadataReader

    let methDef = mdReader.GetMethodDefinition(methDefHandle)
    let si = methDef.DecodeSignature(cenv.SignatureTypeProvider, ())

    ILMethodDef(
        name = mdReader.GetString(methDef.Name),
        attributes = methDef.Attributes,
        implAttributes = methDef.ImplAttributes,
        callingConv = mkILCallingConv si.Header,
        parameters = readILParameters cenv si.ParameterTypes si.ReturnType (methDef.GetParameters()), // TODO: First param might actually be the return type.
        ret = mkILReturn si.ReturnType, // TODO: Do we need more info for ILReturn?
        body = mkMethBodyLazyAux (lazy readMethodBody cenv methDef),
        isEntryPoint = false, // TODO: need to pass entrypoint token
        genericParams = readILGenericParameterDefs cenv (methDef.GetGenericParameters()),
        securityDeclsStored = readILSecurityDeclsStored cenv (methDef.GetDeclarativeSecurityAttributes()),
        customAttrsStored = readILAttributesStored cenv (methDef.GetCustomAttributes()),
        metadataIndex = NoMetadataIdx
    )

let readILFieldDef (cenv: cenv) (ilTypeDefLayout: ILTypeDefLayout) (fieldDefHandle: FieldDefinitionHandle) =
    let mdReader = cenv.MetadataReader

    let fieldDef = mdReader.GetFieldDefinition(fieldDefHandle)

    let data = 
        if not cenv.IsMetadataOnly && int (fieldDef.Attributes &&& FieldAttributes.HasFieldRVA) <> 0 then
            cenv.PEReader.GetSectionData(fieldDef.GetRelativeVirtualAddress()).GetContent().ToArray() // We should just return the immutable array instead of making a copy....
            |> Some
        else
            None

    let literalValue =
        if int (fieldDef.Attributes &&& FieldAttributes.HasDefault) <> 0 then
            tryReadILFieldInit cenv (fieldDef.GetDefaultValue())
        else
            None

    let offset =
        let isStatic = int (fieldDef.Attributes &&& FieldAttributes.Static) <> 0
        let hasLayout = (match ilTypeDefLayout with ILTypeDefLayout.Explicit _ -> true | _ -> false)
        if hasLayout && not isStatic then
            Some(fieldDef.GetOffset())
        else
            None
            
    let marshal =
        if int (fieldDef.Attributes &&& FieldAttributes.HasFieldMarshal) <> 0 then 
            tryReadILNativeType cenv (fieldDef.GetMarshallingDescriptor())
        else
            None

    ILFieldDef(
        name = mdReader.GetString(fieldDef.Name),
        fieldType = fieldDef.DecodeSignature(cenv.SignatureTypeProvider, ()),
        attributes = fieldDef.Attributes,
        data = data,
        literalValue = literalValue,
        offset = offset,
        marshal = marshal,
        customAttrsStored = readILAttributesStored cenv (fieldDef.GetCustomAttributes()),
        metadataIndex = NoMetadataIdx
    )

let readILFieldDefs (cenv: cenv) ilTypeDefLayout (fieldDefHandles: FieldDefinitionHandleCollection) =
    let f =
        lazy
            fieldDefHandles
            |> Seq.map (readILFieldDef cenv ilTypeDefLayout)
            |> List.ofSeq
    mkILFieldsLazy f

let readILPropertyDef (cenv: cenv) (propDefHandle: PropertyDefinitionHandle) =
    let mdReader = cenv.MetadataReader

    let propDef = mdReader.GetPropertyDefinition(propDefHandle)
    let si = propDef.DecodeSignature(cenv.SignatureTypeProvider, ())
    let accessors = propDef.GetAccessors()

    let setMethod =
        if accessors.Getter.IsNil then None
        else
            let spec = readILMethodSpec cenv (MethodDefinitionHandle.op_Implicit(accessors.Getter))
            Some(spec.MethodRef)

    let getMethod =
        if accessors.Setter.IsNil then None
        else
            let spec = readILMethodSpec cenv (MethodDefinitionHandle.op_Implicit(accessors.Setter))
            Some(spec.MethodRef)

    let init =
        if int (propDef.Attributes &&& PropertyAttributes.HasDefault) <> 0 then
            tryReadILFieldInit cenv (propDef.GetDefaultValue())
        else
            None

    let args = si.ParameterTypes |> List.ofSeq

    ILPropertyDef(
        name = mdReader.GetString(propDef.Name),
        attributes = propDef.Attributes,
        setMethod = setMethod,
        getMethod = getMethod,
        callingConv = mkILThisConvention si.Header,
        propertyType = si.ReturnType,
        init = init,
        args = args,
        customAttrsStored = readILAttributesStored cenv (propDef.GetCustomAttributes()),
        metadataIndex = NoMetadataIdx
    )

let readILPropertyDefs (cenv: cenv) (propDefHandles: PropertyDefinitionHandleCollection) =
    let f =
        lazy
            propDefHandles
            |> Seq.map (readILPropertyDef cenv)
            |> List.ofSeq
    mkILPropertiesLazy f

let rec readILTypeDef (cenv: cenv) (typeDefHandle: TypeDefinitionHandle) =
    let mdReader = cenv.MetadataReader

    let typeDef = mdReader.GetTypeDefinition(typeDefHandle)

    let name = 
        let namespaceOpt = mdReader.TryGetString(typeDef.Namespace)
        let name = mdReader.GetString(typeDef.Name)
        match namespaceOpt with
        | ValueNone -> name
        | ValueSome(namespac) -> namespac + "." + name

    let implements =
        typeDef.GetInterfaceImplementations()
        |> Seq.map (fun h ->
            let interfaceImpl = mdReader.GetInterfaceImplementation(h)
            readILType cenv interfaceImpl.Interface
        )
        |> List.ofSeq

    let genericParams = readILGenericParameterDefs cenv (typeDef.GetGenericParameters())

    let extends =
        if typeDef.BaseType.IsNil then None
        else Some(readILType cenv typeDef.BaseType)

    let methods =
        mkILMethodsComputed (fun () ->
            let methDefHandles = typeDef.GetMethods()
            let ilMethodDefs = Array.zeroCreate methDefHandles.Count

            let mutable i = 0
            for methDefHandle in methDefHandles do
                ilMethodDefs.[i] <- readILMethodDef cenv methDefHandle
                i <- i + 1

            ilMethodDefs
        )

    let nestedTypes =
        mkILTypeDefsComputed (fun () ->
            typeDef.GetNestedTypes()
            |> Seq.map (fun h -> 
                readILPreTypeDef cenv h
            )
            |> Array.ofSeq
        )

    let ilTypeDefLayout = mkILTypeDefLayout typeDef.Attributes (typeDef.GetLayout())

    ILTypeDef(
        name = name,
        attributes = typeDef.Attributes,
        layout = ilTypeDefLayout,
        implements = implements,
        genericParams = genericParams,
        extends = extends,
        methods = methods,
        nestedTypes = nestedTypes,
        fields = readILFieldDefs cenv ilTypeDefLayout (typeDef.GetFields()),
        methodImpls = mkILMethodImpls [], // TODO
        events = mkILEvents [], // TODO
        properties = readILPropertyDefs cenv (typeDef.GetProperties()),
        securityDeclsStored = readILSecurityDeclsStored cenv (typeDef.GetDeclarativeSecurityAttributes()),
        customAttrsStored = readILAttributesStored cenv (typeDef.GetCustomAttributes()),
        metadataIndex = MetadataTokens.GetRowNumber(TypeDefinitionHandle.op_Implicit(typeDefHandle))
    )

let readILPreTypeDef (cenv: cenv) (typeDefHandle: TypeDefinitionHandle) =
    let mdReader = cenv.MetadataReader

    let typeDef = mdReader.GetTypeDefinition(typeDefHandle)

    let namespaceOpt = mdReader.TryGetString(typeDef.Namespace)
    let name = mdReader.GetString(typeDef.Name)

    let namespaceSplit =
        match namespaceOpt with
        | ValueNone -> []
        | ValueSome(namespac) -> splitNamespace namespac

    mkILPreTypeDefComputed (namespaceSplit, name, (fun () -> readILTypeDef cenv typeDefHandle))

let readILPreTypeDefs (cenv: cenv) = 
    let mdReader = cenv.MetadataReader

    [|
        for typeDefHandle in mdReader.TypeDefinitions do
            let typeDef = mdReader.GetTypeDefinition(typeDefHandle)
            // Only get top types.
            if not typeDef.IsNested then
                yield readILPreTypeDef cenv typeDefHandle
    |]

let readModuleDef ilGlobals (peReader: PEReader) metadataOnlyFlag =
    let nativeResources = readILNativeResources peReader

    let subsys =
        int16 peReader.PEHeaders.PEHeader.Subsystem

    let subsysversion =
        (int32 peReader.PEHeaders.PEHeader.MajorSubsystemVersion, int32 peReader.PEHeaders.PEHeader.MinorSubsystemVersion)

    let useHighEntropyVA =
        int (peReader.PEHeaders.PEHeader.DllCharacteristics &&& DllCharacteristics.HighEntropyVirtualAddressSpace) <> 0

    let ilOnly =
        int (peReader.PEHeaders.CorHeader.Flags &&& CorFlags.ILOnly) <> 0

    let only32 =
        int (peReader.PEHeaders.CorHeader.Flags &&& CorFlags.Requires32Bit) <> 0

    let is32bitpreferred =
        int (peReader.PEHeaders.CorHeader.Flags &&& CorFlags.Prefers32Bit) <> 0

    let only64 =
        peReader.PEHeaders.CoffHeader.SizeOfOptionalHeader = 240s (* May want to read in the optional header Magic number and check that as well... *)

    let platform = 
        match peReader.PEHeaders.CoffHeader.Machine with
        | Machine.Amd64 -> Some(AMD64)
        | Machine.IA64 -> Some(IA64)
        | _ -> Some(X86)

    let isDll = peReader.PEHeaders.IsDll

    let alignVirt =
        peReader.PEHeaders.PEHeader.SectionAlignment

    let alignPhys =
        peReader.PEHeaders.PEHeader.FileAlignment

    let imageBaseReal = int peReader.PEHeaders.PEHeader.ImageBase

    let entryPointToken = peReader.PEHeaders.CorHeader.EntryPointTokenOrRelativeVirtualAddress

    let mdReader = peReader.GetMetadataReader()
    let moduleDef = mdReader.GetModuleDefinition()
    let ilModuleName = mdReader.GetString(moduleDef.Name)
    let ilMetadataVersion = mdReader.MetadataVersion
    
    let cenv = 
        let sigTyProvider = SignatureTypeProvider(ilGlobals)
        let localSigTyProvider = LocalSignatureTypeProvider(ilGlobals)
        let cenv = cenv(peReader, mdReader, MetadataOnlyFlag.No, sigTyProvider, localSigTyProvider)
        sigTyProvider.cenv <- cenv
        localSigTyProvider.cenv <- cenv
        cenv

    { Manifest = Some(readILAssemblyManifest cenv entryPointToken)
      CustomAttrsStored = readILAttributesStored cenv (moduleDef.GetCustomAttributes())
      MetadataIndex = 1 // TODO: Is this right?
      Name = ilModuleName
      NativeResources = nativeResources
      TypeDefs = mkILTypeDefsComputed (fun () -> readILPreTypeDefs cenv)
      SubSystemFlags = int32 subsys
      IsILOnly = ilOnly
      SubsystemVersion = subsysversion
      UseHighEntropyVA = useHighEntropyVA
      Platform = platform
      StackReserveSize = None  // TODO
      Is32Bit = only32
      Is32BitPreferred = is32bitpreferred
      Is64Bit = only64
      IsDLL=isDll
      VirtualAlignment = alignVirt
      PhysicalAlignment = alignPhys
      ImageBase = imageBaseReal
      MetadataVersion = ilMetadataVersion
      Resources = mkILResources [] // TODO //seekReadManifestResources ctxt mdv pectxtEager pevEager
    }  

let openILModuleReader fileName (ilReaderOptions: Microsoft.FSharp.Compiler.AbstractIL.ILBinaryReader.ILReaderOptions) =
    let peReader = new PEReader(File.ReadAllBytes(fileName).ToImmutableArray())
    { new Microsoft.FSharp.Compiler.AbstractIL.ILBinaryReader.ILModuleReader with

        member __.ILModuleDef = 
            readModuleDef ilReaderOptions.ilGlobals peReader ilReaderOptions.metadataOnly

        member __.ILAssemblyRefs = []

      interface IDisposable with

        member __.Dispose() = ()
    }
