module rec Microsoft.FSharp.Compiler.AbstractIL.ILReader

open System
open System.IO
open System.Linq
open System.Reflection
open System.Reflection.PortableExecutable
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open Microsoft.FSharp.Compiler.AbstractIL.IL  
open Microsoft.FSharp.Compiler.AbstractIL.Internal.Library

type MetadataReader with

    member this.TryGetString(handle: StringHandle) =
        if handle.IsNil then ValueNone
        else
            let str = this.GetString(handle)
            if str = null then ValueNone
            else ValueSome(str)

[<Sealed>]
type cenv(mdReader: MetadataReader, sigTyProvider: ISignatureTypeProvider<ILType, unit>) =

    member __.MetadataReader = mdReader

    member __.SignatureTypeProvider = sigTyProvider

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
    if int (attributes &&& TypeAttributes.SequentialLayout) <> 0 then
        ILTypeDefLayout.Sequential(mkILTypeDefLayoutInfo layout)
    elif int (attributes &&& TypeAttributes.ExplicitLayout) <> 0 then
        ILTypeDefLayout.Explicit(mkILTypeDefLayoutInfo layout)
    else
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

[<Sealed>]
type SignatureTypeProvider(ilg: ILGlobals) =

    member val cenv : cenv = Unchecked.defaultof<_> with get, set

    interface ISignatureTypeProvider<ILType, unit> with

        member __.GetFunctionPointerType(si) =
            let callingSig =
                {
                    CallingConv = readILCallingConv si.Header
                    ArgTypes = si.ParameterTypes |> Seq.toList
                    ReturnType = si.ReturnType
                }
            ILType.FunctionPointer(callingSig)

        member __.GetGenericMethodParameter(_, index) =
            ILType.TypeVar(uint16 index)

        member __.GetGenericTypeParameter(_, index) =
            ILType.TypeVar(uint16 index)

        member __.GetModifiedType(modifier, unmodifiedType, isRequired) =
            ILType.Modified(isRequired, modifier.TypeRef, unmodifiedType)

        member __.GetPinnedType(elementType) = elementType // TODO: Is this right?

        member this.GetTypeFromSpecification(mdReader, ctxt, typeSpecHandle, _) =
            let typeSpec = mdReader.GetTypeSpecification(typeSpecHandle)
            typeSpec.DecodeSignature(this, ctxt)
            
    interface ISimpleTypeProvider<ILType> with

        member __.GetPrimitiveType(typeCode) =
            match typeCode with
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
            | _ -> failwithf "Invalid Primitive Type Code: %A" typeCode

        member this.GetTypeFromDefinition(_, typeDefHandle, _) =
            let typeDef = this.cenv.MetadataReader.GetTypeDefinition(typeDefHandle)
            readILTypeFromTypeDefinition this.cenv typeDef

        member this.GetTypeFromReference(_, typeRefHandle, _) =
            let typeRef = this.cenv.MetadataReader.GetTypeReference(typeRefHandle)
            readILTypeFromTypeReference this.cenv typeRef

    interface IConstructedTypeProvider<ILType> with

        member __.GetGenericInstantiation(genericType, typeArgs) =
            let ilTypeSpec = ILTypeSpec.Create(genericType.TypeRef, typeArgs |> List.ofSeq)
            mkILTy genericType.Boxity ilTypeSpec

        member __.GetArrayType(elementType, shape) =
            let lowerBounds = shape.LowerBounds
            let sizes = shape.Sizes
            let rank = shape.Rank
            let shape = 
                let dim i =
                  (if i < lowerBounds.Length then Some (Seq.item i lowerBounds) else None), 
                  (if i < sizes.Length then Some (Seq.item i sizes) else None)
                ILArrayShape (List.init rank dim)
            mkILArrTy (elementType, shape)

        member __.GetByReferenceType(elementType) =
            ILType.Byref(elementType)

        member __.GetPointerType(elementType) =
            ILType.Ptr(elementType)

    interface ISZArrayTypeProvider<ILType> with

        member __.GetSZArrayType(elementType) =
            mkILArr1DTy elementType

let rec readILScopeRef (cenv: cenv) (handle: EntityHandle) =
    let mdReader = cenv.MetadataReader

    match handle.Kind with
    | HandleKind.AssemblyFile ->
        let asmFile = mdReader.GetAssemblyFile(AssemblyFileHandle.op_Explicit(handle))
        ILScopeRef.Module(readILModuleRefFromAssemblyFile cenv asmFile)

    | HandleKind.AssemblyReference ->
        let asmRef = mdReader.GetAssemblyReference(AssemblyReferenceHandle.op_Explicit(handle))
        ILScopeRef.Assembly(readILAssemblyRefFromAssemblyReference cenv asmRef)

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

let readILAssemblyRefFromAssemblyReference (cenv: cenv) (asmRef: AssemblyReference) =
    let mdReader = cenv.MetadataReader

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

let readILModuleRefFromAssemblyFile (cenv: cenv) (asmFile: AssemblyFile) =
    let mdReader = cenv.MetadataReader

    let name = mdReader.GetString(asmFile.Name)

    let hash = 
        let hashValue = asmFile.HashValue
        if hashValue.IsNil then None
        else Some(mdReader.GetBlobBytes(hashValue))

    ILModuleRef.Create(name, asmFile.ContainsMetadata, hash) 

let readILType (cenv: cenv) (handle: EntityHandle) : ILType =
    let mdReader = cenv.MetadataReader

    match handle.Kind with
    | HandleKind.TypeReference ->
        let typeRef = mdReader.GetTypeReference(TypeReferenceHandle.op_Explicit(handle))
        readILTypeFromTypeReference cenv typeRef

    | HandleKind.TypeDefinition ->
        let typeDef = mdReader.GetTypeDefinition(TypeDefinitionHandle.op_Explicit(handle))
        readILTypeFromTypeDefinition cenv typeDef

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

let readILTypeFromTypeReference (cenv: cenv) (typeRef: TypeReference) =
    let ilTypeRef = readILTypeRefFromTypeReference cenv typeRef
    let ilTypeSpec = ILTypeSpec.Create(ilTypeRef, ILGenericArgs.Empty)
    mkILTy AsObject (* AsObject probably not nok *) ilTypeSpec

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

let readILTypeFromTypeDefinition (cenv: cenv) (typeDef: TypeDefinition) =
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

    mkILTy boxity ilTypeSpec

let readILCallingConv (sigHeader: SignatureHeader) =
    let ilThisConvention =
        if sigHeader.IsInstance then
            if sigHeader.HasExplicitThis then ILThisConvention.InstanceExplicit
            else ILThisConvention.Instance
        else ILThisConvention.Static

    let ilArgConvention =
        match sigHeader.CallingConvention with
        | SignatureCallingConvention.Default -> ILArgConvention.Default
        | SignatureCallingConvention.CDecl -> ILArgConvention.CDecl
        | SignatureCallingConvention.StdCall -> ILArgConvention.StdCall
        | SignatureCallingConvention.ThisCall -> ILArgConvention.ThisCall
        | SignatureCallingConvention.FastCall -> ILArgConvention.FastCall
        | SignatureCallingConvention.VarArgs -> ILArgConvention.VarArg
        | _ -> failwithf "Invalid Signature Calling Convention: %A" sigHeader.CallingConvention

    ILCallingConv.Callconv(ilThisConvention, ilArgConvention)

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

let rec readILMethodSpec (cenv: cenv) (handle: EntityHandle) : ILMethodSpec =
    let mdReader = cenv.MetadataReader
    match handle.Kind with
    | HandleKind.MemberReference ->
        let memberRef = mdReader.GetMemberReference(MemberReferenceHandle.op_Explicit(handle))
        let si = memberRef.DecodeMethodSignature(cenv.SignatureTypeProvider, ())
        
        let name = mdReader.GetString(memberRef.Name)
        let enclILTy = readILType cenv memberRef.Parent
        let ilCallingConv = readILCallingConv si.Header
        let genericArity = 0

        let ilMethodRef = ILMethodRef.Create(enclILTy.TypeRef, ilCallingConv, name, genericArity, si.ParameterTypes |> List.ofSeq, si.ReturnType)

        ILMethodSpec.Create(enclILTy, ilMethodRef, [])

    | HandleKind.MethodDefinition ->
        let methodDefHandle = MethodDefinitionHandle.op_Explicit(handle)
        let methodDef = mdReader.GetMethodDefinition(methodDefHandle)
        let si = methodDef.DecodeSignature(cenv.SignatureTypeProvider, ())

        let name = mdReader.GetString(methodDef.Name)
        let enclTypeDef = mdReader.GetTypeDefinition(methodDef.GetDeclaringType())
        let enclILTy = readILTypeFromTypeDefinition cenv enclTypeDef
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
    [
        for declSecurityAttributeHandle in declSecurityAttributeHandles do
            yield readILSecurityDecl cenv declSecurityAttributeHandle
    ]
    |> mkILSecurityDecls
    |> storeILSecurityDecls

let readILAttribute (cenv: cenv) (customAttrHandle: CustomAttributeHandle) =
    let customAttr = cenv.MetadataReader.GetCustomAttribute(customAttrHandle)

    ILAttribute.Encoded(readILMethodSpec cenv customAttr.Constructor, [||], [])

let readILAttributesStored (cenv: cenv) (customAttrs: CustomAttributeHandleCollection) =
    [
        for customAttrHandle in customAttrs do
            yield readILAttribute cenv customAttrHandle
    ]
    |> mkILCustomAttrs
    |> storeILCustomAttrs

let rec readILNestedExportedTypes (cenv: cenv) (handle: EntityHandle) =
    let mdReader = cenv.MetadataReader
    match handle.Kind with
    | HandleKind.TypeDefinition ->
        let typeDefHandle = TypeDefinitionHandle.op_Explicit(handle)
        let typeDef = mdReader.GetTypeDefinition(typeDefHandle)

        let nested =
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

        mkILNestedExportedTypes nested

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
    [
        for exportedTyHandle in exportedTys do
            yield readILExportedType cenv exportedTyHandle
    ]
    |> mkILExportedTypes

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

type PEReaderKind =
    | OnDisk of fileName: string
    | InMemory

let readILNativeResources (peReader: PEReader) peReaderKind =
    peReader.PEHeaders.SectionHeaders
    |> Seq.choose (fun s ->
        // TODO: Is this right?
        if s.Name.Equals(".rsrc", StringComparison.OrdinalIgnoreCase) then
            match peReaderKind with
            | OnDisk(fileName) ->
                ILNativeResource.In(fileName, s.VirtualAddress, s.PointerToRawData, s.VirtualSize)
                |> Some
            | InMemory ->
                let memBlock = peReader.GetSectionData(s.VirtualAddress)
                let bytes = memBlock.GetContent().ToArray() // We should never do this. ILNativeResource.Out should just take an immutable array...
                ILNativeResource.Out(bytes)
                |> Some
        else
            None
    )
    |> Seq.toList

let tryReadILFieldInit (cenv: cenv) (constantHandle: ConstantHandle) =
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
    | _ -> None

let readILParameter (cenv: cenv) (si: MethodSignature<ILType>) (paramHandle: ParameterHandle) : ILParameter =
    let mdReader = cenv.MetadataReader

    let param = mdReader.GetParameter(paramHandle)

    let nameOpt = mdReader.TryGetString(param.Name)
    let typ = si.ParameterTypes.[param.SequenceNumber]

    {
        Name = match nameOpt with | ValueNone -> None | ValueSome(name) -> Some(name)
        Type = typ
        Default = tryReadILFieldInit cenv (param.GetDefaultValue())
        Marshal = None // TODO
        IsIn = false // TODO
        IsOut = false // TODO
        IsOptional = false // TODO
        CustomAttrsStored = readILAttributesStored cenv (param.GetCustomAttributes())
        MetadataIndex = NoMetadataIdx
    }

let readILMethodDef (cenv: cenv) (methDefHandle: MethodDefinitionHandle) =
    let mdReader = cenv.MetadataReader

    let methDef = mdReader.GetMethodDefinition(methDefHandle)

    let name = mdReader.GetString(methDef.Name) 
    let attributes = methDef.Attributes
    let implAttributes = methDef.ImplAttributes

    let si = methDef.DecodeSignature(cenv.SignatureTypeProvider, ())

    let callingConv = readILCallingConv si.Header
    let parameters = 
        methDef.GetParameters()
        |> Seq.map (fun h ->
            readILParameter cenv si h
        )
        |> List.ofSeq

    ILMethodDef(
        name = name,
        attributes = attributes,
        implAttributes = implAttributes,
        callingConv = callingConv,
        parameters = parameters,
        ret = mkILReturn si.ReturnType,
        body = null, // TODO
        isEntryPoint = false, // TODO
        genericParams = null, // TODO
        securityDeclsStored = readILSecurityDeclsStored cenv (methDef.GetDeclarativeSecurityAttributes()),
        customAttrsStored = readILAttributesStored cenv (methDef.GetCustomAttributes()),
        metadataIndex = NoMetadataIdx
    )

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

    let genericParams =
        typeDef.GetGenericParameters()
        |> Seq.map (fun h ->
            readILGenericParameterDef cenv h
        )
        |> List.ofSeq

    let extends =
        if typeDef.BaseType.IsNil then None
        else Some(readILType cenv typeDef.BaseType)

    let methods =
        mkILMethodsComputed (fun () ->
            typeDef.GetMethods()
            |> Seq.map (fun h ->
                readILMethodDef cenv h
            )
            |> Array.ofSeq
        )

    let nestedTypes =
        typeDef.GetNestedTypes()
        |> Seq.map (fun h -> 
            readILTypeDef cenv h
        )
        |> List.ofSeq
        |> mkILTypeDefs

    ILTypeDef(
        name = name,
        attributes = typeDef.Attributes,
        layout = mkILTypeDefLayout typeDef.Attributes (typeDef.GetLayout()),
        implements = implements,
        genericParams = genericParams,
        extends = extends,
        methods = methods,
        nestedTypes = nestedTypes,
        fields = [], // TODO
        methodImpls = [], // TODO
        events = [], // TODO
        properties = [], // TODO
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

let readModuleDef ilGlobals (peReader: PEReader) peReaderKind =
    let nativeResources = readILNativeResources peReader peReaderKind

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
        let cenv = cenv(mdReader, sigTyProvider)
        sigTyProvider.cenv <- cenv
        cenv

    { Manifest = Some(readILAssemblyManifest cenv entryPointToken)
      CustomAttrsStored = readILAttributesStored cenv (moduleDef.GetCustomAttributes())
      MetadataIndex = 1 // TODO: Is this right?
      Name = ilModuleName
      NativeResources = nativeResources
      TypeDefs = emptyILTypeDefs // TODO //mkILTypeDefsComputed (fun () -> seekReadTopTypeDefs ctxt)
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
    let peReader = new PEReader(new MemoryStream(File.ReadAllBytes(fileName)))
    { new Microsoft.FSharp.Compiler.AbstractIL.ILBinaryReader.ILModuleReader with

        member __.ILModuleDef = 
            readModuleDef ilReaderOptions.ilGlobals peReader (OnDisk fileName)

        member __.ILAssemblyRefs = []

      interface IDisposable with

        member __.Dispose() = ()
    }
