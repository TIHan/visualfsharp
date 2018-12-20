﻿module rec Microsoft.FSharp.Compiler.AbstractIL.ILReader

open System
open System.IO
open System.Reflection
open System.Reflection.PortableExecutable
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open Microsoft.FSharp.Compiler.AbstractIL.IL  
open Microsoft.FSharp.Compiler.AbstractIL.Internal.Library

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

let rec readILScopeRef (mdReader: MetadataReader) (handle: EntityHandle) =
    match handle.Kind with
    | HandleKind.AssemblyReference ->
        let asmRef = mdReader.GetAssemblyReference(AssemblyReferenceHandle.op_Explicit(handle))
        ILScopeRef.Assembly(readILAssemblyRef mdReader asmRef)

    | HandleKind.ModuleReference ->
        let modRef = mdReader.GetModuleReference(ModuleReferenceHandle.op_Explicit(handle))
        ILScopeRef.Module(readILModuleRef mdReader modRef)

    | HandleKind.TypeReference ->
        let typeRef = mdReader.GetTypeReference(TypeReferenceHandle.op_Explicit(handle))
        readILScopeRef mdReader typeRef.ResolutionScope

    | HandleKind.ExportedType ->
        let exportedTy = mdReader.GetExportedType(ExportedTypeHandle.op_Explicit(handle))
        readILScopeRef mdReader exportedTy.Implementation

    | _ ->
        failwithf "Invalid Handle Kind: %A" handle.Kind

let rec readILAssemblyRef (mdReader: MetadataReader) (asmRef: AssemblyReference) =
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

let readILType (mdReader: MetadataReader) (handle: EntityHandle) : ILType =
    match handle.Kind with
    | HandleKind.TypeReference ->
        let typeRef = mdReader.GetTypeReference(TypeReferenceHandle.op_Explicit(handle))
        readILTypeFromTypeReference mdReader typeRef

    | HandleKind.TypeDefinition ->
        let typeDef = mdReader.GetTypeDefinition(TypeDefinitionHandle.op_Explicit(handle))
        readILTypeFromTypeDefinition mdReader typeDef

    | _ ->
        failwithf "Invalid Handle Kind: %A" handle.Kind

let readILModuleRef (mdReader: MetadataReader) (modRef: ModuleReference) =
    let name = mdReader.GetString(modRef.Name)
    ILModuleRef.Create(name, hasMetadata = true, hash = None)

let readILTypeRefFromTypeReference (mdReader: MetadataReader) (typeRef: TypeReference) =
    let ilScopeRef = readILScopeRef mdReader typeRef.ResolutionScope

    let name = mdReader.GetString(typeRef.Name)
    let namespac = mdReader.GetString(typeRef.Namespace)

    ILTypeRef.Create(ilScopeRef, [], namespac + "." + name)

let readILTypeFromTypeReference (mdReader: MetadataReader) (typeRef: TypeReference) =
    let ilTypeRef = readILTypeRefFromTypeReference mdReader typeRef
    let ilTypeSpec = ILTypeSpec.Create(ilTypeRef, ILGenericArgs.Empty)
    mkILTy AsObject (* AsObject probably not nok *) ilTypeSpec

let rec readILTypeRefFromTypeDefinition (mdReader: MetadataReader) (typeDef: TypeDefinition) : ILTypeRef =
    let enclosing =
        if typeDef.IsNested then
            let parentTypeDefHandle = typeDef.GetDeclaringType()
            let parentTypeDef = mdReader.GetTypeDefinition(parentTypeDefHandle)
            let ilTypeRef = readILTypeRefFromTypeDefinition mdReader parentTypeDef
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

let readILTypeFromTypeDefinition (mdReader: MetadataReader) (typeDef: TypeDefinition) =
    let ilTypeRef = readILTypeRefFromTypeDefinition mdReader typeDef

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

[<Sealed>]
type SignatureTypeProvider(ilg: ILGlobals) =
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

        member __.GetPinnedType(elementType) = elementType

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
            | PrimitiveTypeCode.TypedReference -> ilg.typ_Type
            | PrimitiveTypeCode.UInt16 -> ilg.typ_UInt16
            | PrimitiveTypeCode.UInt32 -> ilg.typ_UInt32
            | PrimitiveTypeCode.UInt64 -> ilg.typ_UInt64
            | PrimitiveTypeCode.UIntPtr -> ilg.typ_UIntPtr
            | PrimitiveTypeCode.Void -> ILType.Void
            | _ -> failwithf "Invalid Primitive Type Code: %A" typeCode

        member __.GetTypeFromDefinition(mdReader, typeDefHandle, _) =
            let typeDef = mdReader.GetTypeDefinition(typeDefHandle)
            readILTypeFromTypeDefinition mdReader typeDef

        member __.GetTypeFromReference(mdReader, typeRefHandle, _) =
            let typeRef = mdReader.GetTypeReference(typeRefHandle)
            readILTypeFromTypeReference mdReader typeRef

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

[<Sealed>]
type cenv(mdReader: MetadataReader, sigTyProvider: ISignatureTypeProvider<ILType, unit>) =

    member __.MetadataReader = mdReader

    member __.SignatureTypeProvider = sigTyProvider

let readILGenericParameterDef (cenv: cenv) (genParamHandle: GenericParameterHandle) : ILGenericParameterDef =
    let mdReader = cenv.MetadataReader
    let genParam = mdReader.GetGenericParameter(genParamHandle)
    let attributes = genParam.Attributes

    let constraints =
        genParam.GetConstraints()
        |> Seq.map (fun genParamCnstrHandle ->
            let genParamCnstr = mdReader.GetGenericParameterConstraint(genParamCnstrHandle)
            readILType mdReader genParamCnstr.Type
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
        let enclILTy = readILType mdReader memberRef.Parent
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
        let enclILTy = readILTypeFromTypeDefinition mdReader enclTypeDef
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

let readILSecurityAction (declSecurityAction: DeclarativeSecurityAction) =
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

let readILSecurityDecl (mdReader: MetadataReader) (declSecurityAttributeHandle: DeclarativeSecurityAttributeHandle) =
    let declSecurityAttribute = mdReader.GetDeclarativeSecurityAttribute(declSecurityAttributeHandle)

    let bytes = mdReader.GetBlobBytes(declSecurityAttribute.PermissionSet)
    ILSecurityDecl(readILSecurityAction declSecurityAttribute.Action, bytes)

let readILSecurityDeclsStored (mdReader: MetadataReader) (declSecurityAttributeHandles: DeclarativeSecurityAttributeHandleCollection) =
    [
        for declSecurityAttributeHandle in declSecurityAttributeHandles do
            yield readILSecurityDecl mdReader declSecurityAttributeHandle
    ]
    |> mkILSecurityDecls
    |> storeILSecurityDecls

let readILAssemblyLongevity (flags: AssemblyFlags) =
    let  masked = int flags &&& 0x000e
    if   masked = 0x0000 then ILAssemblyLongevity.Unspecified
    elif masked = 0x0002 then ILAssemblyLongevity.Library
    elif masked = 0x0004 then ILAssemblyLongevity.PlatformAppDomain
    elif masked = 0x0006 then ILAssemblyLongevity.PlatformProcess
    elif masked = 0x0008 then ILAssemblyLongevity.PlatformSystem
    else                      ILAssemblyLongevity.Unspecified

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
        ScopeRef = readILScopeRef mdReader exportedTy.Implementation
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

let readILAssemblyManifest (cenv: cenv) =
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

    {
        Name = mdReader.GetString(asmDef.Name)
        AuxModuleHashAlgorithm = int asmDef.HashAlgorithm
        SecurityDeclsStored = readILSecurityDeclsStored mdReader (asmDef.GetDeclarativeSecurityAttributes())
        PublicKey = publicKey
        Version = Some(mkVersionTuple asmDef.Version)
        Locale = locale
        CustomAttrsStored = readILAttributesStored cenv (asmDef.GetCustomAttributes())
        AssemblyLongevity = readILAssemblyLongevity flags
        DisableJitOptimizations = int (flags &&& AssemblyFlags.DisableJitCompileOptimizer) <> 0
        JitTracking = int (flags &&& AssemblyFlags.EnableJitCompileTracking) <> 0
        IgnoreSymbolStoreSequencePoints = (int flags &&& 0x2000) <> 0 // Not listed in AssemblyFlags
        Retargetable = int (flags &&& AssemblyFlags.Retargetable) <> 0
        ExportedTypes = readILExportedTypes cenv mdReader.ExportedTypes
        EntrypointElsewhere = None // TODO:
        MetadataIndex = 1 // always one
    }

let readModuleDef ilGlobals (peReader: PEReader) =
  //  let nativeResources = readNativeResources pectxtEager peReader ctxt.fileName

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

    let mdReader = peReader.GetMetadataReader()
    let moduleDef = mdReader.GetModuleDefinition()
    let ilModuleName = mdReader.GetString(moduleDef.Name)
    let ilMetadataVersion = mdReader.MetadataVersion
    
    let cenv = cenv(mdReader, SignatureTypeProvider(ilGlobals))

    { Manifest = Some(readILAssemblyManifest cenv)
      CustomAttrsStored = storeILCustomAttrs emptyILCustomAttrs //readILAttributesStored cenv (moduleDef.GetCustomAttributes())
      MetadataIndex = 1 // TODO: Is this right?
      Name = ilModuleName
      NativeResources = []
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
            readModuleDef ilReaderOptions.ilGlobals peReader

        member __.ILAssemblyRefs = []

      interface IDisposable with

        member __.Dispose() = ()
    }
