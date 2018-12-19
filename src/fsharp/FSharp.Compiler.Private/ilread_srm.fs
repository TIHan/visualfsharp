module rec Microsoft.FSharp.Compiler.AbstractIL.ILReader

open System
open System.Reflection
open System.Reflection.PortableExecutable
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open Microsoft.FSharp.Compiler.AbstractIL.Internal 
#if !FX_NO_PDB_READER
open Microsoft.FSharp.Compiler.AbstractIL.Internal.Support 
#endif
open Microsoft.FSharp.Compiler.AbstractIL.Diagnostics 
open Microsoft.FSharp.Compiler.AbstractIL.Internal.BinaryConstants 
open Microsoft.FSharp.Compiler.AbstractIL.IL  
open Microsoft.FSharp.Compiler.AbstractIL.Internal.Library

let createVersionTuple (v: Version) =
    (uint16 v.Major, uint16 v.Minor, uint16 v.Build, uint16 v.Revision)    

let rec createILAssemblyRef (mdReader: MetadataReader) (asmRef: AssemblyReference) =
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

    let version = Some(createVersionTuple asmRef.Version)

    let locale =
        let locale = mdReader.GetString(asmRef.Culture)
        if String.IsNullOrWhiteSpace(locale) then None
        else Some(locale)

    ILAssemblyRef.Create(name, hash, publicKey, retargetable, version, locale)

let createILType (mdReader: MetadataReader) (handle: EntityHandle) : ILType =
    match handle.Kind with
    | HandleKind.TypeReference ->
        let typeRef = mdReader.GetTypeReference(TypeReferenceHandle.op_Explicit(handle))
        typeRef.ToILType(mdReader)

    | HandleKind.TypeDefinition ->
        let typeDef = mdReader.GetTypeDefinition(TypeDefinitionHandle.op_Explicit(handle))
        typeDef.ToILType(mdReader)

    | _ ->
        failwithf "Invalid Handle Kind: %A" handle.Kind

let createILModuleRef (mdReader: MetadataReader) (modRef: ModuleReference) =
    let name = mdReader.GetString(modRef.Name)
    ILModuleRef.Create(name, hasMetadata = true, hash = None)

let createILGenericParameterDef (mdReader: System.Reflection.Metadata.MetadataReader) (genParamHandle: GenericParameterHandle) : ILGenericParameterDef =
    let genParam = mdReader.GetGenericParameter(genParamHandle)
    let attributes = genParam.Attributes

    let constraints =
        genParam.GetConstraints()
        |> Seq.map (fun genParamCnstrHandle ->
            let genParamCnstr = mdReader.GetGenericParameterConstraint(genParamCnstrHandle)
            createILType mdReader genParamCnstr.Type
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
        CustomAttrsStored = storeILCustomAttrs emptyILCustomAttrs // TODO:
        MetadataIndex = MetadataTokens.GetRowNumber(GenericParameterHandle.op_Implicit(genParamHandle))
    }

type TypeReference with

    member typeRef.ToILTypeRef(mdReader: MetadataReader) : ILTypeRef =
        let ilScopeRef =
            match typeRef.ResolutionScope.Kind with
            | HandleKind.AssemblyReference ->
                let asmRef = mdReader.GetAssemblyReference(AssemblyReferenceHandle.op_Explicit(typeRef.ResolutionScope))
                ILScopeRef.Assembly(createILAssemblyRef mdReader asmRef)

            | HandleKind.ModuleReference ->
                let modRef = mdReader.GetModuleReference(ModuleReferenceHandle.op_Explicit(typeRef.ResolutionScope))
                ILScopeRef.Module(createILModuleRef mdReader modRef)

            | HandleKind.LocalScope ->
                ILScopeRef.Local

            | _ ->
                failwithf "Invalid Resolution Scope Handle Kind: %A" typeRef.ResolutionScope.Kind

        let name = mdReader.GetString(typeRef.Name)
        let namespac = mdReader.GetString(typeRef.Namespace)

        ILTypeRef.Create(ilScopeRef, [], namespac + "." + name)

    member typeRef.ToILType(mdReader) =
        let ilTypeRef = typeRef.ToILTypeRef(mdReader)
        let ilTypeSpec = ILTypeSpec.Create(ilTypeRef, ILGenericArgs.Empty)
        mkILTy AsObject (* AsObject probably not nok *) ilTypeSpec

type TypeDefinition with

    member typeDef.ToILTypeRef(mdReader: MetadataReader) : ILTypeRef =
        let enclosing =
            if typeDef.IsNested then
                let parentTypeDefHandle = typeDef.GetDeclaringType()
                let parentTypeDef = mdReader.GetTypeDefinition(parentTypeDefHandle)
                let ilTypeRef = parentTypeDef.ToILTypeRef(mdReader)
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

    member typeDef.ToILType(mdReader) =
        let ilTypeRef = typeDef.ToILTypeRef(mdReader)

        let ilGenericParameterDefs =
            typeDef.GetGenericParameters()
            |> Seq.map (createILGenericParameterDef mdReader)
            |> List.ofSeq

        let ilGenericArgs = mkILFormalGenericArgs 0 ilGenericParameterDefs

        let ilTypeSpec = ILTypeSpec.Create(ilTypeRef, ilGenericArgs)

        let boxity = 
            if int (typeDef.Attributes &&& TypeAttributes.Class) <> 0 then AsObject
            else AsValue

        mkILTy boxity ilTypeSpec

[<Sealed>]
type SignatureTypeProvider(ilg: ILGlobals) =
    interface ISignatureTypeProvider<ILType, ILGlobals> with

        member __.GetFunctionPointerType(si) =
            let sigHeader = si.Header

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

            let callingSig =
                {
                    CallingConv = ILCallingConv.Callconv(ilThisConvention, ilArgConvention)
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
            typeDef.ToILType(mdReader)

        member __.GetTypeFromReference(mdReader, typeRefHandle, _) =
            let typeRef = mdReader.GetTypeReference(typeRefHandle)
            typeRef.ToILType(mdReader)

    interface IConstructedTypeProvider<ILType> with

        member __.GetGenericInstantiation(genericType, _typeArguments) =
            genericType // TODO

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

let rec createILMethodSpec (mdReader: MetadataReader) (handle: EntityHandle) : ILMethodSpec =
    match handle.Kind with
    //| HandleKind.MemberReference ->
    //    let memberRef = mdReader.GetMemberReference(MemberReferenceHandle.op_Explicit(handle))
        
    //    let memberEnclType = createILType mdReader memberRef.Parent

    //    let memberName = mdReader.GetString(memberRef.Name)

    //    memberRef.

        //let memberSigHeader, memberSigTypeCode =
        //    let blobReader = mdReader.GetBlobReader(memberRef.Signature)
        //    blobReader.ReadSignatureHeader(), blobReader.ReadSignatureTypeCode()

        //let _memberCallConv =
        //    if memberSigHeader.IsInstance && memberSigTypeCode &&& SignatureTypeCode. then
        //        ILCallingConv.Instance
        //    else
        //        ILCallingConv.Static

        //let memberSig =
        //    memberRef.DecodeMethodSignature(SignatureTypeProvider(), ())

        //let memberRetTy = memberSig.ReturnType

        //let ilMethodRef = ILMethodRef.Create(memberEnclType, memberCallConv, memberName, 0, [],)

        //// let _ilMethRef = ILMethodRef.Create(memberEnclType, ILCallingConv.Instance, memberName, 0, [], )
        //ILMethodSpec.Create(memberEnclType, ILMethodRef, mkILFormalGenericArgs 0 )
     //   failwith "not impl"
    | HandleKind.MethodDefinition ->
        let methodDefHandle = MethodDefinitionHandle.op_Explicit(handle)
        let methodDef = mdReader.GetMethodDefinition(methodDefHandle)

        let name = mdReader.GetString(methodDef.Name)

        //  methodDef.ImplAttributes = MethodImplAttributes.
     //   let _ilType = convertEntityHandleToILType mdReader (TypeDefinitionHandle.op_Implicit(methodDef.GetDeclaringType()))
        //ILMethodSpec.Create(ilType,)
        failwith "not impl"
    | _ ->
        failwith "invalid metadata"

let createILSecurityAction (declSecurityAction: DeclarativeSecurityAction) =
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

let createILSecurityDecl (mdReader: MetadataReader) (declSecurityAttributeHandle: DeclarativeSecurityAttributeHandle) =
    let declSecurityAttribute = mdReader.GetDeclarativeSecurityAttribute(declSecurityAttributeHandle)

    let bytes = mdReader.GetBlobBytes(declSecurityAttribute.PermissionSet)
    ILSecurityDecl(createILSecurityAction declSecurityAttribute.Action, bytes)

let createILSecurityDeclsStored (mdReader: MetadataReader) =
    [
        for declSecurityAttributeHandle in mdReader.DeclarativeSecurityAttributes do
            yield createILSecurityDecl mdReader declSecurityAttributeHandle
    ]
    |> mkILSecurityDecls
    |> storeILSecurityDecls

let createILAssemblyLongevity (flags: AssemblyFlags) =
    let  masked = int flags &&& 0x000e
    if   masked = 0x0000 then ILAssemblyLongevity.Unspecified
    elif masked = 0x0002 then ILAssemblyLongevity.Library
    elif masked = 0x0004 then ILAssemblyLongevity.PlatformAppDomain
    elif masked = 0x0006 then ILAssemblyLongevity.PlatformProcess
    elif masked = 0x0008 then ILAssemblyLongevity.PlatformSystem
    else                      ILAssemblyLongevity.Unspecified

let createILAttribute (mdReader: MetadataReader) (customAttrHandle: CustomAttributeHandle) =
    let customAttr = mdReader.GetCustomAttribute(customAttrHandle)

    ILAttribute.Encoded(createILMethodSpec mdReader customAttr.Constructor, [||], [])

let createILAttributesStored (mdReader: MetadataReader) =
    [
        for customAttrHandle in mdReader.CustomAttributes do
            yield createILAttribute mdReader customAttrHandle
    ]
    |> mkILCustomAttrs
    |> storeILCustomAttrs

let createILAssemblyManifest (mdReader: MetadataReader) =
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
        SecurityDeclsStored = createILSecurityDeclsStored mdReader
        PublicKey = publicKey
        Version = Some(createVersionTuple asmDef.Version)
        Locale = locale
        CustomAttrsStored = createILAttributesStored mdReader
        AssemblyLongevity = createILAssemblyLongevity flags
        DisableJitOptimizations = int flags &&& int AssemblyFlags.DisableJitCompileOptimizer <> 0
        JitTracking = int flags &&& int AssemblyFlags.EnableJitCompileTracking <> 0
        IgnoreSymbolStoreSequencePoints = false // TODO
        Retargetable = int flags &&& int AssemblyFlags.Retargetable <> 0
        ExportedTypes = mkILExportedTypes [] // TODO:
        EntrypointElsewhere = None // TODO:
        MetadataIndex = 1 // always one because we reading the assembly definition from the first index
    }
        
let tryCreateILAssemblyManifest (peReader: PEReader) =
    Some(peReader.GetMetadataReader())

//let readModuleDef (peReader: PEReader) =
//    let mdv = ctxt.mdfile.GetView()
//    let nativeResources = readNativeResources pectxtEager peReader ctxt.fileName

//    let subsys =
//        int16 peReader.PEHeaders.PEHeader.Subsystem

//    let subsysversion =
//        (int32 peReader.PEHeaders.PEHeader.MajorSubsystemVersion, int32 peReader.PEHeaders.PEHeader.MinorSubsystemVersion)

//    let useHighEntropyVA =
//        int (peReader.PEHeaders.PEHeader.DllCharacteristics &&& DllCharacteristics.HighEntropyVirtualAddressSpace) <> 0

//    let ilOnly =
//        int (peReader.PEHeaders.CorHeader.Flags &&& CorFlags.ILOnly) <> 0

//    let only32 =
//        int (peReader.PEHeaders.CorHeader.Flags &&& CorFlags.Requires32Bit) <> 0

//    let is32bitpreferred =
//        int (peReader.PEHeaders.CorHeader.Flags &&& CorFlags.Prefers32Bit) <> 0

//    let only64 =
//        peReader.PEHeaders.CoffHeader.SizeOfOptionalHeader = 240s (* May want to read in the optional header Magic number and check that as well... *)

//    let platform = 
//        match peReader.PEHeaders.CoffHeader.Machine with
//        | Machine.Amd64 -> Some(AMD64)
//        | Machine.IA64 -> Some(IA64)
//        | _ -> Some(X86)

//    let isDll = peReader.PEHeaders.IsDll

//    let alignVirt =
//        peReader.PEHeaders.PEHeader.SectionAlignment

//    let alignPhys =
//        peReader.PEHeaders.PEHeader.FileAlignment

//    let imageBaseReal = int peReader.PEHeaders.PEHeader.ImageBase

//    let mdReader = peReader.GetMetadataReader()
//    let moduleDef = mdReader.GetModuleDefinition()
//    let ilModuleName = mdReader.GetString(moduleDef.Name)
//    let ilMetadataVersion = mdReader.MetadataVersion
    

//    { Manifest =
//         if ctxt.getNumRows (TableNames.Assembly) > 0 then Some (seekReadAssemblyManifest ctxt pectxtEager 1) 
//         else None
//      CustomAttrsStored = ctxt.customAttrsReader_Module
//      MetadataIndex = idx
//      Name = ilModuleName
//      NativeResources=nativeResources
//      TypeDefs = mkILTypeDefsComputed (fun () -> seekReadTopTypeDefs ctxt)
//      SubSystemFlags = int32 subsys
//      IsILOnly = ilOnly
//      SubsystemVersion = subsysversion
//      UseHighEntropyVA = useHighEntropyVA
//      Platform = platform
//      StackReserveSize = None  // TODO
//      Is32Bit = only32
//      Is32BitPreferred = is32bitpreferred
//      Is64Bit = only64
//      IsDLL=isDll
//      VirtualAlignment = alignVirt
//      PhysicalAlignment = alignPhys
//      ImageBase = imageBaseReal
//      MetadataVersion = ilMetadataVersion
//      Resources = seekReadManifestResources ctxt mdv pectxtEager pevEager }  
