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

    //[<Sealed>]
    //type SignatureTypeProvider() =
    //    interface ISignatureTypeProvider<ILType, int> with

    //        member __.GetFunctionPointerType(s) = 
    //            let callingSig =
    //                {
    //                    CallingConv = ILCallingConv.Callconv(ILThisConvention.Static, ILArgConvention.Default)
    //                    ArgTypes = s.ParameterTypes |> Seq.toList
    //                    ReturnType = s.ReturnType
    //                }
    //            ILType.FunctionPointer(callingSig)

    //        member __.

//let rec convertAssemblyReferenceToILAssemblyRef (mdReader: MetadataReader) (asmRef: AssemblyReference) =
//    let name = mdReader.GetString(asmRef.Name)
//    let flags = asmRef.Flags

//    let hash = 
//        let hashValue = asmRef.HashValue
//        if hashValue.IsNil then None
//        else Some(mdReader.GetBlobBytes(hashValue))

//    let publicKey =
//        if asmRef.PublicKeyOrToken.IsNil then None
//        else 
//            let bytes = mdReader.GetBlobBytes(asmRef.PublicKeyOrToken)
//            let publicKey = 
//                if int (flags &&& AssemblyFlags.PublicKey) <> 0 then
//                    PublicKey(bytes)
//                else
//                    PublicKeyToken(bytes)
//            Some(publicKey)

//    let retargetable = int (flags &&& AssemblyFlags.Retargetable) <> 0

//    let v = asmRef.Version
//    let version = Some(uint16 v.Major, uint16 v.Minor, uint16 v.Build, uint16 v.Revision)

//    let locale =
//        let locale = mdReader.GetString(asmRef.Culture)
//        if String.IsNullOrWhiteSpace(locale) then None
//        else Some(locale)

//    ILAssemblyRef.Create(name, hash, publicKey, retargetable, version, locale)

//let rec convertModuleReferenceToILModuleRef (mdReader: System.Reflection.Metadata.MetadataReader) (modRef: ModuleReference) =
//    let name = mdReader.GetString(modRef.Name)
//    ILModuleRef.Create(name, hasMetadata = true, hash = None)

//let rec convertTypeReferenceToILTypeRef (mdReader: System.Reflection.Metadata.MetadataReader) (typeRef: TypeReference) =
//    let ilScopeRef =
//        match typeRef.ResolutionScope.Kind with
//        | HandleKind.AssemblyReference ->
//            let asmRef = mdReader.GetAssemblyReference(AssemblyReferenceHandle.op_Explicit(typeRef.ResolutionScope))
//            ILScopeRef.Assembly(convertAssemblyReferenceToILAssemblyRef mdReader asmRef)

//        | HandleKind.ModuleReference ->
//            let modRef = mdReader.GetModuleReference(ModuleReferenceHandle.op_Explicit(typeRef.ResolutionScope))
//            ILScopeRef.Module(convertModuleReferenceToILModuleRef mdReader modRef)

//        | HandleKind.LocalScope ->
//            ILScopeRef.Local

//        | _ ->
//            failwithf "Invalid Resolution Scope Handle Kind: %A" typeRef.ResolutionScope.Kind

//    let name = mdReader.GetString(typeRef.Name)
//    let namespac = mdReader.GetString(typeRef.Namespace)

//    ILTypeRef.Create(ilScopeRef, [], namespac + "." + name)

//let rec convertTypeDefinitionToILTypeRef (mdReader: System.Reflection.Metadata.MetadataReader) (typeDef: TypeDefinition) : ILTypeRef =
//    let enclosing =
//        if typeDef.IsNested then
//            let parentTypeDefHandle = typeDef.GetDeclaringType()
//            let parentTypeDef = mdReader.GetTypeDefinition(parentTypeDefHandle)
//            let ilTypeRef = convertTypeDefinitionToILTypeRef mdReader parentTypeDef
//            ilTypeRef.Enclosing @ [ ilTypeRef.Name ]
//        else
//            []
      
//    let name =
//        let name = mdReader.GetString(typeDef.Name)
//        if enclosing.Length > 0 then 
//            name
//        else
//            let namespac = mdReader.GetString(typeDef.Namespace)
//            namespac + "." + name

//    ILTypeRef.Create(ILScopeRef.Local, enclosing, name)

//let rec convertGenericParameterHandleToILGenericParameterDef (mdReader: System.Reflection.Metadata.MetadataReader) (genParamHandle: GenericParameterHandle) : ILGenericParameterDef =
//    let genParam = mdReader.GetGenericParameter(genParamHandle)
//    let attributes = genParam.Attributes

//    let constraints =
//        genParam.GetConstraints()
//        |> Seq.map (fun genParamCnstrHandle ->
//            let genParamCnstr = mdReader.GetGenericParameterConstraint(genParamCnstrHandle)
//            convertEntityHandleToILType mdReader genParamCnstr.Type
//        )
//        |> List.ofSeq     

//    let variance = 
//        if int (attributes &&& GenericParameterAttributes.Covariant) <> 0 then
//            ILGenericVariance.CoVariant
//        elif int (attributes &&& GenericParameterAttributes.Contravariant) <> 0 then
//            ILGenericVariance.ContraVariant
//        else
//            ILGenericVariance.NonVariant

//    {
//        Name = mdReader.GetString(genParam.Name)
//        Constraints = constraints
//        Variance = variance
//        HasReferenceTypeConstraint = int (attributes &&& GenericParameterAttributes.ReferenceTypeConstraint) <> 0
//        HasNotNullableValueTypeConstraint = int (attributes &&& GenericParameterAttributes.NotNullableValueTypeConstraint) <> 0
//        HasDefaultConstructorConstraint = int (attributes &&& GenericParameterAttributes.DefaultConstructorConstraint) <> 0
//        CustomAttrsStored = storeILCustomAttrs emptyILCustomAttrs // TODO:
//        MetadataIndex = MetadataTokens.GetRowNumber(GenericParameterHandle.op_Implicit(genParamHandle))
//    }

//let rec convertEntityHandleToILType (mdReader: System.Reflection.Metadata.MetadataReader) (handle: EntityHandle) : ILType =
//    match handle.Kind with
//    | HandleKind.TypeReference ->
//        let typeRef = mdReader.GetTypeReference(TypeReferenceHandle.op_Explicit(handle))
//        let ilTypeRef = convertTypeReferenceToILTypeRef mdReader typeRef
//        let ilTypeSpec = ILTypeSpec.Create(ilTypeRef, ILGenericArgs.Empty)
//        mkILTy AsObject (* AsObject probably not nok *) ilTypeSpec

//    | HandleKind.TypeDefinition ->
//        let typeDef = mdReader.GetTypeDefinition(TypeDefinitionHandle.op_Explicit(handle))
//        let ilTypeRef = convertTypeDefinitionToILTypeRef mdReader typeDef

//        let ilGenericParameterDefs =
//            typeDef.GetGenericParameters()
//            |> Seq.map (convertGenericParameterHandleToILGenericParameterDef mdReader)
//            |> List.ofSeq

//        let ilGenericArgs = mkILFormalGenericArgs 0 ilGenericParameterDefs

//        let ilTypeSpec = ILTypeSpec.Create(ilTypeRef, ilGenericArgs)
        
//        let boxity = 
//            if int (typeDef.Attributes &&& TypeAttributes.Class) <> 0 then AsObject
//            else AsValue

//        mkILTy boxity ilTypeSpec

//    | _ ->
//        failwithf "Invalid Handle Kind: %A" handle.Kind

//let rec convertEntityHandleToILMethodDef (mdReader: System.Reflection.Metadata.MetadataReader) (handle: EntityHandle) : ILMethodSpec =
//    match handle.Kind with
//    | HandleKind.MemberReference ->
//        let memberHandle = MemberReferenceHandle.op_Explicit(handle)
//        let memberRef = mdReader.GetMemberReference(memberHandle)
        
//        let memberEnclType = convertEntityHandleToILType mdReader memberRef.Parent

//        let memberName = mdReader.GetString(memberRef.Name)

//        let memberSigHeader, _memberSigTypeCode =
//            let blobReader = mdReader.GetBlobReader(memberRef.Signature)
//            blobReader.ReadSignatureHeader(), blobReader.ReadSignatureTypeCode()

//        let _memberCallConv =
//            if memberSigHeader.IsInstance then
//                ILCallingConv.Instance
//            else
//                ILCallingConv.Static

//        //let memberRetTy =
//        //    memberRef.

//        // let _ilMethRef = ILMethodRef.Create(memberEnclType, ILCallingConv.Instance, memberName, 0, [], )
//        //ILMethodSpec.Create(memberEnclType, ILMethodRef.)
//        failwith "not impl"
//    | HandleKind.MethodDefinition ->
//        let methodHandle = MethodDefinitionHandle.op_Explicit(handle)
//        let methodDef = mdReader.GetMethodDefinition(methodHandle)
//        //  methodDef.ImplAttributes = MethodImplAttributes.
//        let _ilType = convertEntityHandleToILType mdReader (TypeDefinitionHandle.op_Implicit(methodDef.GetDeclaringType()))
//        //ILMethodSpec.Create(ilType,)
//        failwith "not impl"
//    | _ ->
//        failwith "invalid metadata"  

//let rec seekReadModule (peReader: System.Reflection.PortableExecutable.PEReader) (ctxt: ILMetadataReader) (pectxtEager: PEReader) pevEager idx =
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
