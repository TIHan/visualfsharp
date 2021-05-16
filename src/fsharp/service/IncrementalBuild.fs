// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

namespace FSharp.Compiler.CodeAnalysis

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.IO
open System.Xml
open System.Runtime.InteropServices
open System.Threading
open Internal.Utilities.Library
open Internal.Utilities.Library.Extras
open FSharp.Compiler
open FSharp.Compiler.AbstractIL
open FSharp.Compiler.AbstractIL.IL
open FSharp.Compiler.AbstractIL.ILBinaryReader
open FSharp.Compiler.CheckExpressions
open FSharp.Compiler.CheckDeclarations
open FSharp.Compiler.CompilerConfig
open FSharp.Compiler.CompilerDiagnostics
open FSharp.Compiler.CompilerGlobalState
open FSharp.Compiler.CompilerImports
open FSharp.Compiler.CompilerOptions
open FSharp.Compiler.CreateILModule
open FSharp.Compiler.DependencyManager
open FSharp.Compiler.Diagnostics
open FSharp.Compiler.EditorServices
open FSharp.Compiler.ErrorLogger
open FSharp.Compiler.IO
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.NameResolution
open FSharp.Compiler.ParseAndCheckInputs
open FSharp.Compiler.ScriptClosure
open FSharp.Compiler.Syntax
open FSharp.Compiler.TcGlobals
open FSharp.Compiler.Text
open FSharp.Compiler.Text.Range
open FSharp.Compiler.Xml
open FSharp.Compiler.TypedTree
open FSharp.Compiler.TypedTreeOps

open Internal.Utilities
open Internal.Utilities.Collections

[<AutoOpen>]
module internal IncrementalBuild =

    let mutable injectCancellationFault = false
    let LocallyInjectCancellationFault() =
        injectCancellationFault <- true
        { new IDisposable with member _.Dispose() =  injectCancellationFault <- false }

// Record the most recent IncrementalBuilder events, so we can more easily unit test/debug the
// 'incremental' behavior of the product.
module IncrementalBuilderEventTesting =

    type internal FixedLengthMRU<'T>() =
        let MAX = 400   // Length of the MRU.  For our current unit tests, 400 is enough.
        let data = Array.create MAX None
        let mutable curIndex = 0
        let mutable numAdds = 0
        // called by the product, to note when a parse/typecheck happens for a file
        member this.Add(filename:'T) =
            numAdds <- numAdds + 1
            data.[curIndex] <- Some filename
            curIndex <- (curIndex + 1) % MAX
        member this.CurrentEventNum = numAdds
        // called by unit tests, returns 'n' most recent additions.
        member this.MostRecentList(n: int) : list<'T> =
            if n < 0 || n > MAX then
                raise <| new System.ArgumentOutOfRangeException("n", sprintf "n must be between 0 and %d, inclusive, but got %d" MAX n)
            let mutable remaining = n
            let mutable s = []
            let mutable i = curIndex - 1
            while remaining <> 0 do
                if i < 0 then
                    i <- MAX - 1
                match data.[i] with
                | None -> ()
                | Some x -> s <- x :: s
                i <- i - 1
                remaining <- remaining - 1
            List.rev s

    type IBEvent =
        | IBEParsed of string // filename
        | IBETypechecked of string // filename
        | IBECreated

    // ++GLOBAL MUTABLE STATE FOR TESTING++
    let MRU = new FixedLengthMRU<IBEvent>()
    let GetMostRecentIncrementalBuildEvents n = MRU.MostRecentList n
    let GetCurrentIncrementalBuildEventNum() = MRU.CurrentEventNum

module Tc = FSharp.Compiler.CheckExpressions

// This module is only here to contain the SyntaxTree type as to avoid amiguity with the module FSharp.Compiler.Syntax.
[<AutoOpen>]
module IncrementalBuildSyntaxTree =

    /// Information needed to lazily parse a file to get a ParsedInput. Internally uses a weak cache.
    [<Sealed>]
    type SyntaxTree (tcConfig: TcConfig, fileParsed: Event<string>, lexResourceManager, sourceRange: range, filename: string, isLastCompiland) =

        let mutable weakCache: WeakReference<_> option = None

        let parse(sigNameOpt: QualifiedNameOfFile option) =
            let errorLogger = CompilationErrorLogger("Parse", tcConfig.errorSeverityOptions)
            // Return the disposable object that cleans up
            use _holder = new CompilationGlobalsScope(errorLogger, BuildPhase.Parse)

            try
                IncrementalBuilderEventTesting.MRU.Add(IncrementalBuilderEventTesting.IBEParsed filename)
                let lower = String.lowercase filename
                let canSkip = sigNameOpt.IsSome && FSharpImplFileSuffixes |> List.exists (FileSystemUtils.checkSuffix lower)
                let input =
                    if canSkip then
                        ParsedInput.ImplFile(
                            ParsedImplFileInput(
                                filename,
                                false,
                                sigNameOpt.Value,
                                [],
                                [],
                                [],
                                isLastCompiland
                            )
                        )
                    else
                        ParseOneInputFile(tcConfig, lexResourceManager, [], filename, isLastCompiland, errorLogger, (*retryLocked*)true)

                fileParsed.Trigger filename

                let res = input, sourceRange, filename, errorLogger.GetDiagnostics()
                // If we do not skip parsing the file, then we can cache the real result.
                if not canSkip then
                    weakCache <- Some(WeakReference<_>(res))
                res
            with exn ->
                let msg = sprintf "unexpected failure in SyntaxTree.parse\nerror = %s" (exn.ToString())
                System.Diagnostics.Debug.Assert(false, msg)
                failwith msg

        /// Parse the given file and return the given input.
        member _.Parse sigNameOpt =
            match weakCache with
            | Some weakCache ->
                match weakCache.TryGetTarget() with
                | true, res -> res
                | _ -> parse sigNameOpt
            | _ -> parse sigNameOpt

        member _.Invalidate() =
            weakCache <- None

        member _.FileName = filename

/// Accumulated results of type checking. The minimum amount of state in order to continue type-checking following files.
[<NoEquality; NoComparison>]
type TcInfo =
    {
        tcState: TcState
        tcEnvAtEndOfFile: TcEnv

        /// Disambiguation table for module names
        moduleNamesDict: ModuleNamesDict

        topAttribs: TopAttribs option

        latestCcuSigForFile: ModuleOrNamespaceType option

        /// Accumulated errors, last file first
        tcErrorsRev:(PhasedDiagnostic * FSharpDiagnosticSeverity)[] list

        tcDependencyFiles: string list

        sigNameOpt: (string * QualifiedNameOfFile) option
    }

    member x.TcErrors =
        Array.concat (List.rev x.tcErrorsRev)

/// Accumulated results of type checking. Optional data that isn't needed to type-check a file, but needed for more information for in tooling.
[<NoEquality; NoComparison>]
type TcInfoExtras =
    {
      /// Accumulated resolutions, last file first
      tcResolutionsRev: TcResolutions list

      /// Accumulated symbol uses, last file first
      tcSymbolUsesRev: TcSymbolUses list

      /// Accumulated 'open' declarations, last file first
      tcOpenDeclarationsRev: OpenDeclaration[] list

      /// Result of checking most recent file, if any
      latestImplFile: TypedImplFile option

      /// If enabled, stores a linear list of ranges and strings that identify an Item(symbol) in a file. Used for background find all references.
      itemKeyStore: ItemKeyStore option

      /// If enabled, holds semantic classification information for Item(symbol)s in a file.
      semanticClassificationKeyStore: SemanticClassificationKeyStore option
    }

    member x.TcSymbolUses =
        List.rev x.tcSymbolUsesRev

/// Accumulated results of type checking.
[<NoEquality; NoComparison>]
type TcInfoState =
    | PartialState of TcInfo
    | FullState of TcInfo * TcInfoExtras

    member this.TcInfo: TcInfo =
        match this with
        | PartialState tcInfo -> tcInfo
        | FullState(tcInfo, _) -> tcInfo

/// Bound model of an underlying syntax and typed tree.
[<Sealed>]
type BoundModel private (tcConfig: TcConfig,
                         tcGlobals: TcGlobals,
                         tcImports: TcImports,
                         keepAssemblyContents, keepAllBackgroundResolutions,
                         keepAllBackgroundSymbolUses,
                         enableBackgroundItemKeyStoreAndSemanticClassification,
                         enablePartialTypeChecking,
                         beforeFileChecked: Event<string>,
                         fileChecked: Event<string>,
                         prevTcInfo: TcInfo,
                         prevTcInfoExtras: (Async<TcInfoExtras option>),
                         syntaxTreeOpt: SyntaxTree option,
                         tcInfoStateOpt: TcInfoState option) as this =

    let mutable lazyTcInfoState = tcInfoStateOpt
    let gate = obj()

    let defaultTypeCheck () =
        async {
            match! prevTcInfoExtras with
            | Some prevTcInfoExtras ->
                return FullState(prevTcInfo, prevTcInfoExtras)
            | _ ->
                return PartialState prevTcInfo
        }

    let mutable lazyAsyncTcInfo =
        AsyncLazy(async {
            return! this.GetTcInfo()
        })

    let mutable lazyAsyncTcInfoExtras =
        AsyncLazy(async {
            let! res = this.GetTcInfoExtras()
            return Some res
        })

    let mutable lazyAsyncFullState =
        AsyncLazy(async {
            return! this.GetState(false)
        })

    let resetAsyncLazyComputations() =
        lazyAsyncTcInfo <-
            AsyncLazy(async {
                return! this.GetTcInfo()
            })

        lazyAsyncTcInfoExtras <-
            AsyncLazy(async {
                let! res = this.GetTcInfoExtras()
                return Some res
            })

        lazyAsyncFullState <-
            AsyncLazy(async {
                return! this.GetState(false)
            })

    member _.TcConfig = tcConfig

    member _.TcGlobals = tcGlobals

    member _.TcImports = tcImports

    member _.BackingSignature =
        match syntaxTreeOpt with
        | Some syntaxTree ->
            let sigFileName = Path.ChangeExtension(syntaxTree.FileName, ".fsi")
            match prevTcInfo.sigNameOpt with
            | Some (expectedSigFileName, sigName) when String.Equals(expectedSigFileName, sigFileName, StringComparison.OrdinalIgnoreCase) ->
                Some sigName
            | _ ->
                None
        | _ ->
            None

    member this.Invalidate() =
        lock gate (fun () ->
            let hasSig = this.BackingSignature.IsSome
            match lazyTcInfoState with
            // If partial checking is enabled and we have a backing sig file, then do nothing. The partial state contains the sig state.
            | Some(PartialState _) when enablePartialTypeChecking && hasSig -> ()
            // If partial checking is enabled and we have a backing sig file, then use the partial state. The partial state contains the sig state.
            | Some(FullState(tcInfo, _)) when enablePartialTypeChecking && hasSig -> 
                lazyTcInfoState <- Some(PartialState tcInfo)
                resetAsyncLazyComputations()
            | _ ->
                lazyTcInfoState <- None
                resetAsyncLazyComputations()

            // Always invalidate the syntax tree cache.
            syntaxTreeOpt
            |> Option.iter (fun x -> x.Invalidate())
        )

    member private this.GetState(partialCheck: bool) =
        async {
            let partialCheck =
                // Only partial check if we have enabled it.
                if enablePartialTypeChecking then partialCheck
                else false

            let mustCheck =
                match lazyTcInfoState, partialCheck with
                | None, _ -> true
                | Some(PartialState _), false -> true
                | _ -> false

            match lazyTcInfoState with
            | Some tcInfoState when not mustCheck -> return tcInfoState
            | _ ->
                lazyTcInfoState <- None
                let! tcInfoState = this.TypeCheck(partialCheck)
                lazyTcInfoState <- Some tcInfoState
                return tcInfoState
        }

    member this.Next(syntaxTree, tcInfo) =
        BoundModel(
            tcConfig,
            tcGlobals,
            tcImports,
            keepAssemblyContents,
            keepAllBackgroundResolutions,
            keepAllBackgroundSymbolUses,
            enableBackgroundItemKeyStoreAndSemanticClassification,
            enablePartialTypeChecking,
            beforeFileChecked,
            fileChecked,
            tcInfo,
            lazyAsyncTcInfoExtras.GetValueAsync(),
            Some syntaxTree,
            None)

    member this.FinishAsync(finalTcErrorsRev, finalTopAttribs) =
        async {
            let! _ = this.GetTcInfoAsync()
            let state = lazyTcInfoState.Value // should not be null at this point

            let finishTcInfo = { state.TcInfo  with tcErrorsRev = finalTcErrorsRev; topAttribs = finalTopAttribs }
            let finishState =
                match state with
                | PartialState(_) -> PartialState(finishTcInfo)
                | FullState(_, tcInfoExtras) -> FullState(finishTcInfo, tcInfoExtras)

            return
                BoundModel(
                    tcConfig,
                    tcGlobals,
                    tcImports,
                    keepAssemblyContents,
                    keepAllBackgroundResolutions,
                    keepAllBackgroundSymbolUses,
                    enableBackgroundItemKeyStoreAndSemanticClassification,
                    enablePartialTypeChecking,
                    beforeFileChecked,
                    fileChecked,
                    prevTcInfo,
                    prevTcInfoExtras,
                    syntaxTreeOpt,
                    Some finishState)
        }

    member private this.GetTcInfo() : Async<_> =
        async {
            let! state = this.GetState(true)
            return state.TcInfo
        }

    member this.GetTcInfoAsync() =
        lazyAsyncTcInfo.GetValueAsync()

    member this.TryTcInfo =
        match lazyTcInfoState with
        | Some(state) ->
            match state with
            | FullState(tcInfo, _)
            | PartialState(tcInfo) -> Some tcInfo
        | _ -> None

    member private this.GetTcInfoExtras() : Async<_> =
        async {
            let! state = this.GetState(false)
            match state with
            | FullState(_, tcInfoExtras) -> return tcInfoExtras
            | PartialState _ ->
                return
                    {
                        tcResolutionsRev = []
                        tcSymbolUsesRev = []
                        tcOpenDeclarationsRev = []
                        latestImplFile = None
                        itemKeyStore = None
                        semanticClassificationKeyStore = None
                    }
        }

    member this.GetTcInfoExtrasAsync() =
        lazyAsyncTcInfoExtras.GetValueAsync()

    member this.GetTcInfoWithExtrasAsync() =
        async {
            match! lazyAsyncFullState.GetValueAsync() with
            | FullState(tcInfo, tcInfoExtras) -> return tcInfo, tcInfoExtras
            | PartialState(tcInfo) ->
                let tcInfoExtras =
                    {
                        tcResolutionsRev = []
                        tcSymbolUsesRev = []
                        tcOpenDeclarationsRev = []
                        latestImplFile = None
                        itemKeyStore = None
                        semanticClassificationKeyStore = None
                    }
                return tcInfo, tcInfoExtras
        }

    member private this.TypeCheck (partialCheck: bool) : Async<TcInfoState> =
        match partialCheck, lazyTcInfoState with
        | true, Some (PartialState _ as state)
        | true, Some (FullState _ as state) -> async { return state }
        | false, Some (FullState _ as state) -> async { return state }
        | _ ->

        async {
            match syntaxTreeOpt with
            | None -> return! defaultTypeCheck ()
            | Some syntaxTree ->
                let sigNameOpt =
                    if partialCheck then
                        this.BackingSignature
                    else
                        None
                match syntaxTree.Parse sigNameOpt with
                | input, _sourceRange, filename, parseErrors ->

                    IncrementalBuilderEventTesting.MRU.Add(IncrementalBuilderEventTesting.IBETypechecked filename)
                    let capturingErrorLogger = CompilationErrorLogger("TypeCheck", tcConfig.errorSeverityOptions)
                    let errorLogger = GetErrorLoggerFilteringByScopedPragmas(false, GetScopedPragmasForInput input, capturingErrorLogger)
                    use _ = new CompilationGlobalsScope (errorLogger, BuildPhase.TypeCheck) :> IDisposable

                    beforeFileChecked.Trigger filename
                    let prevModuleNamesDict = prevTcInfo.moduleNamesDict
                    let prevTcState = prevTcInfo.tcState
                    let prevTcErrorsRev = prevTcInfo.tcErrorsRev
                    let prevTcDependencyFiles = prevTcInfo.tcDependencyFiles

                    ApplyMetaCommandsFromInputToTcConfig (tcConfig, input, Path.GetDirectoryName filename, tcImports.DependencyProvider) |> ignore
                    let sink = TcResultsSinkImpl(tcGlobals)
                    let hadParseErrors = not (Array.isEmpty parseErrors)
                    let input, moduleNamesDict = DeduplicateParsedInputModuleName prevModuleNamesDict input

                    Logger.LogBlockMessageStart filename LogCompilerFunctionId.IncrementalBuild_TypeCheck

                    let! ct = Async.CancellationToken
                    let (tcEnvAtEndOfFile, topAttribs, implFile, ccuSigForFile), tcState =
                        let res =
                            eventually {
                                return!
                                    TypeCheckOneInputEventually
                                        ((fun () -> hadParseErrors || errorLogger.ErrorCount > 0),
                                            tcConfig, tcImports,
                                            tcGlobals,
                                            None,
                                            (if partialCheck then TcResultsSink.NoSink else TcResultsSink.WithSink sink),
                                            prevTcState, input,
                                            partialCheck)
                            } 
                            |> Eventually.reusing (fun () -> new CompilationGlobalsScope (errorLogger, BuildPhase.TypeCheck) :> IDisposable)
                            |> Eventually.force ct
                        match res with
                        | ValueOrCancelled.Cancelled ex -> raise ex
                        | ValueOrCancelled.Value res -> res
                        

                    Logger.LogBlockMessageStop filename LogCompilerFunctionId.IncrementalBuild_TypeCheck

                    fileChecked.Trigger filename
                    let newErrors = Array.append parseErrors (capturingErrorLogger.GetDiagnostics())

                    let tcEnvAtEndOfFile = if keepAllBackgroundResolutions then tcEnvAtEndOfFile else tcState.TcEnvFromImpls

                    let tcInfo =
                        {
                            tcState = tcState
                            tcEnvAtEndOfFile = tcEnvAtEndOfFile
                            moduleNamesDict = moduleNamesDict
                            latestCcuSigForFile = Some ccuSigForFile
                            tcErrorsRev = newErrors :: prevTcErrorsRev
                            topAttribs = Some topAttribs
                            tcDependencyFiles = filename :: prevTcDependencyFiles
                            sigNameOpt =
                                match input with
                                | ParsedInput.SigFile(ParsedSigFileInput(fileName=fileName;qualifiedNameOfFile=qualName)) ->
                                    Some(fileName, qualName)
                                | _ ->
                                    None
                        }

                    if partialCheck then
                        return PartialState tcInfo
                    else
                        match! prevTcInfoExtras with
                        | None -> return PartialState tcInfo
                        | Some prevTcInfoOptional ->
                            // Build symbol keys
                            let itemKeyStore, semanticClassification =
                                if enableBackgroundItemKeyStoreAndSemanticClassification then
                                    Logger.LogBlockMessageStart filename LogCompilerFunctionId.IncrementalBuild_CreateItemKeyStoreAndSemanticClassification
                                    let sResolutions = sink.GetResolutions()
                                    let builder = ItemKeyStoreBuilder()
                                    let preventDuplicates = HashSet({ new IEqualityComparer<struct(pos * pos)> with
                                                                        member _.Equals((s1, e1): struct(pos * pos), (s2, e2): struct(pos * pos)) = Position.posEq s1 s2 && Position.posEq e1 e2
                                                                        member _.GetHashCode o = o.GetHashCode() })
                                    sResolutions.CapturedNameResolutions
                                    |> Seq.iter (fun cnr ->
                                        let r = cnr.Range
                                        if preventDuplicates.Add struct(r.Start, r.End) then
                                            builder.Write(cnr.Range, cnr.Item))

                                    let semanticClassification = sResolutions.GetSemanticClassification(tcGlobals, tcImports.GetImportMap(), sink.GetFormatSpecifierLocations(), None)

                                    let sckBuilder = SemanticClassificationKeyStoreBuilder()
                                    sckBuilder.WriteAll semanticClassification

                                    let res = builder.TryBuildAndReset(), sckBuilder.TryBuildAndReset()
                                    Logger.LogBlockMessageStop filename LogCompilerFunctionId.IncrementalBuild_CreateItemKeyStoreAndSemanticClassification
                                    res
                                else
                                    None, None

                            let tcInfoExtras =
                                {
                                    /// Only keep the typed interface files when doing a "full" build for fsc.exe, otherwise just throw them away
                                    latestImplFile = if keepAssemblyContents then implFile else None
                                    tcResolutionsRev = (if keepAllBackgroundResolutions then sink.GetResolutions() else TcResolutions.Empty) :: prevTcInfoOptional.tcResolutionsRev
                                    tcSymbolUsesRev = (if keepAllBackgroundSymbolUses then sink.GetSymbolUses() else TcSymbolUses.Empty) :: prevTcInfoOptional.tcSymbolUsesRev
                                    tcOpenDeclarationsRev = sink.GetOpenDeclarations() :: prevTcInfoOptional.tcOpenDeclarationsRev
                                    itemKeyStore = itemKeyStore
                                    semanticClassificationKeyStore = semanticClassification
                                }

                            return FullState(tcInfo, tcInfoExtras)
            }

    static member Create(tcConfig: TcConfig,
                         tcGlobals: TcGlobals,
                         tcImports: TcImports,
                         keepAssemblyContents, keepAllBackgroundResolutions,
                         keepAllBackgroundSymbolUses,
                         enableBackgroundItemKeyStoreAndSemanticClassification,
                         enablePartialTypeChecking,
                         beforeFileChecked: Event<string>,
                         fileChecked: Event<string>,
                         prevTcInfo: TcInfo,
                         prevTcInfoExtras: Async<TcInfoExtras option>,
                         syntaxTreeOpt: SyntaxTree option) =
        BoundModel(tcConfig, tcGlobals, tcImports,
                      keepAssemblyContents, keepAllBackgroundResolutions,
                      keepAllBackgroundSymbolUses,
                      enableBackgroundItemKeyStoreAndSemanticClassification,
                      enablePartialTypeChecking,
                      beforeFileChecked,
                      fileChecked,
                      prevTcInfo,
                      prevTcInfoExtras,
                      syntaxTreeOpt,
                      None)

/// Global service state
type FrameworkImportsCacheKey = (*resolvedpath*)string list * string * (*TargetFrameworkDirectories*)string list * (*fsharpBinaries*)string * (*langVersion*)decimal

/// Represents a cache of 'framework' references that can be shared between multiple incremental builds
type FrameworkImportsCache(size) =

    // Mutable collection protected via CompilationThreadToken
    let frameworkTcImportsCache = AgedLookup<CompilationThreadToken, FrameworkImportsCacheKey, (TcGlobals * TcImports)>(size, areSimilar=(fun (x, y) -> x = y))

    /// Reduce the size of the cache in low-memory scenarios
    member _.Downsize ctok = frameworkTcImportsCache.Resize(ctok, newKeepStrongly=0)

    /// Clear the cache
    member _.Clear ctok = frameworkTcImportsCache.Clear ctok

    /// This function strips the "System" assemblies from the tcConfig and returns a age-cached TcImports for them.
    member _.Get(ctok, tcConfig: TcConfig) =
      cancellable {
        // Split into installed and not installed.
        let frameworkDLLs, nonFrameworkResolutions, unresolved = TcAssemblyResolutions.SplitNonFoundationalResolutions(ctok, tcConfig)
        let frameworkDLLsKey =
            frameworkDLLs
            |> List.map (fun ar->ar.resolvedPath) // The cache key. Just the minimal data.
            |> List.sort  // Sort to promote cache hits.

        let! tcGlobals, frameworkTcImports =
          cancellable {
            // Prepare the frameworkTcImportsCache
            //
            // The data elements in this key are very important. There should be nothing else in the TcConfig that logically affects
            // the import of a set of framework DLLs into F# CCUs. That is, the F# CCUs that result from a set of DLLs (including
            // FSharp.Core.dll and mscorlib.dll) must be logically invariant of all the other compiler configuration parameters.
            let key = (frameworkDLLsKey,
                        tcConfig.primaryAssembly.Name,
                        tcConfig.GetTargetFrameworkDirectories(),
                        tcConfig.fsharpBinariesDir,
                        tcConfig.langVersion.SpecifiedVersion)

            match frameworkTcImportsCache.TryGet (ctok, key) with
            | Some res -> return res
            | None ->
                let tcConfigP = TcConfigProvider.Constant tcConfig
                let! ((tcGlobals, tcImports) as res) = TcImports.BuildFrameworkTcImports (ctok, tcConfigP, frameworkDLLs, nonFrameworkResolutions)
                frameworkTcImportsCache.Put(ctok, key, res)
                return tcGlobals, tcImports
          }
        return tcGlobals, frameworkTcImports, nonFrameworkResolutions, unresolved
      }

/// Represents the interim state of checking an assembly
[<Sealed>]
type PartialCheckResults (boundModel: BoundModel, timeStamp: DateTime) =

    member _.TcImports = boundModel.TcImports

    member _.TcGlobals = boundModel.TcGlobals

    member _.TcConfig = boundModel.TcConfig

    member _.TimeStamp = timeStamp

    member _.TryTcInfo = boundModel.TryTcInfo

    member _.GetTcInfo() = boundModel.GetTcInfoAsync()

    member _.GetTcInfoWithExtras() = boundModel.GetTcInfoWithExtrasAsync()

    member _.TryGetItemKeyStore() =
        async {
            let! _, info = boundModel.GetTcInfoWithExtrasAsync()
            return info.itemKeyStore
        }

    member _.GetSemanticClassification() =
        async {
            let! _, info = boundModel.GetTcInfoWithExtrasAsync()
            return info.semanticClassificationKeyStore
        }

[<AutoOpen>]
module Utilities =
    let TryFindFSharpStringAttribute tcGlobals attribSpec attribs =
        match TryFindFSharpAttribute tcGlobals attribSpec attribs with
        | Some (Attrib(_, _, [ AttribStringArg s ], _, _, _, _))  -> Some s
        | _ -> None

/// The implementation of the information needed by TcImports in CompileOps.fs for an F# assembly reference.
//
/// Constructs the build data (IRawFSharpAssemblyData) representing the assembly when used
/// as a cross-assembly reference.  Note the assembly has not been generated on disk, so this is
/// a virtualized view of the assembly contents as computed by background checking.
type RawFSharpAssemblyDataBackedByLanguageService (tcConfig, tcGlobals, tcState: TcState, outfile, topAttrs, assemblyName, ilAssemRef) =

    let generatedCcu = tcState.Ccu
    let exportRemapping = MakeExportRemapping generatedCcu generatedCcu.Contents

    let sigData =
        let _sigDataAttributes, sigDataResources = EncodeSignatureData(tcConfig, tcGlobals, exportRemapping, generatedCcu, outfile, true)
        [ for r in sigDataResources  do
            let ccuName = GetSignatureDataResourceName r
            yield (ccuName, (fun () -> r.GetBytes())) ]

    let autoOpenAttrs = topAttrs.assemblyAttrs |> List.choose (List.singleton >> TryFindFSharpStringAttribute tcGlobals tcGlobals.attrib_AutoOpenAttribute)

    let ivtAttrs = topAttrs.assemblyAttrs |> List.choose (List.singleton >> TryFindFSharpStringAttribute tcGlobals tcGlobals.attrib_InternalsVisibleToAttribute)

    interface IRawFSharpAssemblyData with
        member _.GetAutoOpenAttributes(_ilg) = autoOpenAttrs
        member _.GetInternalsVisibleToAttributes(_ilg) =  ivtAttrs
        member _.TryGetILModuleDef() = None
        member _.GetRawFSharpSignatureData(_m, _ilShortAssemName, _filename) = sigData
        member _.GetRawFSharpOptimizationData(_m, _ilShortAssemName, _filename) = [ ]
        member _.GetRawTypeForwarders() = mkILExportedTypes []  // TODO: cross-project references with type forwarders
        member _.ShortAssemblyName = assemblyName
        member _.ILScopeRef = IL.ILScopeRef.Assembly ilAssemRef
        member _.ILAssemblyRefs = [] // These are not significant for service scenarios
        member _.HasAnyFSharpSignatureDataAttribute =  true
        member _.HasMatchingFSharpSignatureDataAttribute _ilg = true

type IncrementalBuilderState =
    {
        stampedFileNames: DateTime []
        stampedReferencedAssemblies: DateTime []
        initialBoundModel: BoundModel
        boundModels: ImmutableArray<BoundModelLazy>
        finalizedBoundModel: AsyncLazy<((ILAssemblyRef * IRawFSharpAssemblyData option * TypedImplFile list option * BoundModel) * DateTime)>
    }

and BoundModelLazy (refState: IncrementalBuilderState ref, i, syntaxTree: SyntaxTree, enablePartialTypeChecking) =

    /// Type check all files eagerly.
    let TypeCheckTask partialCheck (prevBoundModel: BoundModel) syntaxTree: Async<BoundModel> =
        async {
            let! tcInfo = prevBoundModel.GetTcInfoAsync()
            let boundModel = prevBoundModel.Next(syntaxTree, tcInfo)

            // Eagerly type check
            // We need to do this to keep the expected behavior of events (namely fileChecked) when checking a file/project.
            if partialCheck then
                let! _ = boundModel.GetTcInfoAsync()
                ()
            else
                let! _ = boundModel.GetTcInfoWithExtrasAsync()
                ()

            return boundModel
        }

    let mkLazy partialCheck =
        AsyncLazy(async {
            let state = !refState

            let! prevBoundModel =
                match i with
                | 0 (* first file *) -> async { return state.initialBoundModel }
                | _ -> state.boundModels.[i - 1].GetPartial()
            return! TypeCheckTask partialCheck prevBoundModel syntaxTree
        })

    let lazyFull = mkLazy false
    // If partial type checking is not enabled, GetPartial will always return an eager evaluation of the full check.
    let lazyPartial =
        if enablePartialTypeChecking then
            mkLazy true
        else
            lazyFull
     
    member this.GetPartial() : Async<BoundModel> = lazyPartial.GetValueAsync()
    member this.TryGetPartial() = lazyPartial.TryGetValue()

    member this.GetFull() : Async<BoundModel> = lazyFull.GetValueAsync()
    member this.TryGetFull() = lazyFull.TryGetValue()

/// Manages an incremental build graph for the build of a single F# project
type IncrementalBuilder(
                        initialBoundModel: BoundModel,
                        tcGlobals,
                        nonFrameworkAssemblyInputs,
                        tcConfig: TcConfig,
                        outfile,
                        assemblyName,
                        lexResourceManager,
                        sourceFiles,
                        enablePartialTypeChecking,
                        beforeFileChecked: Event<string>,
                        fileChecked: Event<string>,
#if !NO_EXTENSIONTYPING
                        importsInvalidatedByTypeProvider: Event<unit>,
#endif
                        allDependencies) =

    let fileLookup =
        let items =
            sourceFiles
            |> Array.ofList
            |> Array.mapi (fun i (_, x, _) ->
                KeyValuePair(x, i)
            )
        Collections.Concurrent.ConcurrentDictionary(items, StringComparer.OrdinalIgnoreCase)

    let fileParsed = new Event<string>()
    let projectChecked = new Event<unit>()

    let mutable isImportsInvalidated = false

#if !NO_EXTENSIONTYPING
    do importsInvalidatedByTypeProvider.Publish.Add(fun () -> isImportsInvalidated <- true)
#endif

    //----------------------------------------------------
    // START OF BUILD TASK FUNCTIONS

    /// Finish up the typechecking to produce outputs for the rest of the compilation process
    let FinalizeTypeCheckTask (boundModels: ImmutableArray<BoundModel>) =
      async {
        let errorLogger = CompilationErrorLogger("FinalizeTypeCheckTask", tcConfig.errorSeverityOptions)
        use _ = new CompilationGlobalsScope (errorLogger, BuildPhase.TypeCheck)

        let! results =
            boundModels 
            |> Seq.map (fun boundModel -> async { 
                if enablePartialTypeChecking then
                    let! tcInfo = boundModel.GetTcInfoAsync()
                    return tcInfo, None
                else
                    let! tcInfo, tcInfoExtras = boundModel.GetTcInfoWithExtrasAsync()
                    return tcInfo, tcInfoExtras.latestImplFile
            })
            |> Seq.map (fun work ->
                async {
                    let! tcInfo, latestImplFile = work
                    return (tcInfo.tcEnvAtEndOfFile, defaultArg tcInfo.topAttribs EmptyTopAttrs, latestImplFile, tcInfo.latestCcuSigForFile)
                }
            )
            |> Async.Sequential

        let results = results |> List.ofSeq

        // Get the state at the end of the type-checking of the last file
        let finalBoundModel = boundModels.[boundModels.Length-1]

        let! finalInfo = finalBoundModel.GetTcInfoAsync()

        // Finish the checking
        let (_tcEnvAtEndOfLastFile, topAttrs, mimpls, _), tcState =
            TypeCheckMultipleInputsFinish (results, finalInfo.tcState)

        let ilAssemRef, tcAssemblyDataOpt, tcAssemblyExprOpt =
            try
                // TypeCheckClosedInputSetFinish fills in tcState.Ccu but in incremental scenarios we don't want this,
                // so we make this temporary here
                let oldContents = tcState.Ccu.Deref.Contents
                try
                    let tcState, tcAssemblyExpr = TypeCheckClosedInputSetFinish (mimpls, tcState)

                    // Compute the identity of the generated assembly based on attributes, options etc.
                    // Some of this is duplicated from fsc.fs
                    let ilAssemRef =
                        let publicKey =
                            try
                                let signingInfo = ValidateKeySigningAttributes (tcConfig, tcGlobals, topAttrs)
                                match GetStrongNameSigner signingInfo with
                                | None -> None
                                | Some s -> Some (PublicKey.KeyAsToken(s.PublicKey))
                            with e ->
                                errorRecoveryNoRange e
                                None
                        let locale = TryFindFSharpStringAttribute tcGlobals (tcGlobals.FindSysAttrib  "System.Reflection.AssemblyCultureAttribute") topAttrs.assemblyAttrs
                        let assemVerFromAttrib =
                            TryFindFSharpStringAttribute tcGlobals (tcGlobals.FindSysAttrib "System.Reflection.AssemblyVersionAttribute") topAttrs.assemblyAttrs
                            |> Option.bind  (fun v -> try Some (parseILVersion v) with _ -> None)
                        let ver =
                            match assemVerFromAttrib with
                            | None -> tcConfig.version.GetVersionInfo(tcConfig.implicitIncludeDir)
                            | Some v -> v
                        ILAssemblyRef.Create(assemblyName, None, publicKey, false, Some ver, locale)

                    let tcAssemblyDataOpt =
                        try

                          // Assemblies containing type provider components can not successfully be used via cross-assembly references.
                          // We return 'None' for the assembly portion of the cross-assembly reference
                          let hasTypeProviderAssemblyAttrib =
                              topAttrs.assemblyAttrs |> List.exists (fun (Attrib(tcref, _, _, _, _, _, _)) ->
                                  let nm = tcref.CompiledRepresentationForNamedType.BasicQualifiedName
                                  nm = typeof<Microsoft.FSharp.Core.CompilerServices.TypeProviderAssemblyAttribute>.FullName)

                          if tcState.CreatesGeneratedProvidedTypes || hasTypeProviderAssemblyAttrib then
                            None
                          else
                            Some  (RawFSharpAssemblyDataBackedByLanguageService (tcConfig, tcGlobals, tcState, outfile, topAttrs, assemblyName, ilAssemRef) :> IRawFSharpAssemblyData)

                        with e ->
                            errorRecoveryNoRange e
                            None
                    ilAssemRef, tcAssemblyDataOpt, Some tcAssemblyExpr
                finally
                    tcState.Ccu.Deref.Contents <- oldContents
            with e ->
                errorRecoveryNoRange e
                mkSimpleAssemblyRef assemblyName, None, None

        let diagnostics = errorLogger.GetDiagnostics() :: finalInfo.tcErrorsRev
        let! finalBoundModelWithErrors = finalBoundModel.FinishAsync(diagnostics, Some topAttrs)
        return ilAssemRef, tcAssemblyDataOpt, tcAssemblyExprOpt, finalBoundModelWithErrors
    }

    // END OF BUILD TASK FUNCTIONS
    // ---------------------------------------------------------------------------------------------

    // ---------------------------------------------------------------------------------------------
    // START OF BUILD DESCRIPTION

    let GetSyntaxTree (sourceRange: range, filename: string, isLastCompiland) =
        SyntaxTree(tcConfig, fileParsed, lexResourceManager, sourceRange, filename, isLastCompiland)

    // Inputs
    let fileNames = sourceFiles |> Array.ofList // TODO: This should be an immutable array.
    let referencedAssemblies =  nonFrameworkAssemblyInputs |> Array.ofList // TODO: This should be an immutable array.

    let createBoundModelAsyncLazy (refState: IncrementalBuilderState ref) i =
        let fileInfo = fileNames.[i]
        let syntaxTree = GetSyntaxTree fileInfo
        BoundModelLazy(refState, i, syntaxTree, enablePartialTypeChecking)

    let createBoundModelsAsyncLazy refState count =
        Array.init count (createBoundModelAsyncLazy refState)
        |> ImmutableArray.CreateRange

    let rec createFinalizeBoundModelAsyncLazy (state: IncrementalBuilderState ref) =
        AsyncLazy(async {
            let state = !state
            // Compute last bound model then get all the evaluated models.
            let! _ = state.boundModels.[state.boundModels.Length - 1].GetPartial()
            let boundModels =
                state.boundModels
                |> Seq.map (fun x -> x.TryGetPartial().Value)
                |> ImmutableArray.CreateRange

            let! result = FinalizeTypeCheckTask boundModels
            let result = (result, DateTime.UtcNow)
            return result
        })

    let tryGetSlotPartial (state: IncrementalBuilderState) slot =
        match state.boundModels.[slot].TryGetPartial() with
        | ValueSome boundModel ->
            (boundModel, state.stampedFileNames.[slot])
            |> Some
        | _ ->
            None

    let tryGetBeforeSlotPartial (state: IncrementalBuilderState) slot =
        match slot with
        | 0 (* first file *) ->
            (initialBoundModel, DateTime.MinValue)
            |> Some
        | _ ->
            tryGetSlotPartial state (slot - 1)

    let evalUpToTargetSlotPartial (state: IncrementalBuilderState) targetSlot =
        async {
            if targetSlot < 0 then
                return Some(initialBoundModel, DateTime.MinValue)
            else
                let! boundModel = state.boundModels.[targetSlot].GetPartial()
                return Some(boundModel, state.stampedFileNames.[targetSlot])
        }

    let evalUpToTargetSlotFull (state: IncrementalBuilderState) targetSlot =
        async {
            if targetSlot < 0 then
                return Some(initialBoundModel, DateTime.MinValue)
            else
                let! boundModel = state.boundModels.[targetSlot].GetFull()
                return Some(boundModel, state.stampedFileNames.[targetSlot])
        }

    let MaxTimeStampInDependencies stamps =
        if Seq.isEmpty stamps then
            DateTime.MinValue
        else
            stamps
            |> Seq.max

    // END OF BUILD DESCRIPTION
    // ---------------------------------------------------------------------------------------------

    (*
        The data below represents a dependency graph.

        ReferencedAssembliesStamps => FileStamps => BoundModels => FinalizedBoundModel
    *)

    let mutable currentState =
        let refState = ref Unchecked.defaultof<_>
        let state =
            {
                stampedFileNames = Array.init fileNames.Length (fun _ -> DateTime.MinValue)
                stampedReferencedAssemblies = Array.init referencedAssemblies.Length (fun _ -> DateTime.MinValue)
                initialBoundModel = initialBoundModel
                boundModels = createBoundModelsAsyncLazy refState fileNames.Length
                finalizedBoundModel = createFinalizeBoundModelAsyncLazy refState
            }
        refState := state
        state

    let gate = obj()
    let fileIsReady (filename: string) =
        try
            use _dispose = File.Open(filename, FileMode.Open, FileAccess.Read, FileShare.Read)
            true
        with
        | :? IOException ->
            false

    let createFileWatcher (dir: string) (filenames: string seq) =
        let fsw = new IO.FileSystemWatcher(dir)
        fsw.NotifyFilter <- NotifyFilters.LastWrite
        fsw.Changed.Add(fun args ->
            let filename = args.FullPath
            if args.ChangeType &&& IO.WatcherChangeTypes.Changed = IO.WatcherChangeTypes.Changed &&
               filenames |> Seq.exists (fun x -> String.Equals(x, filename, StringComparison.OrdinalIgnoreCase)) &&
               fileIsReady filename then

               lock gate (fun () ->
                    try
                        let state = currentState
                        let slot = fileLookup.[filename]
                        let boundModels = state.boundModels.ToBuilder()

                        match boundModels.[slot].TryGetPartial() with
                        // This prevents an implementation file that has a backing signature file from invalidating the rest of the build.
                        | ValueSome(boundModel) when enablePartialTypeChecking && boundModel.BackingSignature.IsSome ->
                            boundModel.Invalidate()
                        | _ ->
                            state.stampedFileNames.[slot] <- FileSystem.GetLastWriteTimeShim filename

                            let refState = ref state
                            for j = 0 to state.stampedFileNames.Length - slot - 1 do
                                boundModels.[slot + j] <- createBoundModelAsyncLazy refState (slot + j)

                            let state =
                                { state with
                                    // Something changed, the finalized view of the project must be invalidated.
                                    finalizedBoundModel = createFinalizeBoundModelAsyncLazy refState
                                    boundModels = boundModels.ToImmutable()
                                }
                            refState := state
                            currentState <- state
                    with
                    | _ -> ()
            )
        )
        fsw.EnableRaisingEvents <- true
        fsw 

    let fileWatchers =
        sourceFiles
        |> Seq.map (fun (_, filename, _) -> filename)
        |> Seq.groupBy (fun filename ->
            Path.GetDirectoryName(filename)
        )
        |> Seq.choose (fun (dir, filenames) ->
            try
                createFileWatcher dir filenames
                |> Some
            with
            | _ ->
                None
        )
        |> Array.ofSeq

    let references, inMemReferences =
        tcConfig.referencedDLLs
        |> Array.ofList
        |> Array.partition (fun x ->
            match x with
            | AssemblyReference(_, _, Some _) -> false
            | _ -> true
        )

    let createReferenceWatcher dir filenames =
        let fsw = new IO.FileSystemWatcher(dir)
        fsw.NotifyFilter <- NotifyFilters.LastWrite
        fsw.Changed.Add(fun args ->
            let filename = args.FullPath
            if args.ChangeType &&& IO.WatcherChangeTypes.Changed = IO.WatcherChangeTypes.Changed &&
               filenames |> Seq.exists (fun x -> String.Equals(x, filename, StringComparison.OrdinalIgnoreCase)) &&
               fileIsReady filename then
                isImportsInvalidated <- true
        )
        fsw.EnableRaisingEvents <- true
        fsw

    let referenceWatchers =
        references
        |> Seq.map (fun (AssemblyReference(_, filename, _)) -> filename)
        |> Seq.groupBy (fun filename ->
            Path.GetDirectoryName(filename)
        )
        |> Seq.choose (fun (dir, filenames) ->
            try
                createReferenceWatcher dir filenames
                |> Some
            with
            | _ ->
                None
        )
        |> Array.ofSeq

    let mutable stampedInMemReferences = Array.init inMemReferences.Length (fun _ -> DateTime.MinValue)
    let computeStampedInMemReferences cache canInvalidate =
        inMemReferences
        |> Array.iteri (fun i x ->
            match x with
            | AssemblyReference(_, _, Some projRef) ->
                match projRef.TryGetLogicalTimeStamp(cache) with
                | Some stamp ->
                    let currentStamp = stampedInMemReferences.[i]
                    if stamp > currentStamp then
                        if canInvalidate then
                            isImportsInvalidated <- true
                        stampedInMemReferences.[i] <- stamp
                | _ ->
                    ()
            | _ ->
                failwith "invalid project reference"
        )

    do computeStampedInMemReferences (TimeStampCache(DateTime.MinValue)) false

    do IncrementalBuilderEventTesting.MRU.Add(IncrementalBuilderEventTesting.IBECreated)

    override _.Finalize() =
        fileWatchers
        |> Array.iter (fun x -> x.Dispose())

        referenceWatchers
        |> Array.iter (fun x -> x.Dispose())

    member _.TcConfig = tcConfig

    member _.FileParsed = fileParsed.Publish

    member _.BeforeFileChecked = beforeFileChecked.Publish

    member _.FileChecked = fileChecked.Publish

    member _.ProjectChecked = projectChecked.Publish

#if !NO_EXTENSIONTYPING
    member _.ImportsInvalidatedByTypeProvider = importsInvalidatedByTypeProvider.Publish
#endif

    member _.IsImportsInvalidated = 
        if isImportsInvalidated then true
        else 
            computeStampedInMemReferences (TimeStampCache(DateTime.MinValue)) true
            isImportsInvalidated

    member _.AllDependenciesDeprecated = allDependencies

    member _.PopulatePartialCheckingResults () =
      async {
        computeStampedInMemReferences (TimeStampCache(DateTime.MinValue)) true
        let! _ = currentState.finalizedBoundModel.GetValueAsync()
        projectChecked.Trigger()
      }

    member builder.GetCheckResultsBeforeFileInProjectEvenIfStale filename: PartialCheckResults option  =
        let slotOfFile = builder.GetSlotOfFileName filename
        let result = tryGetBeforeSlotPartial currentState slotOfFile

        match result with
        | Some (boundModel, timestamp) -> Some (PartialCheckResults (boundModel, timestamp))
        | _ -> None

    member builder.TryGetCheckResultsBeforeFileInProject (filename) =
        let slotOfFile = builder.GetSlotOfFileName filename
        match tryGetBeforeSlotPartial currentState slotOfFile with
        | Some(boundModel, timestamp) -> PartialCheckResults(boundModel, timestamp) |> Some
        | _ -> None

    member builder.AreCheckResultsBeforeFileInProjectReady filename =
        (builder.TryGetCheckResultsBeforeFileInProject filename).IsSome

    member _.GetCheckResultsBeforeSlotInProject (slotOfFile) =
      async {
        computeStampedInMemReferences (TimeStampCache(DateTime.MinValue)) true
        let! result = evalUpToTargetSlotPartial currentState (slotOfFile - 1)
        match result with
        | Some (boundModel, timestamp) -> return PartialCheckResults(boundModel, timestamp)
        | None -> return! failwith "Expected results to be ready. (GetCheckResultsBeforeSlotInProject)."
      }

    member _.GetFullCheckResultsBeforeSlotInProject (slotOfFile) =
      async {
        computeStampedInMemReferences (TimeStampCache(DateTime.MinValue)) true
        let! result = evalUpToTargetSlotFull currentState (slotOfFile - 1)
        match result with
        | Some (boundModel, timestamp) -> return PartialCheckResults(boundModel, timestamp)
        | None -> return! failwith "Expected results to be ready. (GetFullCheckResultsBeforeSlotInProject)."
      }

    member builder.GetCheckResultsBeforeFileInProject (filename) =
        let slotOfFile = builder.GetSlotOfFileName filename
        builder.GetCheckResultsBeforeSlotInProject (slotOfFile)

    member builder.GetCheckResultsAfterFileInProject (filename) =
        let slotOfFile = builder.GetSlotOfFileName filename + 1
        builder.GetCheckResultsBeforeSlotInProject (slotOfFile)

    member builder.GetFullCheckResultsBeforeFileInProject (filename) =
        let slotOfFile = builder.GetSlotOfFileName filename
        builder.GetFullCheckResultsBeforeSlotInProject (slotOfFile)

    member builder.GetFullCheckResultsAfterFileInProject (filename) =
        async {
            let slotOfFile = builder.GetSlotOfFileName filename + 1
            let! result = builder.GetFullCheckResultsBeforeSlotInProject(slotOfFile)
            return result
        }

    member builder.GetCheckResultsAfterLastFileInProject () =
        builder.GetCheckResultsBeforeSlotInProject(builder.GetSlotsCount())

    member _.GetCheckResultsAndImplementationsForProject() =
      async {
        computeStampedInMemReferences (TimeStampCache(DateTime.MinValue)) true
        let! result = currentState.finalizedBoundModel.GetValueAsync()
        match result with
        | ((ilAssemRef, tcAssemblyDataOpt, tcAssemblyExprOpt, boundModel), timestamp) ->
            return PartialCheckResults (boundModel, timestamp), ilAssemRef, tcAssemblyDataOpt, tcAssemblyExprOpt
      }

    member builder.GetFullCheckResultsAndImplementationsForProject() =
        async {
            let! result = builder.GetCheckResultsAndImplementationsForProject()
            let results, _, _, _ = result
            let! _ = results.GetTcInfoWithExtras() // Make sure we forcefully evaluate the info
            return result
        }

    member _.GetLogicalTimeStampForProject(_cache: TimeStampCache) =
        let state = currentState
        let t1 = MaxTimeStampInDependencies state.stampedReferencedAssemblies
        let t2 = MaxTimeStampInDependencies state.stampedFileNames
        let t3 = MaxTimeStampInDependencies stampedInMemReferences
        max (max t1 t2) t3

    member _.TryGetSlotOfFileName(filename: string) =
        // Get the slot of the given file and force it to build.
        let CompareFileNames (_, f2, _) =
            let result =
                   String.Compare(filename, f2, StringComparison.CurrentCultureIgnoreCase)=0
                || String.Compare(FileSystem.GetFullPathShim filename, FileSystem.GetFullPathShim f2, StringComparison.CurrentCultureIgnoreCase)=0
            result
        match fileNames |> Array.tryFindIndex CompareFileNames with
        | Some slot -> Some slot
        | None -> None

    member this.GetSlotOfFileName(filename: string) =
        match this.TryGetSlotOfFileName(filename) with
        | Some slot -> slot
        | None -> failwith (sprintf "The file '%s' was not part of the project. Did you call InvalidateConfiguration when the list of files in the project changed?" filename)

    member _.GetSlotsCount () = fileNames.Length

    member this.ContainsFile(filename: string) =
        (this.TryGetSlotOfFileName filename).IsSome

    member builder.GetParseResultsForFile (filename) =
        let slotOfFile = builder.GetSlotOfFileName filename
        let fileInfo = fileNames.[slotOfFile]
        // re-parse on demand instead of retaining
        let syntaxTree = GetSyntaxTree fileInfo
        syntaxTree.Parse None

    member _.SourceFiles  = sourceFiles  |> List.map (fun (_, f, _) -> f)

    /// CreateIncrementalBuilder (for background type checking). Note that fsc.fs also
    /// creates an incremental builder used by the command line compiler.
    static member TryCreateIncrementalBuilderForProjectOptions
                      (ctok, legacyReferenceResolver, defaultFSharpBinariesDir,
                       frameworkTcImportsCache: FrameworkImportsCache,
                       loadClosureOpt: LoadClosure option,
                       sourceFiles: string list,
                       commandLineArgs: string list,
                       projectReferences, projectDirectory,
                       useScriptResolutionRules, keepAssemblyContents,
                       keepAllBackgroundResolutions,
                       tryGetMetadataSnapshot, suggestNamesForErrors,
                       keepAllBackgroundSymbolUses,
                       enableBackgroundItemKeyStoreAndSemanticClassification,
                       enablePartialTypeChecking: bool,
                       dependencyProvider) =

      let useSimpleResolutionSwitch = "--simpleresolution"

      cancellable {

        // Trap and report warnings and errors from creation.
        let delayedLogger = CapturingErrorLogger("IncrementalBuilderCreation")
        use _unwindEL = PushErrorLoggerPhaseUntilUnwind (fun _ -> delayedLogger)
        use _unwindBP = PushThreadBuildPhaseUntilUnwind BuildPhase.Parameter

        let! builderOpt =
         cancellable {
          try

            // Create the builder.
            // Share intern'd strings across all lexing/parsing
            let resourceManager = new Lexhelp.LexResourceManager()

            /// Create a type-check configuration
            let tcConfigB, sourceFiles =

                let getSwitchValue switchString =
                    match commandLineArgs |> Seq.tryFindIndex(fun s -> s.StartsWithOrdinal switchString) with
                    | Some idx -> Some(commandLineArgs.[idx].Substring(switchString.Length))
                    | _ -> None

                let sdkDirOverride =
                    match loadClosureOpt with
                    | None -> None
                    | Some loadClosure -> loadClosure.SdkDirOverride

                // see also fsc.fs: runFromCommandLineToImportingAssemblies(), as there are many similarities to where the PS creates a tcConfigB
                let tcConfigB =
                    TcConfigBuilder.CreateNew(legacyReferenceResolver,
                         defaultFSharpBinariesDir,
                         implicitIncludeDir=projectDirectory,
                         reduceMemoryUsage=ReduceMemoryFlag.Yes,
                         isInteractive=useScriptResolutionRules,
                         isInvalidationSupported=true,
                         defaultCopyFSharpCore=CopyFSharpCoreFlag.No,
                         tryGetMetadataSnapshot=tryGetMetadataSnapshot,
                         sdkDirOverride=sdkDirOverride,
                         rangeForErrors=range0)

                tcConfigB.primaryAssembly <-
                    match loadClosureOpt with
                    | None -> PrimaryAssembly.Mscorlib
                    | Some loadClosure ->
                        if loadClosure.UseDesktopFramework then
                            PrimaryAssembly.Mscorlib
                        else
                            PrimaryAssembly.System_Runtime

                tcConfigB.resolutionEnvironment <- (LegacyResolutionEnvironment.EditingOrCompilation true)

                tcConfigB.conditionalCompilationDefines <-
                    let define = if useScriptResolutionRules then "INTERACTIVE" else "COMPILED"
                    define :: tcConfigB.conditionalCompilationDefines

                tcConfigB.projectReferences <- projectReferences

                tcConfigB.useSimpleResolution <- (getSwitchValue useSimpleResolutionSwitch) |> Option.isSome

                // Apply command-line arguments and collect more source files if they are in the arguments
                let sourceFilesNew = ApplyCommandLineArgs(tcConfigB, sourceFiles, commandLineArgs)

                // Never open PDB files for the language service, even if --standalone is specified
                tcConfigB.openDebugInformationForLaterStaticLinking <- false

                tcConfigB.xmlDocInfoLoader <-
                    { new IXmlDocumentationInfoLoader with
                        /// Try to load xml documentation associated with an assembly by the same file path with the extension ".xml".
                        member _.TryLoad(assemblyFileName, _ilModule) =
                            let xmlFileName = Path.ChangeExtension(assemblyFileName, ".xml")

                            // REVIEW: File IO - Will eventually need to change this to use a file system interface of some sort.
                            XmlDocumentationInfo.TryCreateFromFile(xmlFileName)
                    }
                    |> Some

                tcConfigB, sourceFilesNew

            // If this is a builder for a script, re-apply the settings inferred from the
            // script and its load closure to the configuration.
            //
            // NOTE: it would probably be cleaner and more accurate to re-run the load closure at this point.
            match loadClosureOpt with
            | Some loadClosure ->
                let dllReferences =
                    [for reference in tcConfigB.referencedDLLs do
                        // If there's (one or more) resolutions of closure references then yield them all
                        match loadClosure.References  |> List.tryFind (fun (resolved, _)->resolved=reference.Text) with
                        | Some (resolved, closureReferences) ->
                            for closureReference in closureReferences do
                                yield AssemblyReference(closureReference.originalReference.Range, resolved, None)
                        | None -> yield reference]
                tcConfigB.referencedDLLs <- []
                tcConfigB.primaryAssembly <- (if loadClosure.UseDesktopFramework then PrimaryAssembly.Mscorlib else PrimaryAssembly.System_Runtime)
                // Add one by one to remove duplicates
                dllReferences |> List.iter (fun dllReference ->
                    tcConfigB.AddReferencedAssemblyByPath(dllReference.Range, dllReference.Text))
                tcConfigB.knownUnresolvedReferences <- loadClosure.UnresolvedReferences
            | None -> ()

            let tcConfig = TcConfig.Create(tcConfigB, validate=true)
            let niceNameGen = NiceNameGenerator()
            let outfile, _, assemblyName = tcConfigB.DecideNames sourceFiles

            // Resolve assemblies and create the framework TcImports. This is done when constructing the
            // builder itself, rather than as an incremental task. This caches a level of "system" references. No type providers are
            // included in these references.
            let! (tcGlobals, frameworkTcImports, nonFrameworkResolutions, unresolvedReferences) = frameworkTcImportsCache.Get(ctok, tcConfig)

            // Note we are not calling errorLogger.GetDiagnostics() anywhere for this task.
            // This is ok because not much can actually go wrong here.
            let errorOptions = tcConfig.errorSeverityOptions
            let errorLogger = CompilationErrorLogger("nonFrameworkAssemblyInputs", errorOptions)
            // Return the disposable object that cleans up
            use _holder = new CompilationGlobalsScope(errorLogger, BuildPhase.Parameter)

            // Get the names and time stamps of all the non-framework referenced assemblies, which will act
            // as inputs to one of the nodes in the build.
            //
            // This operation is done when constructing the builder itself, rather than as an incremental task.
            let nonFrameworkAssemblyInputs =
                // Note we are not calling errorLogger.GetDiagnostics() anywhere for this task.
                // This is ok because not much can actually go wrong here.
                let errorLogger = CompilationErrorLogger("nonFrameworkAssemblyInputs", errorOptions)
                // Return the disposable object that cleans up
                use _holder = new CompilationGlobalsScope(errorLogger, BuildPhase.Parameter)

                [ for r in nonFrameworkResolutions do
                    let fileName = r.resolvedPath
                    yield (Choice1Of2 fileName, (fun (cache: TimeStampCache) -> cache.GetFileTimeStamp fileName))

                  for pr in projectReferences  do
                    yield Choice2Of2 pr, (fun (cache: TimeStampCache) -> cache.GetProjectReferenceTimeStamp (pr)) ]

            //
            //
            //
            //
            // Start importing

            let tcConfigP = TcConfigProvider.Constant tcConfig
            let beforeFileChecked = new Event<string>()
            let fileChecked = new Event<string>()

#if !NO_EXTENSIONTYPING
            let importsInvalidatedByTypeProvider = new Event<unit>()
#endif

            // Check for the existence of loaded sources and prepend them to the sources list if present.
            let sourceFiles = tcConfig.GetAvailableLoadedSources() @ (sourceFiles |>List.map (fun s -> rangeStartup, s))

            // Mark up the source files with an indicator flag indicating if they are the last source file in the project
            let sourceFiles =
                let flags, isExe = tcConfig.ComputeCanContainEntryPoint(sourceFiles |> List.map snd)
                ((sourceFiles, flags) ||> List.map2 (fun (m, nm) flag -> (m, nm, (flag, isExe))))

            let basicDependencies =
                [ for (UnresolvedAssemblyReference(referenceText, _))  in unresolvedReferences do
                    // Exclude things that are definitely not a file name
                    if not(FileSystem.IsInvalidPathShim referenceText) then
                        let file = if FileSystem.IsPathRootedShim referenceText then referenceText else Path.Combine(projectDirectory, referenceText)
                        yield file

                  for r in nonFrameworkResolutions do
                        yield  r.resolvedPath  ]

            let allDependencies =
                [| yield! basicDependencies
                   for (_, f, _) in sourceFiles do
                        yield f |]

            // For scripts, the dependency provider is already available.
            // For projects create a fresh one for the project.
            let dependencyProvider =
                match dependencyProvider with
                | None -> new DependencyProvider()
                | Some dependencyProvider -> dependencyProvider

            let! initialBoundModel = 
                cancellable {
                  let errorLogger = CompilationErrorLogger("CombineImportedAssembliesTask", tcConfig.errorSeverityOptions)
                  // Return the disposable object that cleans up
                  use _holder = new CompilationGlobalsScope(errorLogger, BuildPhase.Parameter)

                  let! tcImports =
                    cancellable {
                      try
                          let! tcImports = TcImports.BuildNonFrameworkTcImports(ctok, tcConfigP, tcGlobals, frameworkTcImports, nonFrameworkResolutions, unresolvedReferences, dependencyProvider)
#if !NO_EXTENSIONTYPING
                          tcImports.GetCcusExcludingBase() |> Seq.iter (fun ccu ->
                              // When a CCU reports an invalidation, merge them together and just report a
                              // general "imports invalidated". This triggers a rebuild.
                              //
                              // We are explicit about what the handler closure captures to help reason about the
                              // lifetime of captured objects, especially in case the type provider instance gets leaked
                              // or keeps itself alive mistakenly, e.g. via some global state in the type provider instance.
                              //
                              // The handler only captures
                              //    1. a weak reference to the importsInvalidatedByTypeProvider event.
                              //
                              // The IncrementalBuilder holds the strong reference the importsInvalidated event.
                              //
                              // In the invalidation handler we use a weak reference to allow the IncrementalBuilder to
                              // be collected if, for some reason, a TP instance is not disposed or not GC'd.
                              let capturedImportsInvalidated = WeakReference<_>(importsInvalidatedByTypeProvider)
                              ccu.Deref.InvalidateEvent.Add(fun _ ->
                                  match capturedImportsInvalidated.TryGetTarget() with
                                  | true, tg -> tg.Trigger()
                                  | _ -> ()))
#endif
                          return tcImports
                      with e ->
                          System.Diagnostics.Debug.Assert(false, sprintf "Could not BuildAllReferencedDllTcImports %A" e)
                          errorLogger.Warning e
                          return frameworkTcImports
                    }

                  let tcInitial = GetInitialTcEnv (assemblyName, rangeStartup, tcConfig, tcImports, tcGlobals)
                  let tcState = GetInitialTcState (rangeStartup, assemblyName, tcConfig, tcGlobals, tcImports, niceNameGen, tcInitial)
                  let loadClosureErrors =
                     [ match loadClosureOpt with
                       | None -> ()
                       | Some loadClosure ->
                          for inp in loadClosure.Inputs do
                              yield! inp.MetaCommandDiagnostics ]

                  let initialErrors = Array.append (Array.ofList loadClosureErrors) (errorLogger.GetDiagnostics())
                  let tcInfo =
                      {
                        tcState=tcState
                        tcEnvAtEndOfFile=tcInitial
                        topAttribs=None
                        latestCcuSigForFile=None
                        tcErrorsRev = [ initialErrors ]
                        moduleNamesDict = Map.empty
                        tcDependencyFiles = basicDependencies
                        sigNameOpt = None
                      }
                  let tcInfoExtras =
                      {
                          tcResolutionsRev=[]
                          tcSymbolUsesRev=[]
                          tcOpenDeclarationsRev=[]
                          latestImplFile=None
                          itemKeyStore = None
                          semanticClassificationKeyStore = None
                      }
                  return
                      BoundModel.Create(
                          tcConfig,
                          tcGlobals,
                          tcImports,
                          keepAssemblyContents,
                          keepAllBackgroundResolutions,
                          keepAllBackgroundSymbolUses,
                          enableBackgroundItemKeyStoreAndSemanticClassification,
                          enablePartialTypeChecking,
                          beforeFileChecked,
                          fileChecked,
                          tcInfo,
                          async { return Some tcInfoExtras },
                          None) }

            let builder =
                new IncrementalBuilder(
                    initialBoundModel,
                    tcGlobals,
                    nonFrameworkAssemblyInputs,
                    tcConfig,
                    outfile,
                    assemblyName,
                    resourceManager,
                    sourceFiles,
                    enablePartialTypeChecking,
                    beforeFileChecked,
                    fileChecked,
#if !NO_EXTENSIONTYPING
                    importsInvalidatedByTypeProvider,
#endif
                    allDependencies)
            return Some builder
          with e ->
            errorRecoveryNoRange e
            return None
         }

        let diagnostics =
            match builderOpt with
            | Some builder ->
                let errorSeverityOptions = builder.TcConfig.errorSeverityOptions
                let errorLogger = CompilationErrorLogger("IncrementalBuilderCreation", errorSeverityOptions)
                delayedLogger.CommitDelayedDiagnostics errorLogger
                errorLogger.GetDiagnostics()
            | _ ->
                Array.ofList delayedLogger.Diagnostics
            |> Array.map (fun (d, severity) -> FSharpDiagnostic.CreateFromException(d, severity, range.Zero, suggestNamesForErrors))

        return builderOpt, diagnostics
      }
