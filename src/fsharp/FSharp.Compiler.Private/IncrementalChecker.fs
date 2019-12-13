module internal FSharp.Compiler.CodeAnalysis.IncrementalChecker

open System
open System.IO
open System.Threading
open System.Threading.Tasks
open System.Collections.Immutable
open System.Collections.Generic
open System.Collections.Concurrent
open Internal.Utilities.Collections
open System.Runtime.CompilerServices
open FSharp.Compiler
open FSharp.Compiler.AbstractIL.Internal.Library
open FSharp.Compiler.Ast
open FSharp.Compiler.CompileOps
open FSharp.Compiler.Driver
open FSharp.Compiler.Tast
open FSharp.Compiler.TcGlobals
open FSharp.Compiler.AbstractIL.ILBinaryReader
open FSharp.Compiler.ErrorLogger
open FSharp.Compiler.CompileOptions
open FSharp.Compiler.TypeChecker
open FSharp.Compiler.NameResolution
open Internal.Utilities
open FSharp.Compiler.AbstractIL
open FSharp.Compiler.AbstractIL.IL
open FSharp.Compiler.Tastops
open FSharp.Compiler.Text
open FSharp.Compiler.Range
open FSharp.Compiler.SourceCodeServices
open FSharp.Compiler.CodeAnalysis.SyntaxTree

[<AutoOpen>]
module internal ErrorLoggerExtensions =

    [<AbstractClass;Extension>]
    type CSharpStyleExtensions private () =

        [<Extension>]
        static member ToErrorInfos(errors: (PhasedDiagnostic * FSharpErrorSeverity) []) =
            errors
            |> Array.map (fun (error, severity) ->
                let isError = match severity with FSharpErrorSeverity.Error -> true | _ -> false
                FSharpErrorInfo.CreateFromException (error, isError, range0, suggestNames = false)
            )

    type CompilationErrorLogger with

        member this.GetErrorInfos () =
            this.GetErrors().ToErrorInfos ()

/// Accumulated results of type checking.
[<NoEquality; NoComparison>]
type internal TcAccumulator =
    { tcState: TcState
      tcEnvAtEndOfFile: TcEnv

      topAttribs: TopAttribs option

      /// Result of checking most recent file, if any
      latestImplFile: TypedImplFile option

      latestCcuSigForFile: ModuleOrNamespaceType option

      /// Disambiguation table for module names
      tcModuleNamesDict: ModuleNamesDict

      /// Accumulated errors, last file first
      tcErrorsRev: FSharp.Compiler.SourceCodeServices.FSharpErrorInfo [] list }

[<RequireQualifiedAccess>]
module TcAccumulator =

    let rangeStartup = FSharp.Compiler.Range.rangeN "startup" 1

    let create assemblyName (tcConfig: TcConfig) tcGlobals tcImports niceNameGen (loadClosureOpt: LoadClosure option) : TcAccumulator =
        let errorLogger = CompilationErrorLogger("TcAccumulator.create", tcConfig.errorSeverityOptions)
        use _holder = new CompilationGlobalsScope(errorLogger, BuildPhase.Parameter)

        let tcInitial = GetInitialTcEnv (assemblyName, rangeStartup, tcConfig, tcImports, tcGlobals)
        let tcState = GetInitialTcState (rangeStartup, assemblyName, tcConfig, tcGlobals, tcImports, niceNameGen, tcInitial)
        let loadClosureErrors = 
           [| match loadClosureOpt with 
              | None -> ()
              | Some loadClosure -> 
                for inp in loadClosure.Inputs do
                    for (err, isError) in inp.MetaCommandDiagnostics do 
                        yield err, (if isError then FSharpErrorSeverity.Error else FSharpErrorSeverity.Warning) |]

        let initialErrors = Array.append (loadClosureErrors.ToErrorInfos ()) (errorLogger.GetErrorInfos ())

        { tcState=tcState
          tcEnvAtEndOfFile = tcInitial
          topAttribs = None
          latestImplFile = None
          latestCcuSigForFile = None
          tcErrorsRev = [ initialErrors ] 
          tcModuleNamesDict = Map.empty }

type internal CheckerOptions =
    {
        keepAssemblyContents: bool
        keepAllBackgroundResolutions: bool
    }

[<NoEquality;NoComparison>]
type PreEmitState =
    {
        tcErrors: FSharpErrorInfo []
        tcEnvAtEndOfLastFile: TcEnv
        topAttribs: TopAttribs
        implFiles: TypedImplFile list
        tcState: TcState
    }

type PartialCheckResult =
    | Parsed of FSharpSyntaxTree
    /// Is an impl file, but only checked its signature file (.fsi). This is a performance optimization.
   // | SignatureChecked of SyntaxTree * TcAccumulator
    | Checked of FSharpSyntaxTree * TcAccumulator

    member this.SyntaxTree =
        match this with
        | PartialCheckResult.Parsed syntaxTree -> syntaxTree
        | PartialCheckResult.Checked (syntaxTree, _) -> syntaxTree

[<NoEquality;NoComparison>]
type IncrementalCheckerState =
    {
        tcConfig: TcConfig
        tcGlobals: TcGlobals
        tcImports: TcImports
        initialTcAcc: TcAccumulator
        options: CheckerOptions
        /// Mutable item, used for caching results. Gets copied when new state gets built.
        orderedResults: PartialCheckResult []
        indexLookup: ImmutableDictionary<string, int>
    }

    static member Create (tcConfig, tcGlobals, tcImports, initialTcAcc, options, orderedSources: ImmutableArray<FSharpSyntaxTree>) =
        let length = orderedSources.Length

        let orderedResultsBuilder = ImmutableArray.CreateBuilder length
        let indexLookup = Array.zeroCreate length

        orderedResultsBuilder.Count <- length

        orderedSources
        |> Seq.iteri (fun i src ->
            orderedResultsBuilder.[i] <- Parsed src
            indexLookup.[i] <- KeyValuePair (src.FilePath, i)
        )

        {
            tcConfig = tcConfig
            tcGlobals = tcGlobals
            tcImports = tcImports
            initialTcAcc = initialTcAcc
            options = options
            orderedResults = orderedResultsBuilder.ToArray ()
            indexLookup = ImmutableDictionary.CreateRange indexLookup
        }

    member private this.GetIndex src =
        match this.indexLookup.TryGetValue src with
        | false, _ -> failwith "source does not exist in incremental checker"
        | true, index -> index

    member private this.GetPartialCheckResultByIndex index =
        this.orderedResults.[index]

    member private this.SetPartialCheckResultByIndex (index, result) =
        this.orderedResults.[index] <- result

    member private this.GetPriorTcAccumulatorAsync (filePath: string) =
        async {
            match this.GetIndex filePath with
            | 0 -> return (this.initialTcAcc, 0) // first file

            | cacheIndex ->
                let priorCacheResult = this.GetPartialCheckResultByIndex (cacheIndex - 1)
                match priorCacheResult with
                | PartialCheckResult.Parsed syntaxTree ->
                    // We set no checker flags as we don't want to ask for extra information when checking a dependent file.
                    let! tcAcc, _, _ = this.CheckAsync syntaxTree.FilePath
                    return (tcAcc, cacheIndex)
                | PartialCheckResult.Checked (_, tcAcc) ->
                    return (tcAcc, cacheIndex)
        }

    member this.ReplaceSyntaxTree (oldSrc, newSrc: FSharpSyntaxTree) =
        let index = this.GetIndex oldSrc
        let orderedResults =
            this.orderedResults
            |> Array.mapi (fun i result ->
                // invalidate compilation results of the source and all sources below it.
                if i = index then
                    PartialCheckResult.Parsed newSrc
                elif i > index then
                    PartialCheckResult.Parsed result.SyntaxTree
                else
                    result
            )
        { this with orderedResults = orderedResults }

    member this.GetSyntaxTree filePath =
        match this.indexLookup.TryGetValue filePath with
        | true, i -> this.orderedResults.[i].SyntaxTree
        | _ -> failwith "file for syntax tree does not exist in incremental checker"

    member this.CheckAsync (filePath: string) : Async<(TcAccumulator * TcResultsSinkImpl * SymbolEnv)> =
        let tcConfig = this.tcConfig
        let tcGlobals = this.tcGlobals
        let tcImports = this.tcImports
        let options = this.options

        async {
            let! ct = Async.CancellationToken

            let! (tcAcc, cacheIndex) = this.GetPriorTcAccumulatorAsync filePath
            let syntaxTree = (this.GetPartialCheckResultByIndex cacheIndex).SyntaxTree

            let capturingErrorLogger = CompilationErrorLogger("CheckAsync", tcConfig.errorSeverityOptions)
            let errorLogger = GetErrorLoggerFilteringByScopedPragmas(false, GetScopedPragmasForInput syntaxTree.ParsedInput, capturingErrorLogger)

            let fullComputation = 
                eventually {                 
                    ApplyMetaCommandsFromInputToTcConfig (tcConfig, syntaxTree.ParsedInput, Path.GetDirectoryName filePath) |> ignore

                    let sink = TcResultsSinkImpl tcGlobals

                    let input, moduleNamesDict = DeduplicateParsedInputModuleName tcAcc.tcModuleNamesDict syntaxTree.ParsedInput

                    let! (tcEnvAtEndOfFile, topAttribs, implFile, ccuSigForFile), tcState = 
                        TypeCheckOneInputEventually 
                            (   (fun () -> true), 
                                tcConfig, tcImports, 
                                tcGlobals, 
                                None, 
                                TcResultsSink.WithSink sink,
                                (fun _ _ -> ()),
                                tcAcc.tcState, input)
                
                    /// Only keep the typed interface files when doing a "full" build for fsc.exe, otherwise just throw them away
                    let implFile = if options.keepAssemblyContents then implFile else None
                    let tcEnvAtEndOfFile = (if options.keepAllBackgroundResolutions then tcEnvAtEndOfFile else tcState.TcEnvFromImpls)

                    let symbolEnv = SymbolEnv (tcGlobals, tcState.Ccu, Some ccuSigForFile, tcImports)
                                
                    let newErrors = capturingErrorLogger.GetErrorInfos ()

                    return {tcAcc with  tcState=tcState 
                                        tcEnvAtEndOfFile=tcEnvAtEndOfFile
                                        topAttribs=Some topAttribs
                                        latestImplFile=implFile
                                        latestCcuSigForFile=Some ccuSigForFile
                                        tcErrorsRev = newErrors :: tcAcc.tcErrorsRev 
                                        tcModuleNamesDict = moduleNamesDict }, sink, symbolEnv
                }

            // No one has ever changed this value, although a bit arbitrary.
            // At some point the `eventually { ... }` constructs will go away once we have a thread safe compiler.
            let maxTimeShareMilliseconds = 100L
            // Run part of the Eventually<_> computation until a timeout is reached. If not complete, 
            // return a new Eventually<_> computation which recursively runs more of the computation.
            //   - When the whole thing is finished commit the error results sent through the errorLogger.
            //   - Each time we do real work we reinstall the CompilationGlobalsScope
            let timeSlicedComputation = 
                    fullComputation |> 
                        Eventually.repeatedlyProgressUntilDoneOrTimeShareOverOrCanceled 
                            maxTimeShareMilliseconds
                            ct
                            (fun ctok f -> f ctok)

            let asyncTimeSlicedComputation =
                timeSlicedComputation
                |> Eventually.forceAsync (fun eventuallyWork ->
                    Reactor.Singleton.EnqueueAndAwaitOpAsync("IncrementalChecker", "CheckAsync", String.Empty, fun ctok ->
                        // Reinstall the compilation globals each time we start or restart
                        use _unwind = new CompilationGlobalsScope (errorLogger, BuildPhase.TypeCheck) 
                        Cancellable.ret (eventuallyWork ctok)
                    )
                )
                    
            match! asyncTimeSlicedComputation with
            | Some (tcAcc, sink, senv) ->
                this.SetPartialCheckResultByIndex (cacheIndex, PartialCheckResult.Checked (syntaxTree, tcAcc))
                return (tcAcc, sink, senv)
            | _ -> return raise (OperationCanceledException ())
        }

    member this.SpeculativeCheckAsync (src, tcState: TcState, synExpr: SynExpr) =
        let tcConfig = this.tcConfig
        let tcGlobals = this.tcGlobals
        let tcImports = this.tcImports

        async {
            let syntaxTree = this.GetSyntaxTree src
            let fullComputation =           
                Reactor.Singleton.EnqueueAndAwaitOpAsync("IncrementalChecker", "SpeculativeCheckAsync", String.Empty, fun ctok ->                        
                    ApplyMetaCommandsFromInputToTcConfig (tcConfig, syntaxTree.ParsedInput, Path.GetDirectoryName src) |> ignore

                    let sink = TcResultsSinkImpl tcGlobals
                    TryTypeCheckOneInputSynExpr (ctok, tcConfig, tcImports, tcGlobals, TcResultsSink.WithSink sink, tcState, syntaxTree.ParsedInput, synExpr)
                    |> Option.map (fun ty ->
                        (ty, sink)
                    )
                    |> Cancellable.ret
                )

            return! fullComputation
        }

let getTcAccs state =
    state.orderedResults 
    |> Array.map (function
        | PartialCheckResult.Checked (_, tcAcc) -> tcAcc
        | _ -> failwith "should not happen, missing a checked file"
    )

let getPreEmitState state =
    let tcAccs = getTcAccs state

    // Get the state at the end of the type-checking of the last file
    let finalAcc = tcAccs.[tcAccs.Length-1]

    // Finish the checking
    let (tcEnvAtEndOfLastFile, topAttrs, mimpls, _), tcState = 
        let results = tcAccs |> List.ofArray |> List.map (fun acc-> acc.tcEnvAtEndOfFile, defaultArg acc.topAttribs EmptyTopAttrs, acc.latestImplFile, acc.latestCcuSigForFile)
        TypeCheckMultipleInputsFinish (results, finalAcc.tcState)

    let tcState, mimpls = TypeCheckClosedInputSetFinish (mimpls, tcState)

    {
        tcErrors = finalAcc.tcErrorsRev |> List.last
        tcEnvAtEndOfLastFile = tcEnvAtEndOfLastFile
        topAttribs = topAttrs
        implFiles = mimpls
        tcState = tcState
    }

[<Sealed>]
type IncrementalChecker (state: IncrementalCheckerState) =

    member __.ReplaceSource (oldSrc, newSrc) =
        IncrementalChecker (state.ReplaceSyntaxTree (oldSrc, newSrc))

    member __.CheckAsync src =
        state.CheckAsync src

    member __.SpeculativeCheckAsync (src, tcState, synExpr) =
        state.SpeculativeCheckAsync (src, tcState, synExpr)

    member __.GetSyntaxTree src =
        state.GetSyntaxTree src

    member __.TcConfig = state.tcConfig

    member __.TcGlobals = state.tcGlobals

    member __.TcImports = state.tcImports

    member this.FinishAsync () =
        match state.orderedResults.[state.orderedResults.Length - 1] with
        | PartialCheckResult.Checked _ ->
            async { return getPreEmitState state }
        | result -> 
            async {
                let! _ = this.CheckAsync result.SyntaxTree.FilePath
                return getPreEmitState state
            }

    static member Create(tcConfig, tcGlobals, tcImports, tcAcc, checkerOptions: CheckerOptions, srcs) =
        let state = IncrementalCheckerState.Create (tcConfig, tcGlobals, tcImports, tcAcc, checkerOptions, srcs)
        IncrementalChecker state