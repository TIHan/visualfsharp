// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

namespace FSharp.Compiler


open System
open System.Collections.Generic
open System.IO
open System.Threading
open FSharp.Compiler
open FSharp.Compiler.NameResolution
open FSharp.Compiler.Tastops
open FSharp.Compiler.Lib
open FSharp.Compiler.AbstractIL
open FSharp.Compiler.AbstractIL.IL
open FSharp.Compiler.AbstractIL.ILBinaryReader
open FSharp.Compiler.AbstractIL.Internal.Library 
open FSharp.Compiler.CompileOps
open FSharp.Compiler.CompileOptions
open FSharp.Compiler.ErrorLogger
open FSharp.Compiler.TcGlobals
open FSharp.Compiler.TypeChecker
open FSharp.Compiler.Tast 
open FSharp.Compiler.Range
open FSharp.Compiler.SourceCodeServices
open Internal.Utilities.Collections

[<AutoOpen>]
module internal IncrementalBuild =

    /// A particular node in the Expr language. Use an int for keys instead of the entire Expr to avoid extra hashing.
    type Id = Id of int
            
    [<NoEquality; NoComparison>]
    /// A build rule representing a single output
    type ScalarBuildRule = 
        /// ScalarInput (uniqueRuleId, outputName)
        ///
        /// A build rule representing a single input, producing the input as its single scalar result
        | ScalarInput of Id * string

        /// ScalarDemultiplex (uniqueRuleId, outputName, input, taskFunction)
        ///
        /// A build rule representing the merge of a set of inputs to a single output
        | ScalarDemultiplex of Id * string * VectorBuildRule * (CompilationThreadToken -> obj[] -> Cancellable<obj>)

        /// ScalarMap (uniqueRuleId, outputName, input, taskFunction)
        ///
        /// A build rule representing the transformation of a single input to a single output
        /// THIS CASE IS CURRENTLY UNUSED
        | ScalarMap of Id * string * ScalarBuildRule * (CompilationThreadToken -> obj -> obj)

        /// Get the Id for the given ScalarBuildRule.
        member  x.Id = 
            match x with
            | ScalarInput(id, _) -> id
            | ScalarDemultiplex(id, _, _, _) -> id
            | ScalarMap(id, _, _, _) -> id

        /// Get the Name for the givenScalarExpr.
        member x.Name = 
            match x with 
            | ScalarInput(_, n) -> n                
            | ScalarDemultiplex(_, n, _, _) -> n
            | ScalarMap(_, n, _, _) -> n                

    /// A build rule with a vector of outputs
    and VectorBuildRule = 
        /// VectorInput (uniqueRuleId, outputName)
        ///
        /// A build rule representing the transformation of a single input to a single output
        | VectorInput of Id * string 

        /// VectorInput (uniqueRuleId, outputName, initialAccumulator, inputs, taskFunction)
        ///
        /// A build rule representing the scan-left combining a single scalar accumulator input with a vector of inputs
        | VectorScanLeft of Id * string * ScalarBuildRule * VectorBuildRule * (CompilationThreadToken -> obj -> obj->Eventually<obj>)

        /// VectorMap (uniqueRuleId, outputName, inputs, taskFunction)
        ///
        /// A build rule representing the parallel map of the inputs to outputs
        | VectorMap of Id * string * VectorBuildRule * (CompilationThreadToken -> obj -> obj) 

        /// VectorStamp (uniqueRuleId, outputName, inputs, stampFunction)
        ///
        /// A build rule representing pairing the inputs with a timestamp specified by the given function.  
        | VectorStamp of Id * string * VectorBuildRule * (TimeStampCache -> CompilationThreadToken -> obj -> DateTime)

        /// VectorMultiplex (uniqueRuleId, outputName, input, taskFunction)
        ///
        /// A build rule representing taking a single input and transforming it to a vector of outputs
        | VectorMultiplex of Id * string * ScalarBuildRule * (CompilationThreadToken -> obj -> obj[])

        /// Get the Id for the given VectorBuildRule.
        member x.Id = 
            match x with 
            | VectorInput(id, _) -> id
            | VectorScanLeft(id, _, _, _, _) -> id
            | VectorMap(id, _, _, _) -> id
            | VectorStamp (id, _, _, _) -> id
            | VectorMultiplex(id, _, _, _) -> id
        /// Get the Name for the given VectorBuildRule.
        member x.Name = 
            match x with 
            | VectorInput(_, n) -> n
            | VectorScanLeft(_, n, _, _, _) -> n
            | VectorMap(_, n, _, _) -> n
            | VectorStamp (_, n, _, _) -> n
            | VectorMultiplex(_, n, _, _) -> n
        
    [<NoEquality; NoComparison>]
    type BuildRuleExpr =
        | ScalarBuildRule of ScalarBuildRule
        | VectorBuildRule of VectorBuildRule      
        /// Get the Id for the given Expr.
        member x.Id = 
            match x with 
            | ScalarBuildRule se -> se.Id
            | VectorBuildRule ve -> ve.Id      
        /// Get the Name for the given Expr.
        member x.Name = 
            match x with 
            | ScalarBuildRule se -> se.Name
            | VectorBuildRule ve -> ve.Name    

    // Ids of exprs            
    let nextid = ref 999 // Number ids starting with 1000 to discern them
    let NextId() =
        nextid:=!nextid+1
        Id(!nextid)                    
        
    type INode = 
        abstract Name: string

    type IScalar = 
        inherit INode
        abstract Expr: ScalarBuildRule

    type IVector =
        inherit INode
        abstract Expr: VectorBuildRule
            
    type Scalar<'T> =  interface inherit IScalar  end

    type Vector<'T> = interface inherit IVector end
    
    /// The outputs of a build        
    [<NoEquality; NoComparison>]
    type NamedOutput = 
        | NamedVectorOutput of IVector
        | NamedScalarOutput of IScalar

    type BuildRules = { RuleList: (string * BuildRuleExpr) list }

    /// Visit each task and call op with the given accumulator.
    let FoldOverBuildRules(rules: BuildRules, op, acc)=
        let rec visitVector (ve: VectorBuildRule) acc = 
            match ve with
            | VectorInput _ -> op (VectorBuildRule ve) acc
            | VectorScanLeft(_, _, a, i, _) -> op (VectorBuildRule ve) (visitVector i (visitScalar a acc))
            | VectorMap(_, _, i, _)
            | VectorStamp (_, _, i, _) -> op (VectorBuildRule ve) (visitVector i acc)
            | VectorMultiplex(_, _, i, _) -> op (VectorBuildRule ve) (visitScalar i acc)

        and visitScalar (se: ScalarBuildRule) acc = 
            match se with
            | ScalarInput _ -> op (ScalarBuildRule se) acc
            | ScalarDemultiplex(_, _, i, _) -> op (ScalarBuildRule se) (visitVector i acc)
            | ScalarMap(_, _, i, _) -> op (ScalarBuildRule se) (visitScalar i acc)

        let visitRule (expr: BuildRuleExpr) acc =  
            match expr with
            | ScalarBuildRule se ->visitScalar se acc
            | VectorBuildRule ve ->visitVector ve acc

        List.foldBack visitRule (rules.RuleList |> List.map snd) acc            
    
    /// Convert from interfaces into discriminated union.
    let ToBuild (names: NamedOutput list): BuildRules = 

        // Create the rules.
        let createRules() = 
           { RuleList = names |> List.map (function NamedVectorOutput v -> v.Name, VectorBuildRule(v.Expr)
                                                  | NamedScalarOutput s -> s.Name, ScalarBuildRule(s.Expr)) }
        
        // Ensure that all names are unique.
        let ensureUniqueNames (expr: BuildRuleExpr) (acc: Map<string, Id>) = 
            let AddUniqueIdToNameMapping(id, name)=
                match acc.TryFind name with
                 | Some priorId -> 
                    if id<>priorId then failwith (sprintf "Two build expressions had the same name: %s" name)
                    else acc
                 | None-> Map.add name id acc
            let id = expr.Id
            let name = expr.Name
            AddUniqueIdToNameMapping(id, name)
        
        // Validate the rule tree
        let validateRules (rules: BuildRules) =
            FoldOverBuildRules(rules, ensureUniqueNames, Map.empty) |> ignore
        
        // Convert and validate
        let rules = createRules()
        validateRules rules
        rules

    /// These describe the input conditions for a result. If conditions change then the result is invalid.
    type InputSignature =
        | SingleMappedVectorInput of InputSignature[]
        | EmptyTimeStampedInput of DateTime
        | BoundInputScalar // An external input into the build
        | BoundInputVector // An external input into the build
        | IndexedValueElement of DateTime
        | UnevaluatedInput

        /// Return true if the result is fully evaluated
        member is.IsEvaluated = 
            match is with
            | UnevaluatedInput -> false
            | SingleMappedVectorInput iss -> iss |> Array.forall (fun is -> is.IsEvaluated)
            | _ -> true
            
    
    /// A slot for holding a single result.
    type Result =
        | NotAvailable
        | InProgress of (CompilationThreadToken -> Eventually<obj>) * DateTime 
        | Available of obj * DateTime * InputSignature

        /// Get the available result. Throw an exception if not available.
        member x.GetAvailable() = match x with Available (o, _, _) ->o  | _ -> failwith "No available result"

        /// Get the time stamp if available. Otherwise MaxValue.        
        member x.Timestamp = match x with Available (_, ts, _) -> ts | InProgress(_, ts) -> ts | _ -> DateTime.MaxValue

        /// Get the time stamp if available. Otherwise MaxValue.        
        member x.InputSignature = match x with Available (_, _, signature) -> signature | _ -> UnevaluatedInput
        
        member x.ResultIsInProgress =  match x with | InProgress _ -> true | _ -> false
        member x.GetInProgressContinuation ctok =  match x with | InProgress (f, _) -> f ctok | _ -> failwith "not in progress"
        member x.TryGetAvailable() =  match x with | InProgress _ | NotAvailable -> None | Available (obj, dt, i) -> Some (obj, dt, i)

    /// An immutable sparse vector of results.                
    type ResultVector(size, zeroElementTimestamp, map) =
        let get slot = 
            match Map.tryFind slot map with
            | Some result ->result
            | None->NotAvailable                   
        let asList = lazy List.map (fun i->i, get i) [0..size-1]

        static member OfSize size = ResultVector(size, DateTime.MinValue, Map.empty)
        member rv.Size = size
        member rv.Get slot = get slot
        member rv.Resize newSize = 
            if size<>newSize then 
                ResultVector(newSize, zeroElementTimestamp, map |> Map.filter(fun s _ -> s < newSize))
            else rv

        member rv.Set(slot, value) = 
#if DEBUG
            if slot<0 then failwith "ResultVector slot less than zero"
            if slot>=size then failwith "ResultVector slot too big"
#endif
            ResultVector(size, zeroElementTimestamp, Map.add slot value map)

        member rv.MaxTimestamp() =
            let maximize (lastTimestamp: DateTime) (_, result: Result) =  max lastTimestamp result.Timestamp
            List.fold maximize zeroElementTimestamp (asList.Force())

        member rv.Signature() =
            let l = asList.Force()
            let l = l |> List.map (fun (_, result) -> result.InputSignature)
            SingleMappedVectorInput (l|>List.toArray)
                                  
        member rv.FoldLeft f s: 'a = List.fold f s (asList.Force())
                
    /// A result of performing build actions
    [<NoEquality; NoComparison>]
    type ResultSet =
        | ScalarResult of Result
        | VectorResult of ResultVector
                            
    /// Result of a particular action over the bound build tree
    [<NoEquality; NoComparison>]
    type ActionResult = 
        | IndexedResult of Id * int * (*slotcount*) int * Eventually<obj> * DateTime 
        | ScalarValuedResult of Id * obj * DateTime * InputSignature
        | VectorValuedResult of Id * obj[] * DateTime * InputSignature
        | ResizeResult of Id * (*slotcount*) int
        
        
    /// A pending action over the bound build tree
    [<NoEquality; NoComparison>]
    type Action = 
        | IndexedAction of Id * (*taskname*)string * int * (*slotcount*) int * DateTime * (CompilationThreadToken -> Eventually<obj>)
        | ScalarAction of Id * (*taskname*)string * DateTime * InputSignature * (CompilationThreadToken -> Cancellable<obj>)
        | VectorAction of Id * (*taskname*)string * DateTime * InputSignature *  (CompilationThreadToken -> Cancellable<obj[]>)
        | ResizeResultAction of Id * (*slotcount*) int 
        /// Execute one action and return a corresponding result.
        member action.Execute ctok = 
          cancellable {
            match action with
            | IndexedAction(id, _taskname, slot, slotcount, timestamp, func) -> let res = func ctok in return IndexedResult(id, slot, slotcount, res, timestamp)
            | ScalarAction(id, _taskname, timestamp, inputsig, func) -> let! res = func ctok in return ScalarValuedResult(id, res, timestamp, inputsig)
            | VectorAction(id, _taskname, timestamp, inputsig, func) -> let! res = func ctok in return VectorValuedResult(id, res, timestamp, inputsig)
            | ResizeResultAction(id, slotcount) -> return ResizeResult(id, slotcount)
           }
     
    /// A set of build rules and the corresponding, possibly partial, results from building.
    [<Sealed>]
    type PartialBuild(rules: BuildRules, results: Map<Id, ResultSet>) = 
        member bt.Rules = rules
        member bt.Results = results
   
    /// Given an expression, find the expected width.
    let rec GetVectorWidthByExpr(bt: PartialBuild, ve: VectorBuildRule) = 
        let id = ve.Id
        let KnownValue() = 
            match bt.Results.TryFind id with 
            | Some resultSet ->
                match resultSet with
                | VectorResult rv ->Some rv.Size
                | _ -> failwith "Expected vector to have vector result."
            | None-> None
        match ve with
        | VectorScanLeft(_, _, _, i, _)
        | VectorMap(_, _, i, _)
        | VectorStamp (_, _, i, _) ->
            match GetVectorWidthByExpr(bt, i) with
            | Some _ as r -> r
            | None -> KnownValue()
        | VectorInput _
        | VectorMultiplex _ -> KnownValue()
        
    /// Given an expression name, get the corresponding expression.    
    let GetTopLevelExprByName(bt: PartialBuild, seek: string) =
        bt.Rules.RuleList |> List.filter(fun(name, _) ->name=seek) |> List.map (fun(_, root) ->root) |> List.head
    
    /// Get an expression matching the given name.
    let GetExprByName(bt: PartialBuild, node: INode): BuildRuleExpr = 
        let matchName (expr: BuildRuleExpr) (acc: BuildRuleExpr option): BuildRuleExpr option =
            if expr.Name = node.Name then Some expr else acc
        let matchOption = FoldOverBuildRules(bt.Rules, matchName, None)
        Option.get matchOption

    // Given an Id, find the corresponding expression.
    let GetExprById(bt: PartialBuild, seek: Id): BuildRuleExpr= 
        let rec vectorExprOfId ve =
            match ve with
            | VectorInput(id, _) ->if seek=id then Some (VectorBuildRule ve) else None
            | VectorScanLeft(id, _, a, i, _) ->
                if seek=id then Some (VectorBuildRule ve) else
                    let result = scalarExprOfId a 
                    match result with Some _ -> result | None->vectorExprOfId i
            | VectorMap(id, _, i, _) ->if seek=id then Some (VectorBuildRule ve) else vectorExprOfId i
            | VectorStamp (id, _, i, _) ->if seek=id then Some (VectorBuildRule ve) else vectorExprOfId i
            | VectorMultiplex(id, _, i, _) ->if seek=id then Some (VectorBuildRule ve) else scalarExprOfId i

        and scalarExprOfId se =
            match se with
            | ScalarInput(id, _) ->if seek=id then Some (ScalarBuildRule se) else None
            | ScalarDemultiplex(id, _, i, _) ->if seek=id then Some (ScalarBuildRule se) else vectorExprOfId i
            | ScalarMap(id, _, i, _) ->if seek=id then Some (ScalarBuildRule se) else scalarExprOfId i

        let exprOfId(expr: BuildRuleExpr) = 
            match expr with
            | ScalarBuildRule se ->scalarExprOfId se
            | VectorBuildRule ve ->vectorExprOfId ve

        let exprs = bt.Rules.RuleList |> List.map (fun(_, root) ->exprOfId root) |> List.filter Option.isSome
        match exprs with
        | Some expr :: _ -> expr
        | _ -> failwith (sprintf "GetExprById did not find an expression for Id")

    let GetVectorWidthById (bt: PartialBuild) seek = 
        match GetExprById(bt, seek) with 
        | ScalarBuildRule _ ->failwith "Attempt to get width of scalar." 
        | VectorBuildRule ve -> Option.get (GetVectorWidthByExpr(bt, ve))

    let GetScalarExprResult (bt: PartialBuild, se: ScalarBuildRule) =
        match bt.Results.TryFind (se.Id) with 
        | Some resultSet ->
            match se, resultSet with
            | ScalarInput _, ScalarResult r
            | ScalarMap _, ScalarResult r
            | ScalarDemultiplex _, ScalarResult r ->r
            | _ ->failwith "GetScalarExprResult had no match"
        | None->NotAvailable

    let GetVectorExprResultVector (bt: PartialBuild, ve: VectorBuildRule) =
        match bt.Results.TryFind (ve.Id) with 
        | Some resultSet ->
            match ve, resultSet with
            | VectorScanLeft _, VectorResult rv
            | VectorMap _, VectorResult rv
            | VectorInput _, VectorResult rv
            | VectorStamp _, VectorResult rv
            | VectorMultiplex _, VectorResult rv -> Some rv
            | _ -> failwith "GetVectorExprResultVector had no match"
        | None->None

    let GetVectorExprResult (bt: PartialBuild, ve: VectorBuildRule, slot) =
        match bt.Results.TryFind ve.Id with 
        | Some resultSet ->
            match ve, resultSet with
            | VectorScanLeft _, VectorResult rv
            | VectorMap _, VectorResult rv
            | VectorInput _, VectorResult rv
            | VectorStamp _, VectorResult rv -> rv.Get slot
            | VectorMultiplex _, VectorResult rv -> rv.Get slot
            | _ -> failwith "GetVectorExprResult had no match" 
        | None->NotAvailable

    /// Get the maximum build stamp for an output.
    let MaxTimestamp(bt: PartialBuild, id) = 
        match bt.Results.TryFind id with
        | Some resultSet -> 
            match resultSet with 
            | ScalarResult rs -> rs.Timestamp
            | VectorResult rv -> rv.MaxTimestamp()
        | None -> DateTime.MaxValue
        
    let Signature(bt: PartialBuild, id) =
        match bt.Results.TryFind id with
        | Some resultSet -> 
            match resultSet with 
            | ScalarResult rs -> rs.InputSignature
            | VectorResult rv -> rv.Signature()
        | None -> UnevaluatedInput               
     
    /// Get all the results for the given expr.
    let AllResultsOfExpr extractor (bt: PartialBuild) (expr: VectorBuildRule) = 
        let GetAvailable (rv: ResultVector) = 
            let Extract acc (_, result) = (extractor result) :: acc
            List.rev (rv.FoldLeft Extract [])
        let GetVectorResultById id = 
            match bt.Results.TryFind id with
            | Some found ->
                match found with
                | VectorResult rv ->GetAvailable rv
                | _ -> failwith "wrong result type"
            | None -> []
            
        GetVectorResultById(expr.Id)


   
    [<RequireQualifiedAccess>]
    type BuildInput =
        | Vector of INode * obj list
        | Scalar of INode * obj

        /// Declare a named scalar output.
        static member ScalarInput (node: Scalar<'T>, value: 'T) = BuildInput.Scalar(node, box value)
        static member VectorInput(node: Vector<'T>, values: 'T list) = BuildInput.Vector(node, List.map box values)

        
    let AvailableAllResultsOfExpr bt expr = 
        let msg = "Expected all results to be available"
        AllResultsOfExpr (function Available (o, _, _) -> o | _ -> failwith msg) bt expr
        
    /// Bind a set of build rules to a set of input values.
    let ToBound(buildRules: BuildRules, inputs: BuildInput list) = 
        let now = DateTime.UtcNow
        let rec applyScalarExpr(se, results) =
            match se with
            | ScalarInput(id, n) -> 
                let matches = 
                   [ for input in inputs  do
                       match input with 
                       | BuildInput.Scalar (node, value) ->
                         if node.Name = n then 
                             yield ScalarResult(Available (value, now, BoundInputScalar))
                       | _ -> () ]
                List.foldBack (Map.add id) matches results
            | ScalarMap(_, _, se, _) ->applyScalarExpr(se, results)
            | ScalarDemultiplex(_, _, ve, _) ->ApplyVectorExpr(ve, results)
        and ApplyVectorExpr(ve, results) =
            match ve with
            | VectorInput(id, n) ->
                let matches = 
                   [ for input in inputs  do
                       match input with 
                       | BuildInput.Scalar _ -> ()
                       | BuildInput.Vector (node, values) ->
                         if node.Name = n then 
                            let results = values|>List.mapi(fun i value->i, Available (value, now, BoundInputVector))
                            yield VectorResult(ResultVector(values.Length, DateTime.MinValue, results|>Map.ofList)) ]
                List.foldBack (Map.add id) matches results
            | VectorScanLeft(_, _, a, i, _) ->ApplyVectorExpr(i, applyScalarExpr(a, results))
            | VectorMap(_, _, i, _)
            | VectorStamp (_, _, i, _) ->ApplyVectorExpr(i, results)
            | VectorMultiplex(_, _, i, _) ->applyScalarExpr(i, results)

        let applyExpr expr results =
            match expr with
            | ScalarBuildRule se ->applyScalarExpr(se, results)
            | VectorBuildRule ve ->ApplyVectorExpr(ve, results)
                                                                             
        // Place vector inputs into results map.
        let results = List.foldBack applyExpr (buildRules.RuleList |> List.map snd) Map.empty
        PartialBuild(buildRules, results)
        
    type Target = Target of INode * int option

    /// Visit each executable action necessary to evaluate the given output (with an optional slot in a
    /// vector output). Call actionFunc with the given accumulator.
    let ForeachAction cache ctok (Target(output, optSlot)) bt (actionFunc: Action -> 'T -> 'T) (acc:'T) =
        let seen = Dictionary<Id, bool>()
        let isSeen id = 
            if seen.ContainsKey id then true
            else 
                seen.[id] <- true
                false
                 
        let shouldEvaluate(bt, currentSig: InputSignature, id) =
            if currentSig.IsEvaluated then 
                currentSig <> Signature(bt, id)
            else false
            
        /// Make sure the result vector saved matches the size of expr
        let resizeVectorExpr(ve: VectorBuildRule, acc)  = 
            match GetVectorWidthByExpr(bt, ve) with
            | Some expectedWidth ->
                match bt.Results.TryFind ve.Id with
                | Some found ->
                    match found with
                    | VectorResult rv ->
                        if rv.Size <> expectedWidth then 
                            actionFunc (ResizeResultAction(ve.Id, expectedWidth)) acc
                        else acc
                    | _ -> acc
                | None -> acc        
            | None -> acc           
        
        let rec visitVector optSlot (ve: VectorBuildRule) acc =
        
            if isSeen ve.Id then acc
            else
                let acc = resizeVectorExpr(ve, acc)        
                match ve with
                | VectorInput _ -> acc
                | VectorScanLeft(id, taskname, accumulatorExpr, inputExpr, func) ->
                    let acc =
                        match GetVectorWidthByExpr(bt, ve) with
                        | Some cardinality ->                    
                            let limit = match optSlot with None -> cardinality | Some slot -> (slot+1)
                        
                            let Scan slot =
                                let accumulatorResult = 
                                    if slot=0 then GetScalarExprResult (bt, accumulatorExpr) 
                                    else GetVectorExprResult (bt, ve, slot-1)

                                let inputResult = GetVectorExprResult (bt, inputExpr, slot)
                                match accumulatorResult, inputResult with 
                                | Available (accumulator, accumulatorTimestamp, _accumulatorInputSig), Available (input, inputTimestamp, _inputSig) ->
                                    let inputTimestamp = max inputTimestamp accumulatorTimestamp
                                    let prevOutput = GetVectorExprResult (bt, ve, slot)
                                    let outputTimestamp = prevOutput.Timestamp
                                    let scanOpOpt = 
                                        if inputTimestamp <> outputTimestamp then
                                            Some (fun ctok -> func ctok accumulator input)
                                        elif prevOutput.ResultIsInProgress then
                                            Some prevOutput.GetInProgressContinuation
                                        else 
                                            // up-to-date and complete, no work required
                                            None
                                    match scanOpOpt with 
                                    | Some scanOp -> Some (actionFunc (IndexedAction(id, taskname, slot, cardinality, inputTimestamp, scanOp)) acc)
                                    | None -> None
                                | _ -> None                            
                                
                            match ([0..limit-1]|>List.tryPick Scan) with Some acc ->acc | None->acc
                        | None -> acc
                    
                    // Check each slot for an action that may be performed.
                    visitVector None inputExpr (visitScalar accumulatorExpr acc)

                | VectorMap(id, taskname, inputExpr, func) ->
                    let acc =
                        match GetVectorWidthByExpr(bt, ve) with
                        | Some cardinality ->       
                            if cardinality=0 then
                                // For vector length zero, just propagate the prior timestamp.
                                let inputTimestamp = MaxTimestamp(bt, inputExpr.Id)
                                let outputTimestamp = MaxTimestamp(bt, id)
                                if inputTimestamp <> outputTimestamp then
                                    actionFunc (VectorAction(id, taskname, inputTimestamp, EmptyTimeStampedInput inputTimestamp, fun _ -> cancellable.Return [||])) acc
                                else acc
                            else                                                
                                let MapResults acc slot =
                                    let inputTimestamp = GetVectorExprResult(bt, inputExpr, slot).Timestamp
                                    let outputTimestamp = GetVectorExprResult(bt, ve, slot).Timestamp
                                    if inputTimestamp <> outputTimestamp then
                                        let OneToOneOp ctok =
                                            Eventually.Done (func ctok (GetVectorExprResult(bt, inputExpr, slot).GetAvailable()))
                                        actionFunc (IndexedAction(id, taskname, slot, cardinality, inputTimestamp, OneToOneOp)) acc
                                    else acc
                                match optSlot with 
                                | None ->
                                    [0..cardinality-1] |> List.fold MapResults acc                         
                                | Some slot -> 
                                    MapResults acc slot
                        | None -> acc

                    visitVector optSlot inputExpr acc

                | VectorStamp (id, taskname, inputExpr, func) -> 
               
                    // For every result that is available, check time stamps.
                    let acc =
                        match GetVectorWidthByExpr(bt, ve) with
                        | Some cardinality ->    
                            if cardinality=0 then
                                // For vector length zero, just propagate the prior timestamp.
                                let inputTimestamp = MaxTimestamp(bt, inputExpr.Id)
                                let outputTimestamp = MaxTimestamp(bt, id)
                                if inputTimestamp <> outputTimestamp then
                                    actionFunc (VectorAction(id, taskname, inputTimestamp, EmptyTimeStampedInput inputTimestamp, fun _ -> cancellable.Return [||])) acc
                                else acc
                            else                 
                                let checkStamp acc slot = 
                                    let inputResult = GetVectorExprResult (bt, inputExpr, slot)
                                    match inputResult with
                                    | Available (ires, _, _) ->
                                        let oldTimestamp = GetVectorExprResult(bt, ve, slot).Timestamp
                                        let newTimestamp = func cache ctok ires
                                        if newTimestamp <> oldTimestamp then 
                                            actionFunc (IndexedAction(id, taskname, slot, cardinality, newTimestamp, fun _ -> Eventually.Done ires)) acc
                                        else acc
                                    | _ -> acc
                                match optSlot with 
                                | None ->
                                    [0..cardinality-1] |> List.fold checkStamp acc
                                | Some slot -> 
                                    checkStamp acc slot
                        | None -> acc
                    visitVector optSlot inputExpr acc

                | VectorMultiplex(id, taskname, inputExpr, func) -> 
                    let acc = 
                        match GetScalarExprResult (bt, inputExpr) with
                         | Available (inp, inputTimestamp, inputsig) ->
                           let outputTimestamp = MaxTimestamp(bt, id)
                           if inputTimestamp <> outputTimestamp then
                               let MultiplexOp ctok =  func ctok inp |> cancellable.Return
                               actionFunc (VectorAction(id, taskname, inputTimestamp, inputsig, MultiplexOp)) acc
                           else acc
                         | _ -> acc
                    visitScalar inputExpr acc

        and visitScalar (se: ScalarBuildRule) acc =
            if isSeen se.Id then acc
            else
                match se with
                | ScalarInput _ -> acc
                | ScalarDemultiplex (id, taskname, inputExpr, func) ->
                    let acc = 
                        match GetVectorExprResultVector (bt, inputExpr) with
                        | Some inputResult ->   
                            let currentSig = inputResult.Signature()
                            if shouldEvaluate(bt, currentSig, id) then
                                let inputTimestamp = MaxTimestamp(bt, inputExpr.Id)
                                let DemultiplexOp ctok = 
                                 cancellable {
                                    let input = AvailableAllResultsOfExpr bt inputExpr |> List.toArray
                                    return! func ctok input
                                 }
                                actionFunc (ScalarAction(id, taskname, inputTimestamp, currentSig, DemultiplexOp)) acc
                            else acc
                        | None -> acc

                    visitVector None inputExpr acc

                | ScalarMap (id, taskname, inputExpr, func) ->
                    let acc = 
                        match GetScalarExprResult (bt, inputExpr) with
                        | Available (inp, inputTimestamp, inputsig) ->
                           let outputTimestamp = MaxTimestamp(bt, id)
                           if inputTimestamp <> outputTimestamp then
                               let MapOp ctok = func ctok inp |> cancellable.Return
                               actionFunc (ScalarAction(id, taskname, inputTimestamp, inputsig, MapOp)) acc
                           else acc
                        | _ -> acc
                    
                    visitScalar inputExpr acc
                         
                    
        let expr = bt.Rules.RuleList |> List.find (fun (s, _) -> s = output.Name) |> snd
        match expr with
        | ScalarBuildRule se -> visitScalar se acc
        | VectorBuildRule ve -> visitVector optSlot ve acc                    

    let CollectActions cache target (bt: PartialBuild) =
        // Explanation: This is a false reuse of 'ForeachAction' where the ctok is unused, we are
        // just iterating to determine if there is work to do. This means this is safe to call from any thread.
        let ctok = AssumeCompilationThreadWithoutEvidence ()
        ForeachAction cache ctok target bt (fun a l -> a :: l) []
    
    /// Compute the max timestamp on all available inputs
    let ComputeMaxTimeStamp cache ctok output (bt: PartialBuild) acc =
        let expr = bt.Rules.RuleList |> List.find (fun (s, _) -> s = output) |> snd
        match expr with 
        | VectorBuildRule  (VectorStamp (_id, _taskname, inputExpr, func) as ve) -> 
                match GetVectorWidthByExpr(bt, ve) with
                | Some cardinality ->    
                    let CheckStamp acc slot = 
                        match GetVectorExprResult (bt, inputExpr, slot) with
                        | Available (ires, _, _) -> max acc (func cache ctok ires)
                        | _ -> acc
                    [0..cardinality-1] |> List.fold CheckStamp acc
                | None -> acc

        | _ -> failwith "expected a VectorStamp"

    /// Given the result of a single action, apply that action to the Build
    let ApplyResult(actionResult: ActionResult, bt: PartialBuild) = 
        match actionResult with 
        | ResizeResult(id, slotcount) ->
            match bt.Results.TryFind id with
            | Some resultSet ->
                match resultSet with 
                | VectorResult rv -> 
                    let rv = rv.Resize slotcount
                    let results = Map.add id (VectorResult rv) bt.Results
                    PartialBuild(bt.Rules, results)
                | _ -> failwith "Unexpected"                
            | None -> failwith "Unexpected"
        | ScalarValuedResult(id, value, timestamp, inputsig) ->
            PartialBuild(bt.Rules, Map.add id (ScalarResult(Available (value, timestamp, inputsig))) bt.Results)
        | VectorValuedResult(id, values, timestamp, inputsig) ->
            let Append acc slot = 
                Map.add slot (Available (values.[slot], timestamp, inputsig)) acc
            let results = [0..values.Length-1]|>List.fold Append Map.empty
            let results = VectorResult(ResultVector(values.Length, timestamp, results))
            let bt = PartialBuild(bt.Rules, Map.add id results bt.Results)
            bt
                
        | IndexedResult(id, index, slotcount, value, timestamp) ->
            let width = GetVectorWidthById bt id
            let priorResults = bt.Results.TryFind id 
            let prior =
                match priorResults with
                | Some prior ->prior
                | None->VectorResult(ResultVector.OfSize width)
            match prior with
            | VectorResult rv ->                                
                let result = 
                    match value with 
                    | Eventually.Done res -> 
                        Available (res, timestamp, IndexedValueElement timestamp)
                    | Eventually.NotYetDone f -> 
                        InProgress (f, timestamp)
                let results = rv.Resize(slotcount).Set(index, result)
                PartialBuild(bt.Rules, Map.add id (VectorResult results) bt.Results)
            | _ -> failwith "Unexpected"
        
    let mutable injectCancellationFault = false
    let LocallyInjectCancellationFault() = 
        injectCancellationFault <- true
        { new IDisposable with member __.Dispose() =  injectCancellationFault <- false }

    /// Apply the result, and call the 'save' function to update the build.  
    let ExecuteApply (ctok: CompilationThreadToken) save (action: Action) bt = 
      cancellable {
        let! actionResult = action.Execute ctok
        let newBt = ApplyResult(actionResult, bt)
        save ctok newBt
        return newBt
      }

    /// Evaluate the result of a single output
    let EvalLeafsFirst cache ctok save target bt =

        let rec eval(bt, gen) =
          cancellable {
            #if DEBUG
            // This can happen, for example, if there is a task whose timestamp never stops increasing.
            // Possibly could detect this case directly.
            if gen>5000 then failwith "Infinite loop in incremental builder?"
            #endif

            let workList = CollectActions cache target bt 
            
            let! newBt = 
              (bt, workList) ||> Cancellable.fold (fun bt action -> 
                     if injectCancellationFault then 
                         Cancellable.canceled() 
                     else 
                         ExecuteApply ctok save action bt)

            if newBt=bt then return bt else return! eval(newBt, gen+1)
          }
        eval(bt, 0)
        
    /// Evaluate one step of the build.  Call the 'save' function to save the intermediate result.
    let Step cache ctok save target (bt: PartialBuild) = 
      cancellable {
        // REVIEW: we're building up the whole list of actions on the fringe of the work tree, 
        // executing one thing and then throwing the list away. What about saving the list inside the Build instance?
        let workList = CollectActions cache target bt 
            
        match workList with 
        | action :: _ -> 
            let! res = ExecuteApply ctok save action bt
            return Some res
        | _ -> 
            return None
      }
            
    /// Evaluate an output of the build.
    ///
    /// Intermediate progress along the way may be saved through the use of the 'save' function.
    let Eval cache ctok save node bt = EvalLeafsFirst cache ctok save (Target(node, None)) bt

    /// Evaluate an output of the build.
    ///
    /// Intermediate progress along the way may be saved through the use of the 'save' function.
    let EvalUpTo cache ctok save (node, n) bt = EvalLeafsFirst cache ctok save (Target(node, Some n)) bt

    /// Check if an output is up-to-date and ready
    let IsReady cache target bt = 
        let workList = CollectActions cache target bt 
        workList.IsEmpty
        
    /// Check if an output is up-to-date and ready
    let MaxTimeStampInDependencies cache ctok target bt = 
        ComputeMaxTimeStamp cache ctok target bt DateTime.MinValue 

    /// Get a scalar vector. Result must be available
    let GetScalarResult<'T>(node: Scalar<'T>, bt): ('T*DateTime) option = 
        match GetTopLevelExprByName(bt, node.Name) with 
        | ScalarBuildRule se ->
            match bt.Results.TryFind se.Id with
            | Some result ->
                match result with 
                | ScalarResult sr ->
                    match sr.TryGetAvailable() with                     
                    | Some (r, timestamp, _) -> Some (downcast r, timestamp)
                    | None -> None
                | _ ->failwith "Expected a scalar result."
            | None->None
        | VectorBuildRule _ -> failwith "Expected scalar."
    
    /// Get a result vector. All results must be available or thrown an exception.
    let GetVectorResult<'T>(node: Vector<'T>, bt): 'T[] = 
        match GetTopLevelExprByName(bt, node.Name) with 
        | ScalarBuildRule _ -> failwith "Expected vector."
        | VectorBuildRule ve -> AvailableAllResultsOfExpr bt ve |> List.map unbox |> Array.ofList
        
    /// Get an element of vector result or None if there were no results.
    let GetVectorResultBySlot<'T>(node: Vector<'T>, slot, bt): ('T*DateTime) option = 
        match GetTopLevelExprByName(bt, node.Name) with 
        | ScalarBuildRule _ -> failwith "Expected vector expression"
        | VectorBuildRule ve ->
            match GetVectorExprResult(bt, ve, slot).TryGetAvailable() with
            | Some (o, timestamp, _) -> Some (downcast o, timestamp)
            | None->None

    /// Given an input value, find the corresponding slot.        
    let TryGetSlotByInput<'T>(node: Vector<'T>, build: PartialBuild, found:'T->bool): int option = 
        let expr = GetExprByName(build, node)
        let id = expr.Id
        match build.Results.TryFind id with 
        | None -> None
        | Some resultSet ->
        match resultSet with 
        | VectorResult rv ->
            let MatchNames acc (slot, result) = 
                match result with
                | Available (o, _, _) ->
                    let o = o :?> 'T
                    if found o then Some slot else acc
                | _ -> acc
            let slotOption = rv.FoldLeft MatchNames None
            slotOption 
            // failwith (sprintf "Could not find requested input '%A' named '%s' in set %+A" input name rv)
        | _ -> None // failwith (sprintf "Could not find requested input: %A" input)

    
    // Redeclare functions in the incremental build scope-----------------------------------------------------------------------

    // Methods for declaring inputs and outputs            

    /// Declares a vector build input.
    let InputVector<'T> name = 
        let expr = VectorInput(NextId(), name) 
        { new Vector<'T>
          interface IVector with
               override __.Name = name
               override pe.Expr = expr }

    /// Declares a scalar build input.
    let InputScalar<'T> name = 
        let expr = ScalarInput(NextId(), name)
        { new Scalar<'T>
          interface IScalar with
               override __.Name = name
               override pe.Expr = expr }
    
            
    module Vector =
        /// Maps one vector to another using the given function.    
        let Map (taskname: string) (task: CompilationThreadToken -> 'I -> 'O) (input: Vector<'I>): Vector<'O> = 
            let input = input.Expr
            let expr = VectorMap(NextId(), taskname, input, (fun ctok x -> box (task ctok (unbox x))))
            { new Vector<'O>
              interface IVector with
                   override __.Name = taskname
                   override pe.Expr = expr }            
            
        
        /// Apply a function to each element of the vector, threading an accumulator argument
        /// through the computation. Returns intermediate results in a vector.
        let ScanLeft (taskname: string) (task: CompilationThreadToken -> 'A -> 'I -> Eventually<'A>) (acc: Scalar<'A>) (input: Vector<'I>): Vector<'A> =
            let BoxingScanLeft ctok a i = Eventually.box(task ctok (unbox a) (unbox i))
            let acc = acc.Expr
            let input = input.Expr
            let expr = VectorScanLeft(NextId(), taskname, acc, input, BoxingScanLeft) 
            { new Vector<'A>
              interface IVector with
                   override __.Name = taskname
                   override pe.Expr = expr }    
            
        /// Apply a function to a vector to get a scalar value.
        let Demultiplex (taskname: string) (task: CompilationThreadToken -> 'I[] -> Cancellable<'O>) (input: Vector<'I>): Scalar<'O> =
            let BoxingDemultiplex ctok inps =
                cancellable { 
                  let! res = task ctok (Array.map unbox inps)
                  return box res
                }
            let input = input.Expr
            let expr = ScalarDemultiplex(NextId(), taskname, input, BoxingDemultiplex)
            { new Scalar<'O>
              interface IScalar with
                   override __.Name = taskname
                   override pe.Expr = expr }                
            
        /// Creates a new vector with the same items but with 
        /// timestamp specified by the passed-in function.  
        let Stamp (taskname: string) (task: TimeStampCache -> CompilationThreadToken -> 'I -> DateTime) (input: Vector<'I>): Vector<'I> =
            let input = input.Expr
            let expr = VectorStamp (NextId(), taskname, input, (fun cache ctok x -> task cache ctok (unbox x)))
            { new Vector<'I>
              interface IVector with
                   override __.Name = taskname
                   override pe.Expr = expr }    

        let AsScalar (taskname: string) (input: Vector<'I>): Scalar<'I array> = 
            Demultiplex taskname (fun _ctok x -> cancellable.Return x) input
                  
    let VectorInput(node: Vector<'T>, values: 'T list) = (node.Name, values.Length, List.map box values)
    
    /// Declare build outputs and bind them to real values.
    type BuildDescriptionScope() =
        let mutable outputs = []

        /// Declare a named scalar output.
        member b.DeclareScalarOutput(output: Scalar<'T>)=
            outputs <- NamedScalarOutput output :: outputs

        /// Declare a named vector output.
        member b.DeclareVectorOutput(output: Vector<'T>)=
            outputs <- NamedVectorOutput output :: outputs

        /// Set the concrete inputs for this build
        member b.GetInitialPartialBuild(inputs: BuildInput list) =
            ToBound(ToBuild outputs, inputs)   


        

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

module Tc = FSharp.Compiler.TypeChecker


/// Accumulated results of type checking.
[<NoEquality; NoComparison>]
type TypeCheckAccumulator =
    { tcState: TcState
      tcImports: TcImports
      tcGlobals: TcGlobals
      tcConfig: TcConfig
      tcEnvAtEndOfFile: TcEnv

      /// Accumulated resolutions, last file first
      tcResolutionsRev: TcResolutions list

      /// Accumulated symbol uses, last file first
      tcSymbolUsesRev: TcSymbolUses list

      /// Accumulated 'open' declarations, last file first
      tcOpenDeclarationsRev: OpenDeclaration[] list

      topAttribs: TopAttribs option

      /// Result of checking most recent file, if any
      latestImplFile: TypedImplFile option

      latestCcuSigForFile: ModuleOrNamespaceType option

      tcDependencyFiles: string list

      /// Disambiguation table for module names
      tcModuleNamesDict: ModuleNamesDict

      /// Accumulated errors, last file first
      tcErrorsRev:(PhasedDiagnostic * FSharpErrorSeverity)[] list }

      
/// Global service state
type FrameworkImportsCacheKey = (*resolvedpath*)string list * string * (*TargetFrameworkDirectories*)string list * (*fsharpBinaries*)string * (*langVersion*)decimal

/// Represents a cache of 'framework' references that can be shared between multiple incremental builds
type FrameworkImportsCache(keepStrongly) = 

    // Mutable collection protected via CompilationThreadToken 
    let frameworkTcImportsCache = AgedLookup<CompilationThreadToken, FrameworkImportsCacheKey, (TcGlobals * TcImports)>(keepStrongly, areSimilar=(fun (x, y) -> x = y)) 

    /// Reduce the size of the cache in low-memory scenarios
    member __.Downsize ctok = frameworkTcImportsCache.Resize(ctok, keepStrongly=0)

    /// Clear the cache
    member __.Clear ctok = frameworkTcImportsCache.Clear ctok

    /// This function strips the "System" assemblies from the tcConfig and returns a age-cached TcImports for them.
    member __.Get(ctok, tcConfig: TcConfig) =
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


//------------------------------------------------------------------------------------
// Rules for reactive building.
//
// This phrases the compile as a series of vector functions and vector manipulations.
// Rules written in this language are then transformed into a plan to execute the 
// various steps of the process.
//-----------------------------------------------------------------------------------


/// Represents the interim state of checking an assembly
type PartialCheckResults = 
    { TcState: TcState 
      TcImports: TcImports 
      TcGlobals: TcGlobals 
      TcConfig: TcConfig 
      TcEnvAtEnd: TcEnv 

      /// Kept in a stack so that each incremental update shares storage with previous files
      TcErrorsRev: (PhasedDiagnostic * FSharpErrorSeverity)[] list 

      /// Kept in a stack so that each incremental update shares storage with previous files
      TcResolutionsRev: TcResolutions list 

      /// Kept in a stack so that each incremental update shares storage with previous files
      TcSymbolUsesRev: TcSymbolUses list 

      /// Kept in a stack so that each incremental update shares storage with previous files
      TcOpenDeclarationsRev: OpenDeclaration[] list

      /// Disambiguation table for module names
      ModuleNamesDict: ModuleNamesDict

      TcDependencyFiles: string list 

      TopAttribs: TopAttribs option

      TimeStamp: DateTime

      LatestImplementationFile: TypedImplFile option 

      LatestCcuSigForFile: ModuleOrNamespaceType option }

    member x.TcErrors  = Array.concat (List.rev x.TcErrorsRev)
    member x.TcSymbolUses  = List.rev x.TcSymbolUsesRev

    static member Create (tcAcc: TypeCheckAccumulator, timestamp) = 
        { TcState = tcAcc.tcState
          TcImports = tcAcc.tcImports
          TcGlobals = tcAcc.tcGlobals
          TcConfig = tcAcc.tcConfig
          TcEnvAtEnd = tcAcc.tcEnvAtEndOfFile
          TcErrorsRev = tcAcc.tcErrorsRev
          TcResolutionsRev = tcAcc.tcResolutionsRev
          TcSymbolUsesRev = tcAcc.tcSymbolUsesRev
          TcOpenDeclarationsRev = tcAcc.tcOpenDeclarationsRev
          TcDependencyFiles = tcAcc.tcDependencyFiles
          TopAttribs = tcAcc.topAttribs
          ModuleNamesDict = tcAcc.tcModuleNamesDict
          TimeStamp = timestamp 
          LatestImplementationFile = tcAcc.latestImplFile 
          LatestCcuSigForFile = tcAcc.latestCcuSigForFile }


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
        let _sigDataAttributes, sigDataResources = Driver.EncodeInterfaceData(tcConfig, tcGlobals, exportRemapping, generatedCcu, outfile, true)
        [ for r in sigDataResources  do
            let ccuName = GetSignatureDataResourceName r
            yield (ccuName, (fun () -> r.GetBytes())) ]

    let autoOpenAttrs = topAttrs.assemblyAttrs |> List.choose (List.singleton >> TryFindFSharpStringAttribute tcGlobals tcGlobals.attrib_AutoOpenAttribute)

    let ivtAttrs = topAttrs.assemblyAttrs |> List.choose (List.singleton >> TryFindFSharpStringAttribute tcGlobals tcGlobals.attrib_InternalsVisibleToAttribute)

    interface IRawFSharpAssemblyData with 
        member __.GetAutoOpenAttributes(_ilg) = autoOpenAttrs
        member __.GetInternalsVisibleToAttributes(_ilg) =  ivtAttrs
        member __.TryGetILModuleDef() = None
        member __.GetRawFSharpSignatureData(_m, _ilShortAssemName, _filename) = sigData
        member __.GetRawFSharpOptimizationData(_m, _ilShortAssemName, _filename) = [ ]
        member __.GetRawTypeForwarders() = mkILExportedTypes []  // TODO: cross-project references with type forwarders
        member __.ShortAssemblyName = assemblyName
        member __.ILScopeRef = IL.ILScopeRef.Assembly ilAssemRef
        member __.ILAssemblyRefs = [] // These are not significant for service scenarios
        member __.HasAnyFSharpSignatureDataAttribute =  true
        member __.HasMatchingFSharpSignatureDataAttribute _ilg = true


/// Manages an incremental build graph for the build of a single F# project
type IncrementalBuilder(compilation: Driver.Compilation, keepAssemblyContents: bool, keepAllBackgroundResolutions: bool) =

    let mutable compilation = compilation
    let () = keepAssemblyContents |> ignore
    let () = keepAllBackgroundResolutions |> ignore

    let fileParsed = new Event<string>()
    let beforeFileChecked = new Event<string>()
    let fileChecked = new Event<string>()
    let projectChecked = new Event<unit>()
#if !NO_EXTENSIONTYPING
    let importsInvalidatedByTypeProvider = new Event<string>()
#endif

    let convert (compilation: Driver.Compilation) ((tcEnv, topAttribs, impl, mty), tcState) =
        {
            TcState = tcState
            TcImports = compilation.TypeChecker.TcImports
            TcGlobals = compilation.TcGlobals
            TcConfig = compilation.TcConfig

            TcEnvAtEnd = tcEnv

            TcErrorsRev = []

            TcResolutionsRev = []

            TcSymbolUsesRev = []

            TcOpenDeclarationsRev = []

            ModuleNamesDict = Map.Empty

            TcDependencyFiles = []

            TopAttribs = Some topAttribs

            TimeStamp = DateTime.Now

            LatestImplementationFile = impl

            LatestCcuSigForFile = Some mty
        }

    member _.TcConfig = compilation.TcConfig
           
    member _.FileParsed = fileParsed.Publish
           
    member _.BeforeFileChecked = beforeFileChecked.Publish
           
    member _.FileChecked = fileChecked.Publish
           
    member _.ProjectChecked = projectChecked.Publish

#if !NO_EXTENSIONTYPING
    member _.ImportsInvalidatedByTypeProvider = importsInvalidatedByTypeProvider.Publish
#endif

    member _.TryGetCurrentTcImports () = Some(compilation.TypeChecker.TcImports)

    member _.AllDependenciesDeprecated: string [] = [||]

    member _.SourceFiles = compilation.SourceFiles

    member _.Step (_ctok: CompilationThreadToken) =  
      cancellable {
        return true
      }
    
    member builder.GetCheckResultsBeforeFileInProjectEvenIfStale filename: PartialCheckResults option  = 
        let compilation = compilation
        compilation.TypeChecker.CheckBefore filename
        |> convert compilation
        |> Some
    
    member builder.AreCheckResultsBeforeFileInProjectReady filename = 
        compilation.TypeChecker.IsReadyBefore filename  

    member builder.GetCheckResultsBeforeFileInProject (_ctok: CompilationThreadToken, filename) = 
        let compilation = compilation
        compilation.TypeChecker.CheckBefore filename
        |> convert compilation
        |> Cancellable.ret

    member builder.GetCheckResultsAfterFileInProject (_ctok: CompilationThreadToken, filename: string) = 
        let compilation = compilation
        compilation.TypeChecker.Check filename
        |> convert compilation
        |> Cancellable.ret

    member builder.GetCheckResultsAfterLastFileInProject (_ctok: CompilationThreadToken) = 
        let compilation = compilation
        compilation.TypeChecker.CheckLast()
        |> convert compilation
        |> Cancellable.ret

    member builder.GetCheckResultsAndImplementationsForProject(_ctok: CompilationThreadToken) = 
      cancellable {
        let compilation = compilation
        let tcConfig = compilation.TcConfig
        let tcGlobals = compilation.TcGlobals
        let tcImports = compilation.TypeChecker.TcImports
        let assemblyName = compilation.AssemblyName
        let outfile = compilation.OutputFileName
        let (tcState, topAttrs, mimpls, tcEnvAtEndOfLastFile) = compilation.TypeChecker.Finish()

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
                                let signingInfo = Driver.ValidateKeySigningAttributes (tcConfig, tcGlobals, topAttrs)
                                match Driver.GetStrongNameSigner signingInfo with 
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

        let result =
            {
                TcState = tcState
                TcImports = tcImports
                TcGlobals = tcGlobals
                TcConfig = tcConfig

                TcEnvAtEnd = tcEnvAtEndOfLastFile

                TcErrorsRev = []

                TcResolutionsRev = []

                TcSymbolUsesRev = []

                TcOpenDeclarationsRev = []

                ModuleNamesDict = Map.Empty

                TcDependencyFiles = []

                TopAttribs = Some topAttrs

                TimeStamp = DateTime.Now

                LatestImplementationFile = None

                LatestCcuSigForFile = None
            }          

        return result, ilAssemRef, tcAssemblyDataOpt, tcAssemblyExprOpt
      }
        
    member __.GetLogicalTimeStampForProject(_cache: TimeStampCache, _ctok: CompilationThreadToken) = 
        DateTime.Now     
      
    member builder.GetParseResultsForFile (_ctok: CompilationThreadToken, filename) =
      cancellable {
        let compilation = compilation
        let parsedInput = compilation.GetParsedInput filename
        return (Some parsedInput, parsedInput.Range, filename, Array.empty<PhasedDiagnostic * FSharpErrorSeverity>)
      }

    /// CreateIncrementalBuilder (for background type checking). Note that fsc.fs also
    /// creates an incremental builder used by the command line compiler.
    static member TryCreateBackgroundBuilderForProjectOptions
                      (_ctok: CompilationThreadToken, 
                       _legacyReferenceResolver: ReferenceResolver.Resolver, 
                       _defaultFSharpBinariesDir: string,
                       _frameworkTcImportsCache: FrameworkImportsCache,
                       _loadClosureOpt: LoadClosure option,
                       _sourceFiles: string list,
                       commandLineArgs: string list,
                       _projectReferences: IProjectReference list,
                       _projectDirectory: string,
                       _useScriptResolutionRules: bool, 
                       keepAssemblyContents: bool,
                       keepAllBackgroundResolutions: bool, 
                       _maxTimeShareMilliseconds: int64,
                       _tryGetMetadataSnapshot: ILBinaryReader.ILReaderTryGetMetadataSnapshot, 
                       suggestNamesForErrors: bool) =
      let _useSimpleResolutionSwitch = "--simpleresolution"

      cancellable {

        // Trap and report warnings and errors from creation.
        let delayedLogger = CapturingErrorLogger("IncrementalBuilderCreation")
        use _unwindEL = PushErrorLoggerPhaseUntilUnwind (fun _ -> delayedLogger)
        use _unwindBP = PushThreadBuildPhaseUntilUnwind BuildPhase.Parameter

        let! builderOpt =
         cancellable {
          try
            let compilation = Driver.Compilation.Create(Array.ofList commandLineArgs)
            let builder = 
                new IncrementalBuilder(compilation,
                                       keepAssemblyContents=keepAssemblyContents, 
                                       keepAllBackgroundResolutions=keepAllBackgroundResolutions)
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
                errorLogger.GetErrors() |> Array.map (fun (d, severity) -> d, severity = FSharpErrorSeverity.Error)
            | _ ->
                Array.ofList delayedLogger.Diagnostics
            |> Array.map (fun (d, isError) -> FSharpErrorInfo.CreateFromException(d, isError, range.Zero, suggestNamesForErrors))

        return builderOpt, diagnostics
      }
