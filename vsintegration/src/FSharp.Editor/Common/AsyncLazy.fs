module internal Microsoft.VisualStudio.FSharp.Editor.Utilities

open System
open System.Threading

[<RequireQualifiedAccess>]
type private ValueStrength<'T when 'T : not struct> =
    | None
    | Strong of 'T
    | Weak of WeakReference<'T>

    member this.TryGetTarget (value: outref<'T>) =
        match this with
        | ValueStrength.None -> 
            false
        | ValueStrength.Strong v ->
            value <- v
            true
        | ValueStrength.Weak v ->
            v.TryGetTarget &value

type private AsyncLazyWeakMessage<'T> =
    | GetValue of AsyncReplyChannel<Result<'T, Exception>> * CancellationToken

type private AgentInstance<'T> = (MailboxProcessor<AsyncLazyWeakMessage<'T>> * CancellationTokenSource)

[<RequireQualifiedAccess>]
type private AgentAction<'T> =
    | GetValue of AgentInstance<'T>
    | CachedValue of 'T

[<Sealed>]
type AsyncLazyWeak<'T when 'T : not struct> (computation: Async<'T>) =

    let gate = obj ()
    let mutable requestCount = 0
    let mutable cachedResult: WeakReference<'T> voption = ValueNone

    let tryGetResult () =
        match cachedResult with
        | ValueSome weak ->
            match weak.TryGetTarget () with
            | true, result -> ValueSome result
            | _ -> ValueNone
        | _ -> ValueNone

    let loop (agent: MailboxProcessor<AsyncLazyWeakMessage<'T>>) =
        async {
            while true do
                match! agent.Receive() with
                | GetValue (replyChannel, ct) ->
                    try
                        use _reg = 
                            ct.Register (fun () -> 
                                let ex = OperationCanceledException() :> exn
                                replyChannel.Reply (Error ex)
                            )
                        ct.ThrowIfCancellationRequested ()

                        match tryGetResult () with
                        | ValueSome result ->
                            replyChannel.Reply (Ok result)
                        | _ ->
                            let! result = computation
                            cachedResult <- ValueSome (WeakReference<_> result)

                            if not ct.IsCancellationRequested then
                                replyChannel.Reply (Ok result) 
                    with 
                    | ex ->
                        replyChannel.Reply (Error ex)
        }

    let mutable agentInstance: (MailboxProcessor<AsyncLazyWeakMessage<'T>> * CancellationTokenSource) option = None

    member __.GetValueAsync () =
       async {
           // fast path
           // TODO: Perhaps we could make the fast path non-allocating since we create a new async everytime.
           match tryGetResult () with
           | ValueSome result -> return result
           | _ ->
               let action =
                    lock gate <| fun () ->
                        // We try to get the cached result after the lock so we don't spin up a new mailbox processor.
                        match tryGetResult () with
                        | ValueSome result -> AgentAction<'T>.CachedValue result
                        | _ ->
                            requestCount <- requestCount + 1
                            match agentInstance with
                            | Some agentInstance -> AgentAction<'T>.GetValue agentInstance
                            | _ ->
                                let cts = new CancellationTokenSource ()
                                let agent = new MailboxProcessor<AsyncLazyWeakMessage<'T>> ((fun x -> loop x), cancellationToken = cts.Token)
                                let newAgentInstance = (agent, cts)
                                agentInstance <- Some newAgentInstance
                                agent.Start ()
                                AgentAction<'T>.GetValue newAgentInstance

               match action with
               | AgentAction.CachedValue result -> return result
               | AgentAction.GetValue (agent, cts) ->
                        
                   try
                       let! ct = Async.CancellationToken
                       match! agent.PostAndAsyncReply (fun replyChannel -> GetValue(replyChannel, ct)) with
                       | Ok result -> return result
                       | Error ex -> return raise ex
                    finally
                        lock gate <| fun () ->
                            requestCount <- requestCount - 1
                            if requestCount = 0 then
                                 cts.Cancel () // cancel computation when all requests are cancelled
                                 (agent :> IDisposable).Dispose ()
                                 cts.Dispose ()
                                 agentInstance <- None
       }

       member __.TryGetValue () = tryGetResult ()

[<Sealed>]
type AsyncLazy<'T> (computation) =
    
    let computation =
        async {
            let! result = computation
            return ref result
        }
    let gate = obj ()
    let mutable asyncLazyWeak = ValueSome (AsyncLazyWeak<'T ref> computation)
    let mutable cachedResult = ValueNone // hold strongly

    member __.GetValueAsync () =
        async {
            // fast path
            // TODO: Perhaps we could make the fast path non-allocating since we create a new async everytime.
            match cachedResult, asyncLazyWeak with
            | ValueSome result, _ -> return result
            | _, ValueSome weak ->
                let! result = weak.GetValueAsync ()
                lock gate <| fun () ->
                    // Make sure we set it only once.
                    if cachedResult.IsNone then
                        cachedResult <- ValueSome result.contents
                        asyncLazyWeak <- ValueNone // null out computation function so we don't strongly hold onto any references once we finished computing.
                return cachedResult.Value
            | _ -> 
                return failwith "should not happen"
        }

    member __.TryGetValue () = cachedResult