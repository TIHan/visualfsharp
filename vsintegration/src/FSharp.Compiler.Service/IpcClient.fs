namespace FSharp.Compiler.Server

open System
open System.IO
open System.Diagnostics
open System.Threading
open System.Threading.Tasks
open System.IO.Pipes

open Newtonsoft.Json

type IpcMessage<'Send, 'Receive> =
    {
        Data: 'Send
        Reply: AsyncReplyChannel<'Receive>
    }

type IpcMessageClient<'Send, 'Receive>() =

    let buffer = Array.zeroCreate<char> Constants.IpcBufferSize

    let restartingEvent = Event<unit>()

    let mutable agent = Unchecked.defaultof<_>

    member this.Start() =
        agent <- MailboxProcessor<IpcMessage<'Send, 'Receive>>.Start(fun agent -> async {
            try
                use fcs = new NamedPipeClientStream(".", "fsharpcompilerserver", PipeDirection.InOut, PipeOptions.Asynchronous)
                fcs.Connect()
                fcs.ReadMode <- PipeTransmissionMode.Message

                use writer = new StreamWriter(fcs)
                writer.AutoFlush <- true

                use reader = new StreamReader(fcs)

                writer.Write("Client Connected")

                let count = reader.Read(buffer, 0, buffer.Length)
                let msg = String(buffer, 0, count)
                printfn "%A" msg
                printfn "You are CONNECTED"

                while true do
                
                    let! msg = agent.Receive()

                    if not fcs.IsConnected then
                        failwith "Disconnected"

                    writer.Write(JsonConvert.SerializeObject(msg.Data))

                    let count = reader.Read(buffer, 0, buffer.Length)
                    let msgString = String(buffer, 0, count)

                    msg.Reply.Reply(JsonConvert.DeserializeObject<'Receive>(msgString))
            with
            | ex ->
                printfn "[FSharp Compiler Client] - Restarting due to: %s" ex.Message
                restartingEvent.Trigger()
                this.Start()
        })

    member __.Send(msg) =
        if not (obj.ReferenceEquals(agent, null)) then
            agent.PostAndAsyncReply(fun reply -> { Data = msg; Reply = reply })
        else
            failwith "IpcClient not started."

    [<CLIEvent>]
    member __.Restarting = restartingEvent.Publish

    interface IDisposable with

        member __.Dispose() =
            if not (obj.ReferenceEquals(agent, null)) then
                (agent :> IDisposable).Dispose()

