namespace Microsoft.FSharp.Compiler.Server

open System
open System.IO
open System.Diagnostics
open System.Threading
open System.Threading.Tasks
open System.IO.Pipes

open Newtonsoft.Json

type IpcMessageServer<'Receive, 'Response>(f : 'Receive -> Async<'Response>) =

    let buffer = Array.zeroCreate<char> Constants.IpcBufferSize

    member this.Run() =
        use fcs = new NamedPipeServerStream("fsharpcompilerserver", PipeDirection.InOut, 1, PipeTransmissionMode.Message, PipeOptions.None)

        printfn "[FSharp Compiler Server] - Transmission Mode: %A" fcs.TransmissionMode
        printfn "[FSharp Compiler Server] - Waiting for connection"

        fcs.WaitForConnection()

        printfn "[FSharp Compiler Server] - Client Connected"

        use writer = new StreamWriter(fcs, AutoFlush = true)
        use reader = new StreamReader(fcs)

        let count = reader.Read(buffer, 0, buffer.Length)
        let msg = String(buffer, 0, count)
        printfn "%A" msg

        writer.Write("Server says hello.")

        while true do
            if not fcs.IsConnected then
                failwith "Disconnected"

            let count = reader.Read(buffer, 0, buffer.Length)
            if count > 0 then
                let msgString = String(buffer, 0, count)

                let receivedMsg = JsonConvert.DeserializeObject<'Receive>(msgString)

                printfn "Received: %A" (receivedMsg.GetType().Name)

                let responseMsg = f receivedMsg |> Async.RunSynchronously

                writer.Write(JsonConvert.SerializeObject(responseMsg))
