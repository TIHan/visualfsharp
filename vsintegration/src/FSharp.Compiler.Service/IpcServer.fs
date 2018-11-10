namespace FSharp.Compiler.Server

open System
open System.IO
open System.Diagnostics
open System.Threading
open System.Threading.Tasks
open System.IO.Pipes

open Newtonsoft.Json

type IpcMessageServer<'Receive, 'Response>(f : 'Receive -> 'Response) =

    let buffer = Array.zeroCreate<char> Constants.IpcBufferSize

    member __.Start() =
        use fcs = new NamedPipeServerStream("fsharpcompilerserver", PipeDirection.InOut, 1, PipeTransmissionMode.Message, PipeOptions.Asynchronous)

        printfn "[FSharp Compiler Server] - Transmission Mode: %A" fcs.TransmissionMode
        fcs.WaitForConnection()

        printfn "Client Connected"

        use writer = new StreamWriter(fcs)
        use reader = new StreamReader(fcs)
        writer.AutoFlush <- true

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

                printfn "Received: %A" receivedMsg

                let responseMsg = f receivedMsg

                writer.Write(JsonConvert.SerializeObject(responseMsg))

