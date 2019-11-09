// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

module internal FSharp.Compiler.CommandLineMain

open System
open System.Diagnostics
open System.IO
open System.Reflection
open System.Runtime.CompilerServices

open FSharp.Compiler
open FSharp.Compiler.AbstractIL
open FSharp.Compiler.AbstractIL.IL 
open FSharp.Compiler.AbstractIL.ILBinaryReader 
open FSharp.Compiler.ErrorLogger
open FSharp.Compiler.Driver
open FSharp.Compiler.Lib
open FSharp.Compiler.Range
open FSharp.Compiler.CompileOps
open FSharp.Compiler.AbstractIL.Internal.Library 
open Internal.Utilities

[<Dependency("FSharp.Compiler.Private",LoadHint.Always)>] 
do ()


module Driver = 
    let main argv = 

        let ctok = AssumeCompilationThreadWithoutEvidence ()

        // Check for --pause as the very first step so that a compiler can be attached here.
        let pauseFlag = argv |> Array.exists  (fun x -> x = "/pause" || x = "--pause")
        if pauseFlag then 
            System.Console.WriteLine("Press return to continue...")
            System.Console.ReadLine() |> ignore

#if !FX_NO_APP_DOMAINS
        let timesFlag = argv |> Array.exists  (fun x -> x = "/times" || x = "--times")
        if timesFlag then 
            let stats = ILBinaryReader.GetStatistics()
            AppDomain.CurrentDomain.ProcessExit.Add(fun _ -> 
                printfn "STATS: #ByteArrayFile = %d, #MemoryMappedFileOpen = %d, #MemoryMappedFileClosed = %d, #RawMemoryFile = %d, #WeakByteArrayFile = %d" 
                    stats.byteFileCount 
                    stats.memoryMapFileOpenedCount 
                    stats.memoryMapFileClosedCount 
                    stats.rawMemoryFileCount 
                    stats.weakByteFileCount)
#endif

        let quitProcessExiter = 
            { new Exiter with 
                member x.Exit(n) =                    
                    try ()
                      //exit n
                    with _ -> 
                      ()
                    Unchecked.defaultof<_>
                    //failwithf "%s" <| FSComp.SR.elSysEnvExitDidntExit() 
            }

        let legacyReferenceResolver = 
#if CROSS_PLATFORM_COMPILER
            SimulatedMSBuildReferenceResolver.SimulatedMSBuildResolver
#else
            LegacyMSBuildReferenceResolver.getResolver()
#endif

        // This is the only place where ReduceMemoryFlag.No is set. This is because fsc.exe is not a long-running process and
        // thus we can use file-locking memory mapped files.
        //
        // This is also one of only two places where CopyFSharpCoreFlag.Yes is set.  The other is in LegacyHostedCompilerForTesting.
        mainCompile (ctok, argv, legacyReferenceResolver, (*bannerAlreadyPrinted*)false, ReduceMemoryFlag.No, CopyFSharpCoreFlag.Yes, quitProcessExiter, ConsoleLoggerProvider(), None, None)
        0 

let run argv =
    System.Runtime.GCSettings.LatencyMode <- System.Runtime.GCLatencyMode.Batch
    use unwindBuildPhase = PushThreadBuildPhaseUntilUnwind BuildPhase.Parameter

    if not runningOnMono then Lib.UnmanagedProcessExecutionOptions.EnableHeapTerminationOnCorruption() (* SDL recommendation *)
    Lib.UnmanagedProcessExecutionOptions.EnableHeapTerminationOnCorruption() (* SDL recommendation *)

    try 
        Driver.main(Array.append [| "fsc.exe" |] argv)
    with e -> 
        errorRecovery e FSharp.Compiler.Range.range0
        1

open System.IO
open System.IO.Pipes
open System.Text
open System.Threading
open System.Diagnostics

type FSCompilerServer () =

    member this.StartLoop(run) =
        while true do
            printfn "Waiting for connection..."
            use pipeServer = new NamedPipeServerStream("FSCompiler", PipeDirection.InOut, NamedPipeServerStream.MaxAllowedServerInstances, PipeTransmissionMode.Message, PipeOptions.None)
            try
                pipeServer.WaitForConnection()

                while pipeServer.IsConnected do
                    if pipeServer.IsMessageComplete then
                        use sr = new StreamReader(pipeServer, Encoding.UTF8, false, 1024 * 10, true)
                        use sw = new StreamWriter(pipeServer, Encoding.UTF8, 1024 * 10, true)
                        let msg = sr.ReadLine()
                        let argv = msg.Split(';')
                        let exitCode: int = run argv
                        sw.WriteLine("***success***" + string exitCode)
                        sw.Flush()
                        pipeServer.WaitForPipeDrain()
                        pipeServer.Disconnect()
            with
            | :? IOException as ex ->
                printfn "FSCompiler Server IO Error: %s" ex.Message
            | ex ->
                printfn "%A" ex

    interface IDisposable with

        member _.Dispose() = ()

type FSCompilerClient () =
    
    let pipeClient = new NamedPipeClientStream(".", "FSCompiler", PipeDirection.InOut, PipeOptions.None, Security.Principal.TokenImpersonationLevel.Impersonation)

    member _.Start(argv: string [], connectTimeoutInSeconds) =
        try
            pipeClient.Connect(TimeSpan.FromSeconds(float connectTimeoutInSeconds).TotalMilliseconds |> int)
            use sr = new StreamReader(pipeClient, Encoding.UTF8, false, 256, true)          
            use sw = new StreamWriter(pipeClient, Encoding.UTF8, 256, true)
            sw.AutoFlush <- true

            let msg =
                if argv.Length > 0 then
                    argv |> Array.reduce (fun s1 s2 -> s1 + ";" + s2)
                else
                    String.Empty
            sw.WriteLine(msg)
            let read = sr.ReadLine()
            if read.StartsWith("***success***") then
                Int32.Parse(read.Replace("***success***", String.Empty))
            else
                stderr.Write read
                0
        with
        | :? IOException as ex ->
            printfn "FSCompiler Client IO Error: %s" ex.Message
            0

    interface IDisposable with

        member _.Dispose() = pipeClient.Dispose()

module RuntimeHostInfo =

#if NETCOREAPP
    let tryGetDotNetPath() = Some "dotnet"
#else
    let tryGetDotNetPath() = None
#endif

    let entryName = System.Reflection.Assembly.GetEntryAssembly().Location


let processStartServer(argv) =
    let startInfo = 
        match RuntimeHostInfo.tryGetDotNetPath() with
        | Some dotNetPath ->
            ProcessStartInfo(dotNetPath, Array.append [|RuntimeHostInfo.entryName;"server"|] argv |> Array.reduce(fun s1 s2 -> s1 + " " + s2))
        | _ ->
            ProcessStartInfo(RuntimeHostInfo.entryName, Array.append [|"server"|] argv |> Array.reduce(fun s1 s2 -> s1 + " " + s2))

    startInfo.UseShellExecute <- true

    Process.Start(startInfo) |> ignore

let fixPath (x: string) =
    if not (String.IsNullOrWhiteSpace(x)) && not (x.StartsWith("-")) && not (Path.IsPathRooted(x)) then
        Path.Combine(Environment.CurrentDirectory, x)
    elif x.StartsWith("-o:") then
        let path = x.Replace("-o:", "")
        if Path.IsPathRooted(path) then
            x
        else
            "-o:" + Path.Combine(Environment.CurrentDirectory, path)
    else
        x

[<EntryPoint>]
let main(argv) =
    let isServer, argv =
        if argv.Length > 0 && argv.[0] = "server" then true, argv |> Array.skip 1
        else false, (argv 
                     |> Array.map (fun x -> 
                        if x.StartsWith("@") then
                            let filePath = x.Replace("@", "")
                            let text =
                                File.ReadAllText(filePath).Replace("\r", "").Split('\n')
                                |> Array.map fixPath
                                |> Array.reduce (fun s1 s2 -> s1 + "\n" + s2)
                            File.WriteAllText(filePath, text)
                            x
                        else
                            fixPath x)
                     )

    if isServer then
        use server = new FSCompilerServer()
        server.StartLoop(run)
        1
    else
        try
            use client = new FSCompilerClient()
            client.Start(argv, 1)
        with
        | :? TimeoutException ->
            processStartServer(argv)
            use client = new FSCompilerClient()
            client.Start(argv, 10)
