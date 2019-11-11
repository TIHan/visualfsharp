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
open System.Security.AccessControl

type FSCompilerClient (run) =
    
    let pipeClient = new NamedPipeClientStream(".", "FSCompiler", PipeDirection.InOut, PipeOptions.None, Security.Principal.TokenImpersonationLevel.Impersonation)

    member _.Start(argv: string [], timeout) =
        try
            pipeClient.Connect(timeout)
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
                1
        with
        | :? IOException as ex ->
            printfn "FSCompiler Client IO Error: %s" ex.Message
            run argv // fallback

    interface IDisposable with

        member _.Dispose() = pipeClient.Dispose()

type FSCompilerServer (run) =

    let rec server () =
        use pipeServer = new NamedPipeServerStream("FSCompiler", PipeDirection.InOut, NamedPipeServerStream.MaxAllowedServerInstances, PipeTransmissionMode.Message, PipeOptions.None)
        try
            pipeServer.WaitForConnection()

            let mutable didDisconnect = false
            while not didDisconnect && pipeServer.IsConnected do
                if pipeServer.IsMessageComplete then
                    use sr = new StreamReader(pipeServer, Encoding.UTF8, false, 1024 * 10, true)
                    use sw = new StreamWriter(pipeServer, Encoding.UTF8, 1024 * 10, true)
                    let msg = sr.ReadLine()
                    let argv = msg.Split(';')
                    let exitCode: int = run argv
                    sw.WriteLine("***success***" + string exitCode)
                    sw.Flush()
                    pipeServer.WaitForPipeDrain()
                    didDisconnect <- true
        with
        | :? IOException as ex ->
            printfn "FSCompiler Server IO Error: %s" ex.Message
        | ex ->
            printfn "%A" ex
        server ()

    member this.Start() =
        let waitHandle = new EventWaitHandle(false, EventResetMode.AutoReset)
        let threadCount = Environment.ProcessorCount

        for _ in 1..threadCount do
            let thread = Thread(server)
            thread.Start()

        waitHandle.WaitOne() |> ignore

    interface IDisposable with

        member _.Dispose() = ()

module RuntimeHostInfo =

#if NETCOREAPP
    let tryGetDotNetPath() = Some "dotnet"
#else
    let tryGetDotNetPath() = None
#endif

    let entryName = System.Reflection.Assembly.GetEntryAssembly().Location


let processStartServer() =
    let startInfo = 
        match RuntimeHostInfo.tryGetDotNetPath() with
        | Some dotNetPath ->
            ProcessStartInfo(dotNetPath, [|RuntimeHostInfo.entryName;"server"|] |> Array.reduce(fun s1 s2 -> s1 + " " + s2))
        | _ ->
            ProcessStartInfo(RuntimeHostInfo.entryName, [|"server"|] |> Array.reduce(fun s1 s2 -> s1 + " " + s2))

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

///// <returns>
///// Null if not enough information was found to create a valid pipe name.
///// </returns>
//internal static string GetPipeNameForPathOpt(string compilerExeDirectory)
//{
//    // Prefix with username and elevation
//    bool isAdmin = false;
//    if (PlatformInformation.IsWindows)
//    {
//        var currentIdentity = WindowsIdentity.GetCurrent();
//        var principal = new WindowsPrincipal(currentIdentity);
//        isAdmin = principal.IsInRole(WindowsBuiltInRole.Administrator);
//    }

//    var userName = Environment.UserName;
//    if (userName == null)
//    {
//        return null;
//    }

//    return GetPipeName(userName, isAdmin, compilerExeDirectory);
//}

//internal static string GetPipeName(
//    string userName,
//    bool isAdmin,
//    string compilerExeDirectory)
//{
//    // Normalize away trailing slashes.  File APIs include / exclude this with no 
//    // discernable pattern.  Easiest to normalize it here vs. auditing every caller
//    // of this method.
//    compilerExeDirectory = compilerExeDirectory.TrimEnd(Path.DirectorySeparatorChar);

//    var pipeNameInput = $"{userName}.{isAdmin}.{compilerExeDirectory}";
//    using (var sha = SHA256.Create())
//    {
//        var bytes = sha.ComputeHash(Encoding.UTF8.GetBytes(pipeNameInput));
//        return Convert.ToBase64String(bytes)
//            .Replace("/", "_")
//            .Replace("=", string.Empty);
//    }
//}

[<EntryPoint>]
let main(argv) =
    let isServer, argv =
        if argv.Length > 0 && argv.[0] = "server" then true, [||]
        else false, (Array.append [|"--build-from:" + Directory.GetCurrentDirectory()|] argv)

    let mutable createdNew = false
    use serverMutex = new Mutex(true, "FSCompiler", &createdNew)

    if isServer then
        use server = new FSCompilerServer(run)
        server.Start()
        0
    else
        if createdNew then
            serverMutex.ReleaseMutex()
            processStartServer()

        use client = new FSCompilerClient(run)
        client.Start(argv, 60000)
