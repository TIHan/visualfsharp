namespace FSharp.Compiler.Server

open System
open System.IO
open System.Reflection
open System.Diagnostics

open Newtonsoft.Json

open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.SourceCodeServices

[<RequireQualifiedAccess>]
type CompilerCommand =
    | GetSemanticClassification of GetSemanticClassificationCommand
    | GetMatchBraces

[<RequireQualifiedAccess>]
type CompilerResult =
    | Error
    | GetSemanticClassification of GetSemanticClassificationResult option

/// https://stackoverflow.com/questions/3342941/kill-child-process-when-parent-process-is-killed
module WindowsHelpers =
    open System.Runtime.InteropServices
    open FSharp.NativeInterop

    type JobObjectInfoType =
        | AssociateCompletionPortInformation = 7
        | BasicLimitInformation = 2
        | BasicUIRestrictions = 4
        | EndOfJobTimeInformation = 6
        | ExtendedLimitInformation = 9
        | SecurityLimitInformation = 5
        | GroupInformation = 11

    [<Struct>]
    type SECURITY_ATTRIBUTES =
        val mutable nLength : int
        val mutable lpSecurityDescriptor : IntPtr
        val mutable bInheritHandle : int

    [<Struct>]
    type IO_COUNTERS =
        val mutable ReadOperationCount : uint64
        val mutable WriteOperationCount : uint64
        val mutable OtherOperationCount : uint64
        val mutable ReadTransferCount : uint64
        val mutable WriteTransferCount : uint64
        val mutable OtherTransferCount : uint64

    [<Struct>]
    type JOBOBJECT_BASIC_LIMIT_INFORMATION =
        val mutable PerProcessUserTimeLimit : int64
        val mutable PerJobUserTimeLimit : int64
        val mutable LimitFlags : int16
        val mutable MinimumWorkingSetSize : UIntPtr
        val mutable MaximumWorkingSetSize : UIntPtr
        val mutable ActiveProcessLimit : int16
        val mutable Affinity : int64
        val mutable PriorityClass : int16
        val mutable SchedulingClass : int16

    [<Struct>]
    type JOBOBJECT_EXTENDED_LIMIT_INFORMATION =
        val mutable BasicLimitInformation : JOBOBJECT_BASIC_LIMIT_INFORMATION
        val mutable IoInfo : IO_COUNTERS
        val mutable ProcessMemoryLeft : uint32
        val mutable JobMemoryLimit : uint32
        val mutable PeakProcessMemoryUsed : uint32
        val mutable PeakJobMemoryUsed : uint32

    [<DllImport("kernel32.dll", CharSet = CharSet.Unicode)>]
    extern IntPtr CreateJobObject(obj a, string lpName)

    [<DllImport("kernel32.dll")>]
    extern bool SetInformationJobObject(IntPtr hJob, JobObjectInfoType infoType, IntPtr lpJobObjectInfo, uint32 cbJobObjectInfoLength)

    [<DllImport("kernel32.dll", SetLastError = true)>]
    extern bool AssignProcessToJobObject(IntPtr job, IntPtr proc)

    [<DllImport("kernel32.dll")>]
    extern bool CloseHandle(IntPtr hObject)

    type Job() =

        let mutable isDisposed = false
        let mutable handle = CreateJobObject(null, null)

        do
            let mutable info = JOBOBJECT_BASIC_LIMIT_INFORMATION()
            info.LimitFlags <- 0x2000s

            let mutable extendedInfo = JOBOBJECT_EXTENDED_LIMIT_INFORMATION()
            extendedInfo.BasicLimitInformation <- info

            let length = Marshal.SizeOf(typeof<JOBOBJECT_EXTENDED_LIMIT_INFORMATION>)
            let extendedInfoPtr = Marshal.AllocHGlobal(length)
            Marshal.StructureToPtr(extendedInfo, extendedInfoPtr, false)
            
            if not (SetInformationJobObject(handle, JobObjectInfoType.ExtendedLimitInformation, extendedInfoPtr, uint32 length)) then
                failwithf "Unable to set information. Error: %i" (Marshal.GetLastWin32Error())

        member private __.Dispose(_isDisposing) =
            if isDisposed then ()
            else

            CloseHandle(handle) |> ignore
            handle <- IntPtr.Zero

            isDisposed <- true

        member __.AddProcess(handleProc) =
            AssignProcessToJobObject(handle, handleProc)
            
        interface IDisposable with

            member this.Dispose() =
                this.Dispose(true)
                GC.SuppressFinalize(this)


[<Sealed>]
type internal CompilerServerOut() =

    let mutable procOpt : Process option = None
    let mutable isStarted = false
    let mutable extraDispose = id
    let mutable restartingHandler = Unchecked.defaultof<_>

    let getAssemblyDirectory (asm: Assembly) =
        Path.GetDirectoryName(asm.Location)

    let startProcess () =
        match procOpt with
        | Some(proc) -> 
            try proc.Kill() with | _ -> ()
            procOpt <- None
        | _ -> ()

        try
            let p = new Process()
            p.StartInfo.UseShellExecute <- true
            p.StartInfo.FileName <- Path.Combine(getAssemblyDirectory (Assembly.GetExecutingAssembly()), "FSharp.Compiler.Server.exe")

            procOpt <- Some(p)

            p.Start() |> ignore

            // For windows, we want to kill this child process if the parent process dies/closes.
            match Environment.OSVersion.Platform with
            | x when 
                x = PlatformID.Win32NT ||
                x = PlatformID.Win32S ||
                x = PlatformID.WinCE ->
                let job = new WindowsHelpers.Job()
                job.AddProcess(p.Handle) |> ignore
                extraDispose <- fun () -> (job :> IDisposable).Dispose()
            | _ -> ()

        with
        | ex ->
            printfn "failed: %s" ex.Message
            reraise()
        

    let ipcClient = new IpcMessageClient<CompilerCommand, CompilerResult>()

    do
        restartingHandler <- ipcClient.Restarting.Subscribe(fun () ->
            startProcess ()
        )

    member __.Start() =
        if not isStarted then
            startProcess ()
            ipcClient.Start ()
            isStarted <- true
        else
            failwith "FSharp Compiler Server Client already started."

    interface ICompilerServer with

        member __.GetSemanticClassificationAsync(cmd) = async {
            match! ipcClient.Send(CompilerCommand.GetSemanticClassification(cmd)) with
            | CompilerResult.GetSemanticClassification(result) -> return result
            | _ -> return None
        }

    interface IDisposable with

        member __.Dispose() =
            restartingHandler.Dispose()
            (ipcClient :> IDisposable).Dispose()
            match procOpt with
            | Some(proc) -> try proc.Dispose() with | _ -> ()
            | _ -> ()
            extraDispose()

[<Sealed>]
type internal CompilerServerIn(checker: FSharpChecker) =

    interface ICompilerServer with 

        member __.GetSemanticClassificationAsync(cmd: GetSemanticClassificationCommand) = async {
                let data = cmd.CheckerData
                let! _, fileAnswer = checker.ParseAndCheckFileInProject(data.FilePath, data.TextVersionHash, data.SourceText, data.Options.ToFSharpProjectOptions(), data.UserOpName)
                match fileAnswer with
                | FSharpCheckFileAnswer.Aborted -> return None
                | FSharpCheckFileAnswer.Succeeded(fileResults) ->
                    let start = mkPos cmd.RangeToClassify.StartLine cmd.RangeToClassify.StartColumn
                    let en = mkPos cmd.RangeToClassify.EndLine cmd.RangeToClassify.EndColumn
                    let targetRange = mkRange cmd.CheckerData.FilePath start en
                    return
                        Some { Items =
                            fileResults.GetSemanticClassification(Some(targetRange))
                            |> Array.map (fun (m, t) -> { Range = { StartLine = m.StartLine; StartColumn = m.StartColumn; EndLine = m.EndLine; EndColumn = m.EndColumn }; Type = t })
                        } 
            }

    interface IDisposable with

        member __.Dispose() = ()

module CompilerServer =

    let Run () =
        let checker = FSharpChecker.Create()
        let server = new CompilerServerIn(checker) :> ICompilerServer

        let ipcServer = IpcMessageServer<CompilerCommand, CompilerResult>(fun cmd -> async {
                match cmd with
                | CompilerCommand.GetSemanticClassification(cmd) ->
                    let! result = server.GetSemanticClassificationAsync(cmd)
                    return CompilerResult.GetSemanticClassification(result)
                | _ -> return CompilerResult.GetSemanticClassification(None)
        })
       
        ipcServer.Run()

    let CreateInProcess checker =
        new CompilerServerIn(checker) :> ICompilerServer

    let CreateOutOfProcess () =
        let client = new CompilerServerOut()
        client.Start()
        client :> ICompilerServer