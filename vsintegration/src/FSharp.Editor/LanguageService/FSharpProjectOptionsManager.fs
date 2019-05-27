﻿// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

namespace Microsoft.VisualStudio.FSharp.Editor

open System
open System.Diagnostics
open System.Collections.Generic
open System.Collections.Concurrent
open System.Collections.Immutable
open System.ComponentModel.Composition
open System.IO
open System.Linq
open Microsoft.CodeAnalysis
open FSharp.Compiler.CompileOps
open FSharp.Compiler.SourceCodeServices
open Microsoft.VisualStudio
open Microsoft.VisualStudio.FSharp.Editor
open Microsoft.VisualStudio.LanguageServices
open Microsoft.VisualStudio.LanguageServices.Implementation.ProjectSystem
open Microsoft.VisualStudio.Shell
open System.Threading
open Microsoft.VisualStudio.Shell.Interop
open Microsoft.VisualStudio.LanguageServices.Implementation.TaskList

[<AutoOpen>]
module private FSharpProjectOptionsHelpers =

    let mapCpsProjectToSite(project:Project, cpsCommandLineOptions: IDictionary<ProjectId, string[] * string[]>) =
        let sourcePaths, referencePaths, options =
            match cpsCommandLineOptions.TryGetValue(project.Id) with
            | true, (sourcePaths, options) -> sourcePaths, [||], options
            | false, _ -> [||], [||], [||]
        let mutable errorReporter = Unchecked.defaultof<_> // we don't use this

        {
            new IProjectSite with
                member __.Description = project.Name
                member __.CompilationSourceFiles = sourcePaths
                member __.CompilationOptions =
                    Array.concat [options; referencePaths |> Array.map(fun r -> "-r:" + r)]
                member __.CompilationReferences = referencePaths
                member site.CompilationBinOutputPath = site.CompilationOptions |> Array.tryPick (fun s -> if s.StartsWith("-o:") then Some s.[3..] else None)
                member __.ProjectFileName = project.FilePath
                member __.AdviseProjectSiteChanges(_,_) = ()
                member __.AdviseProjectSiteCleaned(_,_) = ()
                member __.AdviseProjectSiteClosed(_,_) = ()
                member __.IsIncompleteTypeCheckEnvironment = false
                member __.TargetFrameworkMoniker = ""
                member __.ProjectGuid =  project.Id.Id.ToString()
                member __.LoadTime = System.DateTime.Now
                member __.ProjectProvider = None
                member __.BuildErrorReporter with get () = errorReporter and set (v) = errorReporter <- v
        }

[<RequireQualifiedAccess>]
module internal FSharpCompilationHelpers =
    
    [<NoEquality; NoComparison>]
    type cenv =
        {
            checker: FSharpChecker
            cache: ConcurrentDictionary<ProjectId, Project * FSharpParsingOptions * FSharpProjectOptions>
            // Hack to store command line options from HandleCommandLineChanges, remove it when HandleCommandLineChanges gets removed.
            cpsCommandLineOptions: ConcurrentDictionary<ProjectId, string [] * string []>
            enableInMemoryCrossProjectReferences: bool
            /// We only use the workspace directly when checking to see if something is from cps or legacy projects.
            /// This will go away when cps and legacy are fully unified to the workspace, meaning we can get command line options and file ordered documents from a workspace project.
            /// This will most likely be removed when 'cpsCommandLineOptions' gets removed.
            workspace: Workspace
            mutable currentSolution: Solution option
        }

    let createCompilationEnv workspace checker enableInMemoryCrossProjectReferences =
        {
            checker = checker
            cache = ConcurrentDictionary ()
            cpsCommandLineOptions = ConcurrentDictionary ()
            enableInMemoryCrossProjectReferences = enableInMemoryCrossProjectReferences
            workspace = workspace
            currentSolution = None
        }

    let invalidateProjectCache cenv projectId =
        cenv.cache.TryRemove projectId |> ignore

    let setProjectCache cenv (project: Project) parsingOptions projectOptions =
        cenv.cache.[project.Id] <- (project, parsingOptions, projectOptions)

    let updateProject cenv (project: Project) =
        match cenv.cache.TryGetValue project.Id with
        | true, (_, parsingOptions, projectOptions) ->
            setProjectCache cenv project parsingOptions projectOptions
        | _ ->
            ()

    let invalidateProject cenv projectId =
        match cenv.cache.TryGetValue projectId with
        | true, (_, _, projectOptions) ->
            Logging.Logging.logInfof "*** fully invalidate project - %A" projectId
            invalidateProjectCache cenv projectId
            cenv.cpsCommandLineOptions.TryRemove projectId |> ignore
            cenv.checker.InvalidateProject projectOptions
        | _ -> ()

    let clearCaches cenv =
        // We don't clear cpsCommandLineOptions as it could be dangerous, we only try to remove from cpsCommandLineOptions invdividual projects.
        cenv.cache.Clear ()
        cenv.checker.StopBackgroundCompile ()
        cenv.checker.InvalidateAll ()

    let setSolution cenv (solution: Solution) =
        let checker = cenv.checker
        match cenv.currentSolution with
        | Some oldSolution when solution.Id <> oldSolution.Id ->
            clearCaches cenv

        | Some oldSolution when solution.Version <> oldSolution.Version ->
            checker.StopBackgroundCompile ()
            Logging.Logging.logInfof "*** solution version: %A -- old solution version: %A" (solution.Version.GetHashCode()) (oldSolution.Version.GetHashCode())
            let changes = solution.GetChanges oldSolution
            for removedProject in changes.GetRemovedProjects() do
                invalidateProject cenv removedProject.Id

        | _ -> ()

        cenv.currentSolution <- Some solution

    let hasProjectChanged (oldProject: Project) (project: Project) =
        oldProject.Version <> project.Version

    let rec tryComputeOptionsByFile cenv (document: Document) (ct: CancellationToken) =
        async {
            let isScript = isScriptFile document.FilePath
            match cenv.cache.TryGetValue(document.Project.Id) with
            | false, _ ->
                let checker = cenv.checker
                let! sourceText = document.GetTextAsync(ct) |> Async.AwaitTask
                let! projectOptions, _ =
                    if isScript then
                        checker.GetProjectOptionsFromScript(document.FilePath, sourceText.ToFSharpSourceText())
                    else
                        async {
                            return {
                                ProjectFileName = document.FilePath
                                ProjectId = None
                                SourceFiles = [|document.FilePath|]
                                OtherOptions = [||]
                                ReferencedProjects = [||]
                                IsIncompleteTypeCheckEnvironment = false
                                UseScriptResolutionRules = SourceFile.MustBeSingleFileProject (Path.GetFileName(document.FilePath))
                                LoadTime = DateTime.Now
                                UnresolvedReferences = None
                                OriginalLoadReferences = []
                                ExtraProjectInfo= None
                                Stamp = None
                            }, Unchecked.defaultof<_>
                        }

                checker.CheckProjectInBackground(projectOptions, userOpName="checkOptions")

                let parsingOptions, _ = checker.GetParsingOptionsFromProjectOptions(projectOptions)

                setProjectCache cenv document.Project parsingOptions projectOptions

                return Some(parsingOptions, projectOptions)

            | true, (oldProject, parsingOptions, projectOptions) ->
                let! version = document.Project.GetDependentVersionAsync ct |> Async.AwaitTask
                let! oldVersion = oldProject.GetDependentVersionAsync ct |> Async.AwaitTask
                // Only recompute if it's a script.
                if version <> oldVersion && isScript then
                    invalidateProjectCache cenv oldProject.Id
                    return! tryComputeOptionsByFile cenv document ct
                else
                    return Some(parsingOptions, projectOptions)
        }

    let rec checkProject cenv (oldProject: Project) (newProject: Project) ct =
        async {
            Debug.Assert (oldProject.Id = newProject.Id)

            if hasProjectChanged oldProject newProject then
                Logging.Logging.logInfof "*** invalidate project cache - %A" oldProject.Id
                invalidateProjectCache cenv oldProject.Id
                return false
            elif cenv.enableInMemoryCrossProjectReferences then
                let! oldVersion = oldProject.GetDependentVersionAsync ct |> Async.AwaitTask
                let! newVersion = newProject.GetDependentVersionAsync ct |> Async.AwaitTask

                if oldVersion <> newVersion then
                    // invalidate any project that depends on this project
                    newProject.Solution.GetProjectDependencyGraph().GetProjectsThatTransitivelyDependOnThisProject newProject.Id
                    |> Seq.iter (fun projectId ->
                        Logging.Logging.logInfof "*** Depend on this project - invalidate project cache - %A" projectId
                        invalidateProjectCache cenv projectId
                    )

                    match! tryComputeProjectReferences cenv newProject ct with
                    | None -> 
                        return false
                    | Some referencedProjects ->
                        match cenv.cache.TryGetValue newProject.Id with
                        | true, (_, parsingOptions, projectOptions) ->
                            
                            cenv.cache.[newProject.Id] <- (newProject, parsingOptions, { projectOptions with ReferencedProjects = referencedProjects })
                            return true
                        | _ ->
                            return false
                else
                    return true
            else
                return true
        }

    and tryComputeProjectReferences cenv (project: Project) ct =
        async {
            if cenv.enableInMemoryCrossProjectReferences then
                // Because this code can be kicked off before the hack, HandleCommandLineChanges, occurs,
                //     the command line options will not be available and we should bail if one of the project references does not give us anything.
                let mutable canBail = false

                let referencedProjects = ResizeArray()
                for projectReference in project.ProjectReferences do
                    let referencedProject = project.Solution.GetProject(projectReference.ProjectId)
                    if referencedProject.Language = FSharpConstants.FSharpLanguageName then
                        match! tryComputeOptions cenv referencedProject ct with
                        | None -> canBail <- true
                        | Some(_, projectOptions) -> referencedProjects.Add(referencedProject.OutputFilePath, projectOptions)
                if canBail then
                    return None
                else
                    return Some (referencedProjects.ToArray ())
            else
                return Some [||]
        }
    
    and tryComputeOptions cenv (project: Project) (ct: CancellationToken) =
        async {
            let projectId = project.Id
            match cenv.cache.TryGetValue(projectId) with
            | false, _ ->

                // Because this code can be kicked off before the hack, HandleCommandLineChanges, occurs,
                //     the command line options will not be available and we should bail if one of the project references does not give us anything.
                let mutable canBail = false
            
                let referencedProjects = ResizeArray()

                if cenv.enableInMemoryCrossProjectReferences then
                    for projectReference in project.ProjectReferences do
                        let referencedProject = project.Solution.GetProject(projectReference.ProjectId)
                        if referencedProject.Language = FSharpConstants.FSharpLanguageName then
                            match! tryComputeOptions cenv referencedProject ct with
                            | None -> canBail <- true
                            | Some(_, projectOptions) -> referencedProjects.Add(referencedProject.OutputFilePath, projectOptions)

                if canBail then
                    return None
                else

                let projectSite = 
                    match cenv.workspace with
                    | (:? VisualStudioWorkspace as workspace) ->
                        match workspace.GetHierarchy (projectId) with
                        // Legacy
                        | (:? IProvideProjectSite as provideSite) -> provideSite.GetProjectSite()
                        // Cps
                        | _ ->  mapCpsProjectToSite(project, cenv.cpsCommandLineOptions)
                    | _ -> mapCpsProjectToSite(project, cenv.cpsCommandLineOptions)

                let otherOptions =
                    project.ProjectReferences
                    |> Seq.map (fun x -> "-r:" + project.Solution.GetProject(x.ProjectId).OutputFilePath)
                    |> Array.ofSeq
                    |> Array.append (
                            project.MetadataReferences.OfType<PortableExecutableReference>()
                            |> Seq.map (fun x -> "-r:" + x.FilePath)
                            |> Array.ofSeq
                            |> Array.append (
                                    // Clear any references from CompilationOptions. 
                                    // We get the references from Project.ProjectReferences/Project.MetadataReferences.
                                    projectSite.CompilationOptions
                                    |> Array.filter (fun x -> not (x.Contains("-r:")))
                                )
                        )

                let! version = project.GetDependentVersionAsync ct |> Async.AwaitTask

                let projectOptions =
                    {
                        ProjectFileName = projectSite.ProjectFileName
                        ProjectId = Some(projectId.ToFSharpProjectIdString())
                        SourceFiles = projectSite.CompilationSourceFiles
                        OtherOptions = otherOptions
                        ReferencedProjects = referencedProjects.ToArray()
                        IsIncompleteTypeCheckEnvironment = projectSite.IsIncompleteTypeCheckEnvironment
                        UseScriptResolutionRules = SourceFile.MustBeSingleFileProject (Path.GetFileName(project.FilePath))
                        LoadTime = projectSite.LoadTime
                        UnresolvedReferences = None
                        OriginalLoadReferences = []
                        ExtraProjectInfo= None
                        Stamp = Some(int64 (version.GetHashCode()))
                    }

                // This can happen if we didn't receive the callback from HandleCommandLineChanges yet.
                if Array.isEmpty projectOptions.SourceFiles then
                    return None
                else
                    let checker = cenv.checker
                    
                    Logging.Logging.logInfof "*** computing project - %A version: %A" project.Name project.Version

                    let parsingOptions, _ = checker.GetParsingOptionsFromProjectOptions(projectOptions)

                    setProjectCache cenv project parsingOptions projectOptions

                    return Some(parsingOptions, projectOptions)
  
            | true, (oldProject, parsingOptions, projectOptions) ->
                match! checkProject cenv oldProject project ct with
                | false ->
                    return! tryComputeOptions cenv project ct
                | true ->
                    return Some(parsingOptions, projectOptions)
        }

[<RequireQualifiedAccess>]
type private FSharpProjectOptionsMessage =
    | TryGetOptionsByDocument of Document * AsyncReplyChannel<(FSharpParsingOptions * FSharpProjectOptions) option> * CancellationToken
    | TryGetOptionsByProject of Project * AsyncReplyChannel<(FSharpParsingOptions * FSharpProjectOptions) option> * CancellationToken
    | ClearCaches

[<Sealed>]
type private FSharpProjectOptionsReactor (workspace: VisualStudioWorkspace, settings: EditorOptions, checkerProvider: FSharpCheckerProvider) =
    let cancellationTokenSource = new CancellationTokenSource()

    let cenv = FSharpCompilationHelpers.createCompilationEnv workspace checkerProvider.Checker settings.LanguageServicePerformance.EnableInMemoryCrossProjectReferences

    let isSolutionValid (solution1: Solution) (solution2: Solution) =
        solution1.Id <> solution2.Id || solution1.Version <> solution2.Version

    let loop (agent: MailboxProcessor<FSharpProjectOptionsMessage>) =
        async {
            while true do
                match! agent.Receive() with
                | FSharpProjectOptionsMessage.TryGetOptionsByDocument(document, reply, ct) ->
                    if ct.IsCancellationRequested || isSolutionValid workspace.CurrentSolution document.Project.Solution then
                        reply.Reply None
                    else
                        try
                            // We only allow solutions from the VisualStudioWorkspace.
                            if obj.ReferenceEquals (document.Project.Solution.Workspace, workspace) then
                                FSharpCompilationHelpers.setSolution cenv workspace.CurrentSolution
                                if document.Project.Name = FSharpConstants.FSharpMiscellaneousFilesName then
                                    let! options = FSharpCompilationHelpers.tryComputeOptionsByFile cenv document ct
                                    reply.Reply options
                                else
                                    let! options = FSharpCompilationHelpers.tryComputeOptions cenv document.Project ct
                                    reply.Reply options
                            else
                                reply.Reply None
                        with
                        | _ ->
                            reply.Reply None

                | FSharpProjectOptionsMessage.TryGetOptionsByProject(project, reply, ct) ->
                    if ct.IsCancellationRequested || isSolutionValid workspace.CurrentSolution project.Solution then
                        reply.Reply None
                    else
                        try
                            // We only allow solutions from the VisualStudioWorkspace.
                            // Do not process misc files here.
                            if obj.ReferenceEquals (project.Solution.Workspace, workspace) && not (project.Name = FSharpConstants.FSharpMiscellaneousFilesName) then
                                FSharpCompilationHelpers.setSolution cenv workspace.CurrentSolution
                                let! options = FSharpCompilationHelpers.tryComputeOptions cenv project ct
                                reply.Reply options
                            else
                                reply.Reply None
                        with
                        | _ ->
                            reply.Reply None

                | FSharpProjectOptionsMessage.ClearCaches ->
                    FSharpCompilationHelpers.clearCaches cenv
        }

    let agent = MailboxProcessor.Start((fun agent -> loop agent), cancellationToken = cancellationTokenSource.Token)

    member __.TryGetOptionsByProjectAsync(project, ct) =
        agent.PostAndAsyncReply(fun reply -> FSharpProjectOptionsMessage.TryGetOptionsByProject(project, reply, ct))

    member __.TryGetOptionsByDocumentAsync(document, ct) =
        agent.PostAndAsyncReply(fun reply -> FSharpProjectOptionsMessage.TryGetOptionsByDocument(document, reply, ct))

    member __.SetCpsCommandLineOptions(projectId, sourcePaths, options) =
        cenv.cpsCommandLineOptions.[projectId] <- (sourcePaths, options)

    member __.Reset () =
        cenv.cpsCommandLineOptions.Clear ()
        agent.Post FSharpProjectOptionsMessage.ClearCaches

    member __.TryGetCachedOptionsByProjectId(projectId) =
        match cenv.cache.TryGetValue(projectId) with
        | true, result -> Some(result)
        | _ -> None

    interface IDisposable with
        member __.Dispose() = 
            cancellationTokenSource.Cancel()
            cancellationTokenSource.Dispose() 
            (agent :> IDisposable).Dispose()

/// Exposes FCS FSharpProjectOptions information management as MEF component.
//
// This service allows analyzers to get an appropriate FSharpProjectOptions value for a project or single file.
// It also allows a 'cheaper' route to get the project options relevant to parsing (e.g. the #define values).
// The main entrypoints are TryGetOptionsForDocumentOrProject and TryGetOptionsForEditingDocumentOrProject.
[<Export(typeof<FSharpProjectOptionsManager>); Composition.Shared>]
type internal FSharpProjectOptionsManager
    [<ImportingConstructor>]
    (
        checkerProvider: FSharpCheckerProvider,
        [<Import(typeof<VisualStudioWorkspace>)>] workspace: VisualStudioWorkspaceImpl,
        settings: EditorOptions
    ) =

    let projectDisplayNameOf projectFileName =
        if String.IsNullOrWhiteSpace projectFileName then projectFileName
        else Path.GetFileNameWithoutExtension projectFileName

    let reactor = new FSharpProjectOptionsReactor(workspace, settings, checkerProvider)

    /// Dangerous reset, only call when you close a solution.
    member __.Reset () =
        reactor.Reset ()

    /// Get compilation defines relevant for syntax processing.  
    /// Quicker then TryGetOptionsForDocumentOrProject as it doesn't need to recompute the exact project 
    /// options for a script.
    member this.GetCompilationDefinesForEditingDocument(document:Document) = 
        let parsingOptions =
            match reactor.TryGetCachedOptionsByProjectId(document.Project.Id) with
            | Some (_, parsingOptions, _) -> parsingOptions
            | _ -> { FSharpParsingOptions.Default with IsInteractive = IsScript document.Name }
        CompilerEnvironment.GetCompilationDefinesForEditing parsingOptions     

    member this.TryGetOptionsByProject(project) =
        reactor.TryGetOptionsByProjectAsync(project)

    /// Get the exact options for a document or project
    member this.TryGetOptionsForDocumentOrProject(document: Document, cancellationToken) =
        async { 
            match! reactor.TryGetOptionsByDocumentAsync(document, cancellationToken) with
            | Some(parsingOptions, projectOptions) ->
                return Some(parsingOptions, None, projectOptions)
            | _ ->
                return None
        }

    /// Get the options for a document or project relevant for syntax processing.
    /// Quicker then TryGetOptionsForDocumentOrProject as it doesn't need to recompute the exact project options for a script.
    member this.TryGetOptionsForEditingDocumentOrProject(document:Document, cancellationToken) = 
        async {
            let! result = this.TryGetOptionsForDocumentOrProject(document, cancellationToken) 
            return result |> Option.map(fun (parsingOptions, _, projectOptions) -> parsingOptions, projectOptions)
        }

    [<Export>]
    /// This handles commandline change notifications from the Dotnet Project-system
    /// Prior to VS 15.7 path contained path to project file, post 15.7 contains target binpath
    /// binpath is more accurate because a project file can have multiple in memory projects based on configuration
    member __.HandleCommandLineChanges(path:string, sources:ImmutableArray<CommandLineSourceFile>, _references:ImmutableArray<CommandLineReference>, options:ImmutableArray<string>) =
        use _logBlock = Logger.LogBlock(LogEditorFunctionId.LanguageService_HandleCommandLineArgs)

        let projectId =
            match workspace.ProjectTracker.TryGetProjectByBinPath(path) with
            | true, project -> project.Id
            | false, _ -> workspace.ProjectTracker.GetOrCreateProjectIdForPath(path, projectDisplayNameOf path)
        let project =  workspace.ProjectTracker.GetProject(projectId)
        if project <> null then
            let path = project.ProjectFilePath
            let fullPath p =
                if Path.IsPathRooted(p) || path = null then p
                else Path.Combine(Path.GetDirectoryName(path), p)
            let sourcePaths = sources |> Seq.map(fun s -> fullPath s.Path) |> Seq.toArray

            reactor.SetCpsCommandLineOptions(projectId, sourcePaths, options.ToArray())

    member __.Checker = checkerProvider.Checker