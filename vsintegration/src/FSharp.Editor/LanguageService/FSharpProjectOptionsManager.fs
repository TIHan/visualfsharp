// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

namespace Microsoft.VisualStudio.FSharp.Editor

open System
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

    let mapCpsProjectToSite(workspace:VisualStudioWorkspaceImpl, project:Project, serviceProvider:System.IServiceProvider, cpsCommandLineOptions: IDictionary<ProjectId, string[] * string[]>) =
        let hier = workspace.GetHierarchy(project.Id)
        let sourcePaths, referencePaths, options =
            match cpsCommandLineOptions.TryGetValue(project.Id) with
            | true, (sourcePaths, options) -> sourcePaths, [||], options
            | false, _ -> [||], [||], [||]
        {
            new IProvideProjectSite with
                member x.GetProjectSite() =
                    let mutable errorReporter = 
                        let reporter = ProjectExternalErrorReporter(project.Id, "FS", serviceProvider)
                        Some(reporter:> IVsLanguageServiceBuildErrorReporter2)

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
                            member __.ProjectProvider = Some (x)
                            member __.BuildErrorReporter with get () = errorReporter and set (v) = errorReporter <- v
                    }
            interface IVsHierarchy with
                member __.SetSite(psp) = hier.SetSite(psp)
                member __.GetSite(psp) = hier.GetSite(ref psp)
                member __.QueryClose(pfCanClose)= hier.QueryClose(ref pfCanClose)
                member __.Close() = hier.Close()
                member __.GetGuidProperty(itemid, propid, pguid) = hier.GetGuidProperty(itemid, propid, ref pguid)
                member __.SetGuidProperty(itemid, propid, rguid) = hier.SetGuidProperty(itemid, propid, ref rguid)
                member __.GetProperty(itemid, propid, pvar) = hier.GetProperty(itemid, propid, ref pvar) 
                member __.SetProperty(itemid, propid, var)  = hier.SetProperty(itemid, propid, var)
                member __.GetNestedHierarchy(itemid, iidHierarchyNested, ppHierarchyNested, pitemidNested) = 
                    hier.GetNestedHierarchy(itemid, ref iidHierarchyNested, ref ppHierarchyNested, ref pitemidNested)
                member __.GetCanonicalName(itemid, pbstrName) = hier.GetCanonicalName(itemid, ref pbstrName)
                member __.ParseCanonicalName(pszName, pitemid) = hier.ParseCanonicalName(pszName, ref pitemid)
                member __.Unused0() = hier.Unused0()
                member __.AdviseHierarchyEvents(pEventSink, pdwCookie) = hier.AdviseHierarchyEvents(pEventSink, ref pdwCookie)
                member __.UnadviseHierarchyEvents(dwCookie) = hier.UnadviseHierarchyEvents(dwCookie)
                member __.Unused1() = hier.Unused1()
                member __.Unused2() = hier.Unused2()
                member __.Unused3() = hier.Unused3()
                member __.Unused4() = hier.Unused4()
        }

    [<RequireQualifiedAccess>]
    type ProjectChanged =
        | None
        | Hard // added, removed, or newer project references
        | Soft // project references had its files modified

[<RequireQualifiedAccess>]
type private FSharpProjectOptionsMessage =
    | TryGetOptionsByDocument of Document * AsyncReplyChannel<(FSharpParsingOptions * FSharpProjectOptions) option> * CancellationToken
    | TryGetOptionsByProject of Project * AsyncReplyChannel<(FSharpParsingOptions * FSharpProjectOptions) option> * CancellationToken
    | ClearOptions of ProjectId
    | ClearSingleFileOptionsCache of DocumentId

[<Sealed>]
type private FSharpProjectOptionsReactor (workspace: VisualStudioWorkspaceImpl, settings: EditorOptions, serviceProvider, checkerProvider: FSharpCheckerProvider) =
    let cancellationTokenSource = new CancellationTokenSource()

    // Hack to store command line options from HandleCommandLineChanges
    let cpsCommandLineOptions = new ConcurrentDictionary<ProjectId, string[] * string[]>()

    let cache = Dictionary<ProjectId, Project * FSharpParsingOptions * FSharpProjectOptions>()
    let singleFileCache = Dictionary<DocumentId, VersionStamp * DateTime * FSharpParsingOptions * FSharpProjectOptions>()

    let rec tryComputeOptionsByFile (document: Document) cancellationToken (loadTime: DateTime option) =
        async {
            let! fileStamp = document.GetTextVersionAsync(cancellationToken) |> Async.AwaitTask
            match singleFileCache.TryGetValue(document.Id) with
            | false, _ ->
                let! sourceText = document.GetTextAsync(cancellationToken) |> Async.AwaitTask
                let loadTime = defaultArg loadTime DateTime.Now
                let! scriptProjectOptions, _ = checkerProvider.Checker.GetProjectOptionsFromScript(document.FilePath, sourceText.ToFSharpSourceText(), loadTime)
                let projectOptions =
                    if isScriptFile document.FilePath then
                        scriptProjectOptions
                    else
                        {
                            ProjectFileName = document.FilePath
                            ProjectId = None
                            SourceFiles = [|document.FilePath|]
                            OtherOptions = [||]
                            ReferencedProjects = []
                            IsIncompleteTypeCheckEnvironment = false
                            UseScriptResolutionRules = SourceFile.MustBeSingleFileProject (Path.GetFileName(document.FilePath))
                            LoadTime = DateTime.Now
                            UnresolvedReferences = None
                            OriginalLoadReferences = []
                            ExtraProjectInfo= None
                            Stamp = Some(int64 (fileStamp.GetHashCode()))
                            DependentStamp = None
                        }

                cancellationToken.ThrowIfCancellationRequested()

                checkerProvider.Checker.CheckProjectInBackground(projectOptions, userOpName="checkOptions")

                let parsingOptions, _ = checkerProvider.Checker.GetParsingOptionsFromProjectOptions(projectOptions)

                singleFileCache.[document.Id] <- (fileStamp, loadTime, parsingOptions, projectOptions)

                return Some(parsingOptions, projectOptions)

            | true, (fileStamp2, loadTime2, parsingOptions, projectOptions) ->
                if fileStamp <> fileStamp2 then
                    singleFileCache.Remove(document.Id) |> ignore
                    return! tryComputeOptionsByFile document cancellationToken  (Some loadTime2)
                else
                    return Some(parsingOptions, projectOptions)
        }

    let rec tryComputeFSharpReferencedProjects (project: Project) (cancellationToken: CancellationToken) =
        async {
            if settings.LanguageServicePerformance.EnableInMemoryCrossProjectReferences then
                // Because this code can be kicked off before the hack, HandleCommandLineChanges, occurs,
                //     the command line options will not be available and we should bail if one of the project references does not give us anything.
                let mutable canBail = false

                let referencedProjects = ResizeArray ()
                for projectReference in project.ProjectReferences do
                    let referencedProject = project.Solution.GetProject(projectReference.ProjectId)
                    if referencedProject.Language = FSharpConstants.FSharpLanguageName then
                        match! tryComputeOptions referencedProject cancellationToken with
                        | None -> 
                            canBail <- true
                        | Some(_, projectOptions) -> 
                            referencedProjects.Add { DllPath = referencedProject.OutputFilePath; ProjectOptions = projectOptions }

                if canBail then
                    return None
                else
                    return
                        referencedProjects
                        |> List.ofSeq
                        |> Some
            else
                return Some []
        }

    // Fully invalidate a project in the compiler service; will rebuild an incremental builder.
    and invalidateOptions (oldProject: Project) (project: Project) (cancellationToken: CancellationToken) =
        async {
            cache.Remove oldProject.Id |> ignore
            return! tryComputeOptions project cancellationToken
        }

    and updateOptions (oldProject: Project) (project: Project) (parsingOptions: FSharpParsingOptions) (projectOptions: FSharpProjectOptions) (cancellationToken: CancellationToken) =
        async {
            System.Diagnostics.Debug.Assert (oldProject.Id = project.Id, "OldProject and Project do not have the same ProjectId.")

            // Something major in the project has changed: added/removed project references, added/removed/changed metadata references, modified command line options, order-changed/new/removed documents.
            // We need to rebuild everything.
            if oldProject.Version <> project.Version then
                return! invalidateOptions oldProject project cancellationToken
            else
                // This path is to update the dependent stamp and referenced projects. 
                // Referenced projects could have changed but not affect the project iself, so it's a minor update, such as a file changing in a project reference.
                match! tryComputeFSharpReferencedProjects project cancellationToken with
                | None ->
                    // We have invalid project references. Invalidate project.
                    return! invalidateOptions oldProject project cancellationToken
                | Some referencedProjects ->
                    let! projectDependentVersion = project.GetDependentVersionAsync cancellationToken |> Async.AwaitTask

                    let dependentStamp = Some (int64 (projectDependentVersion.GetHashCode ()))

                    let projectOptions = { projectOptions with ReferencedProjects = referencedProjects; DependentStamp = dependentStamp }
                    cache.[project.Id] <- (project, parsingOptions, projectOptions)
                    return Some (parsingOptions, projectOptions)
        }

    and tryComputeOptions (project: Project) (cancellationToken: CancellationToken) =
        async {
            let projectId = project.Id
            match cache.TryGetValue(projectId) with
            | false, _ ->

                match! tryComputeFSharpReferencedProjects project cancellationToken with
                | None -> return None
                | Some fsharpReferencedProjects ->

                    let hier = workspace.GetHierarchy(projectId)
                    let projectSite = 
                        match hier with
                        // Legacy
                        | (:? IProvideProjectSite as provideSite) -> provideSite.GetProjectSite()
                        // Cps
                        | _ -> 
                            let provideSite = mapCpsProjectToSite(workspace, project, serviceProvider, cpsCommandLineOptions)
                            provideSite.GetProjectSite()

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

                    let! dependentVersion = project.GetDependentVersionAsync cancellationToken |> Async.AwaitTask
                    let dependentStamp = Some (int64 (dependentVersion.GetHashCode ()))

                    let projectOptions =
                        {
                            ProjectFileName = projectSite.ProjectFileName
                            ProjectId = Some(projectId.ToFSharpProjectIdString())
                            SourceFiles = projectSite.CompilationSourceFiles
                            OtherOptions = otherOptions
                            ReferencedProjects = fsharpReferencedProjects
                            IsIncompleteTypeCheckEnvironment = projectSite.IsIncompleteTypeCheckEnvironment
                            UseScriptResolutionRules = SourceFile.MustBeSingleFileProject (Path.GetFileName(project.FilePath))
                            LoadTime = projectSite.LoadTime
                            UnresolvedReferences = None
                            OriginalLoadReferences = []
                            ExtraProjectInfo= None
                            Stamp = Some(int64 (project.Version.GetHashCode()))
                            DependentStamp = dependentStamp
                        }

                    cancellationToken.ThrowIfCancellationRequested()

                    // This can happen if we didn't receive the callback from HandleCommandLineChanges yet.
                    if Array.isEmpty projectOptions.SourceFiles then
                        return None
                    else
                        checkerProvider.Checker.InvalidateConfiguration(projectOptions, startBackgroundCompileIfAlreadySeen = false, userOpName = "computeOptions")

                        let parsingOptions, _ = checkerProvider.Checker.GetParsingOptionsFromProjectOptions(projectOptions)

                        cache.[projectId] <- (project, parsingOptions, projectOptions)

                        return Some(parsingOptions, projectOptions)
  
            | true, (oldProject, parsingOptions, projectOptions) ->
                let! oldDependentVersion = oldProject.GetDependentVersionAsync cancellationToken |> Async.AwaitTask
                let! dependentVersion = project.GetDependentVersionAsync cancellationToken |> Async.AwaitTask

                // If something changed, update the project options
                if oldDependentVersion <> dependentVersion then
                    return! updateOptions oldProject project parsingOptions projectOptions cancellationToken
                else                 
                    return Some (parsingOptions, projectOptions)
        }

    let loop (agent: MailboxProcessor<FSharpProjectOptionsMessage>) =
        async {
            while true do
                match! agent.Receive() with
                | FSharpProjectOptionsMessage.TryGetOptionsByDocument(document, reply, cancellationToken) ->
                    try
                        // For now, disallow miscellaneous workspace since we are using the hacky F# miscellaneous files project.
                        if document.Project.Solution.Workspace.Kind = WorkspaceKind.MiscellaneousFiles then
                            reply.Reply(None)
                        elif document.Project.Name = FSharpConstants.FSharpMiscellaneousFilesName then
                            let! options = tryComputeOptionsByFile document cancellationToken None
                            reply.Reply(options)
                        else
                            let! options = tryComputeOptions document.Project cancellationToken
                            reply.Reply(options)
                    with
                    | _ ->
                        reply.Reply(None)
                | FSharpProjectOptionsMessage.TryGetOptionsByProject(project, reply, cancellationToken) ->
                    try
                        if project.Solution.Workspace.Kind = WorkspaceKind.MiscellaneousFiles || project.Name = FSharpConstants.FSharpMiscellaneousFilesName then
                            reply.Reply(None)
                        else
                            let! options = tryComputeOptions project cancellationToken
                            reply.Reply(options)
                    with
                    | _ ->
                        reply.Reply(None)
                | FSharpProjectOptionsMessage.ClearOptions(projectId) ->
                    cache.Remove(projectId) |> ignore
                | FSharpProjectOptionsMessage.ClearSingleFileOptionsCache(documentId) ->
                    singleFileCache.Remove(documentId) |> ignore
        }

    let agent = MailboxProcessor.Start((fun agent -> loop agent), cancellationToken = cancellationTokenSource.Token)

    member __.TryGetOptionsByProjectAsync(project, cancellationToken) =
        agent.PostAndAsyncReply(fun reply -> FSharpProjectOptionsMessage.TryGetOptionsByProject(project, reply, cancellationToken))

    member __.TryGetOptionsByDocumentAsync(document, cancellationToken) =
        agent.PostAndAsyncReply(fun reply -> FSharpProjectOptionsMessage.TryGetOptionsByDocument(document, reply, cancellationToken))

    member __.ClearOptionsByProjectId(projectId) =
        agent.Post(FSharpProjectOptionsMessage.ClearOptions(projectId))

    member __.ClearSingleFileOptionsCache(documentId) =
        agent.Post(FSharpProjectOptionsMessage.ClearSingleFileOptionsCache(documentId))

    member __.SetCpsCommandLineOptions(projectId, sourcePaths, options) =
        cpsCommandLineOptions.[projectId] <- (sourcePaths, options)

    member __.TryGetCachedOptionsByProjectId(projectId) =
        match cache.TryGetValue(projectId) with
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
        [<Import(typeof<SVsServiceProvider>)>] serviceProvider: System.IServiceProvider,
        settings: EditorOptions
    ) =

    let checker = checkerProvider.Checker

    let projectDisplayNameOf projectFileName =
        if String.IsNullOrWhiteSpace projectFileName then projectFileName
        else Path.GetFileNameWithoutExtension projectFileName

    let reactor = new FSharpProjectOptionsReactor(workspace, settings, serviceProvider, checkerProvider)

    do
        // We need to listen to this event for lifecycle purposes.
        workspace.WorkspaceChanged.Add(fun args ->
            match args.Kind with
            | WorkspaceChangeKind.ProjectRemoved -> reactor.ClearOptionsByProjectId(args.ProjectId)
            | _ -> ()
        )

    /// Clear a project from the project table
    member this.ClearInfoForProject(projectId:ProjectId) = 
        reactor.ClearOptionsByProjectId(projectId)

    member this.ClearSingleFileOptionsCache(documentId) =
        reactor.ClearSingleFileOptionsCache(documentId)

    /// Get compilation defines relevant for syntax processing.  
    /// Quicker then TryGetOptionsForDocumentOrProject as it doesn't need to recompute the exact project 
    /// options for a script.
    member this.GetCompilationDefines(document:Document) = 
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
        let path = project.ProjectFilePath
        let fullPath p =
            if Path.IsPathRooted(p) || path = null then p
            else Path.Combine(Path.GetDirectoryName(path), p)
        let sourcePaths = sources |> Seq.map(fun s -> fullPath s.Path) |> Seq.toArray

        reactor.SetCpsCommandLineOptions(projectId, sourcePaths, options.ToArray())

    member this.TryParseAndCheckDocument(document: Document, cancellationToken) =
        async {
            let languageServicePerformanceOptions = settings.LanguageServicePerformance
            let filePath = document.FilePath
            let! sourceText = document.GetTextAsync cancellationToken |> Async.AwaitTask
            let! textVersion = document.GetTextVersionAsync cancellationToken |> Async.AwaitTask
            let textVersionHash = textVersion.GetHashCode ()
            match! this.TryGetOptionsByProject (document.Project, cancellationToken) with
            | None -> return None
            | Some (_, options) ->
                let parseAndCheckFile =
                    async {
                        let! parseResults, checkFileAnswer = checker.ParseAndCheckFileInProject(filePath, textVersionHash, sourceText.ToFSharpSourceText(), options)
                        return
                            match checkFileAnswer with
                            | FSharpCheckFileAnswer.Aborted -> 
                                None
                            | FSharpCheckFileAnswer.Succeeded(checkFileResults) ->
                                Some (parseResults, checkFileResults)
                    }

                let tryGetFreshResultsWithTimeout() =
                    async {
                        let! worker = Async.StartChild(parseAndCheckFile, millisecondsTimeout=languageServicePerformanceOptions.TimeUntilStaleCompletion)
                        try
                            return! worker
                        with :? TimeoutException ->
                            return None // worker is cancelled at this point, we cannot return it and wait its completion anymore
                    }

                let bindParsedInput(results: (FSharpParseFileResults * FSharpCheckFileResults) option) =
                    match results with
                    | Some(parseResults, checkResults) ->
                        match parseResults.ParseTree with
                        | Some parsedInput -> Some (parseResults, parsedInput, checkResults, sourceText)
                        | None -> None
                    | None -> None

                if languageServicePerformanceOptions.AllowStaleCompletionResults then
                    let! freshResults = tryGetFreshResultsWithTimeout()
                    
                    let! results =
                        match freshResults with
                        | Some x -> async.Return (Some x)
                        | None ->
                            async {
                                match checker.TryGetRecentCheckResultsForFile(filePath, options) with
                                | Some (parseResults, checkFileResults, _) ->
                                    return Some (parseResults, checkFileResults)
                                | None ->
                                    return! parseAndCheckFile
                            }
                    return bindParsedInput results
                else 
                    let! results = parseAndCheckFile
                    return bindParsedInput results
        }

        member this.GetMatchingBraces (document: Document, cancellationToken) =
            async {
                match! this.TryGetOptionsByProject (document.Project, cancellationToken) with
                | None -> return [||]
                | Some (parsingOptions, _) ->
                    let! sourceText = document.GetTextAsync cancellationToken |> Async.AwaitTask
                    return! checker.MatchBraces(document.FilePath, sourceText.ToFSharpSourceText(), parsingOptions)
            }

        member this.GetAllUsesOfAllSymbolsInSourceString (document: Document, checkForUnusedOpens, cancellationToken) = async {
            
            match! this.TryParseAndCheckDocument (document, cancellationToken) with
            | None -> return [||]
            | Some(_, _,checkResults, _) ->
                let! fsharpSymbolsUses = checkResults.GetAllUsesOfAllSymbolsInFile()
                let allSymbolsUses =
                  fsharpSymbolsUses
                  |> Array.map (fun symbolUse -> 
                      let fullNames = 
                          match symbolUse.Symbol with
                          // Make sure that unsafe manipulation isn't executed if unused opens are disabled
                          | _ when not checkForUnusedOpens -> None
                          | Symbol.MemberFunctionOrValue func when func.IsExtensionMember ->
                              if func.IsProperty then
                                  let fullNames =
                                      [|  if func.HasGetterMethod then
                                              match func.GetterMethod.DeclaringEntity with 
                                              | Some e -> yield e.TryGetFullName()
                                              | None -> ()
                                          if func.HasSetterMethod then
                                              match func.SetterMethod.DeclaringEntity with 
                                              | Some e -> yield e.TryGetFullName()
                                              | None -> ()
                                      |]
                                      |> Array.choose id
                                  match fullNames with
                                  | [||]  -> None 
                                  | _     -> Some fullNames
                              else 
                                  match func.DeclaringEntity with
                                  // C# extension method
                                  | Some (Symbol.FSharpEntity Symbol.Class) ->
                                      let fullName = symbolUse.Symbol.FullName.Split '.'
                                      if fullName.Length > 2 then
                                          (* For C# extension methods FCS returns full name including the class name, like:
                                              Namespace.StaticClass.ExtensionMethod
                                              So, in order to properly detect that "open Namespace" actually opens ExtensionMethod,
                                              we remove "StaticClass" part. This makes C# extension methods looks identically 
                                              with F# extension members.
                                          *)
                                          let fullNameWithoutClassName =
                                              Array.append fullName.[0..fullName.Length - 3] fullName.[fullName.Length - 1..]
                                          Some [|String.Join (".", fullNameWithoutClassName)|]
                                      else None
                                  | _ -> None
                          // Operators
                          | Symbol.MemberFunctionOrValue func ->
                              match func with
                              | Symbol.Constructor _ ->
                                  // full name of a constructor looks like "UnusedSymbolClassifierTests.PrivateClass.( .ctor )"
                                  // to make well formed full name parts we cut "( .ctor )" from the tail.
                                  let fullName = func.FullName
                                  let ctorSuffix = ".( .ctor )"
                                  let fullName =
                                      if fullName.EndsWith ctorSuffix then 
                                          fullName.[0..fullName.Length - ctorSuffix.Length - 1]
                                      else fullName
                                  Some [| fullName |]
                              | _ -> 
                                  Some [| yield func.FullName 
                                          match func.TryGetFullCompiledOperatorNameIdents() with
                                          | Some idents -> yield String.concat "." idents
                                          | None -> ()
                                      |]
                          | Symbol.FSharpEntity e ->
                              match e with
                              | e, Symbol.Attribute, _ ->
                                  e.TryGetFullName ()
                                  |> Option.map (fun fullName ->
                                      [| fullName; fullName.Substring(0, fullName.Length - "Attribute".Length) |])
                              | e, _, _ -> 
                                  e.TryGetFullName () |> Option.map (fun fullName -> [| fullName |])
                          | Symbol.RecordField _
                          | Symbol.UnionCase _ as symbol ->
                              Some [| let fullName = symbol.FullName
                                      yield fullName
                                      let idents = fullName.Split '.'
                                      // Union cases/Record fields can be accessible without mentioning the enclosing type. 
                                      // So we add a FullName without having the type part.
                                      if idents.Length > 1 then
                                          yield String.Join (".", Array.append idents.[0..idents.Length - 3] idents.[idents.Length - 1..])
                                  |]
                          |  _ -> None
                          |> Option.defaultValue [|symbolUse.Symbol.FullName|]
                          |> Array.map (fun fullName -> fullName.Split '.')
            
                      {   SymbolUse = symbolUse
                          IsUsed = true
                          FullNames = fullNames 
                      })
                return allSymbolsUses 
            }
