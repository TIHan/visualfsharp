open System
open System.IO
open System.Reflection
open System.Windows
open System.Windows.Controls
open System.Collections.Generic
open System.Collections.Immutable
open System.Threading
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.Text
open FSharp.Compiler.Range
open FSharp.Compiler.Text
open FSharp.Compiler.Visualizer
open FSharp.Compiler.Ast
open FSharp.Compiler.SourceCodeServices

let createProject name referencedProjects =
    let tmpPath = Path.GetTempPath()
    let file = Path.Combine(tmpPath, Path.ChangeExtension(name, ".fs"))

    {
        ProjectFileName = Path.Combine(tmpPath, Path.ChangeExtension(name, ".dll"))
        ProjectId = None
        SourceFiles = [|file|]
        OtherOptions = 
            Array.append [|"--optimize+"; "--target:exe"; "--targetprofile:mscorlib" |] (referencedProjects |> Array.ofList |> Array.map (fun x -> "-r:" + x.ProjectFileName))
        ReferencedProjects =
            referencedProjects
            |> List.map (fun x -> (x.ProjectFileName, x))
            |> Array.ofList
        IsIncompleteTypeCheckEnvironment = false
        UseScriptResolutionRules = false
        LoadTime = DateTime()
        UnresolvedReferences = None
        OriginalLoadReferences = []
        ExtraProjectInfo = None
        Stamp = None
    }

module private SourceText =

    open System.Runtime.CompilerServices

    let weakTable = ConditionalWeakTable<SourceText, ISourceText>()

    let create (sourceText: SourceText) =

        let sourceText =
            { new ISourceText with
            
                member __.Item with get index = sourceText.[index]

                member __.GetLineString(lineIndex) =
                    sourceText.Lines.[lineIndex].ToString()

                member __.GetLineCount() =
                    sourceText.Lines.Count

                member __.GetLastCharacterPosition() =
                    if sourceText.Lines.Count > 0 then
                        (sourceText.Lines.Count, sourceText.Lines.[sourceText.Lines.Count - 1].Span.Length)
                    else
                        (0, 0)

                member __.GetSubTextString(start, length) =
                    sourceText.GetSubText(TextSpan(start, length)).ToString()

                member __.SubTextEquals(target, startIndex) =
                    if startIndex < 0 || startIndex >= sourceText.Length then
                        raise (ArgumentOutOfRangeException("startIndex"))

                    if String.IsNullOrEmpty(target) then
                        raise (ArgumentException("Target is null or empty.", "target"))

                    let lastIndex = startIndex + target.Length
                    if lastIndex <= startIndex || lastIndex >= sourceText.Length then
                        raise (ArgumentException("Target is too big.", "target"))

                    let mutable finished = false
                    let mutable didEqual = true
                    let mutable i = 0
                    while not finished && i < target.Length do
                        if target.[i] <> sourceText.[startIndex + i] then
                            didEqual <- false
                            finished <- true // bail out early                        
                        else
                            i <- i + 1

                    didEqual

                member __.ContentEquals(sourceText) =
                    match sourceText with
                    | :? SourceText as sourceText -> sourceText.ContentEquals(sourceText)
                    | _ -> false

                member __.Length = sourceText.Length

                member __.CopyTo(sourceIndex, destination, destinationIndex, count) =
                    sourceText.CopyTo(sourceIndex, destination, destinationIndex, count)
            }

        sourceText

type SourceText with

    member this.ToFSharpSourceText() =
        SourceText.weakTable.GetValue(this, Runtime.CompilerServices.ConditionalWeakTable<_,_>.CreateValueCallback(SourceText.create))

[<Sealed>]
type FrameworkWindow (app: System.Windows.Application, init) as this =
    inherit Window ()

    do this.Content <- init ()

[<Sealed>]
type ViewGlobals(app: Application) =

    member _.Exit() = app.Shutdown()

let keywordColor = Drawing.Color.FromArgb (86, 156, 214)
let stringColor = Drawing.Color.FromArgb (214, 157, 133)
let highlight = Drawing.Color.Green

module View =

    let setHighlightSpans highlightSpans (textEditor: FSharpTextEditor) =
        textEditor.ClearAllTextSpanColors ()
        highlightSpans
        |> List.iter (fun (HighlightSpan (span, color, kind)) ->
            if textEditor.SourceText.Length >= span.End && not (span.Length = 0) then
                let linePosSpan = textEditor.SourceText.Lines.GetLinePositionSpan span
                let startLine = linePosSpan.Start.Line
                let endLine = linePosSpan.End.Line

                for lineNumber = startLine to endLine do

                    let line = textEditor.SourceText.Lines.[lineNumber]

                    let start =
                        if startLine = lineNumber then
                            span.Start
                        else
                            line.Start

                    let length =
                        if endLine = lineNumber then
                            if startLine = lineNumber then
                                span.Length
                            else
                                span.End - line.Start
                        else
                            line.End - span.Start

                    let textSpanColors = textEditor.GetTextSpanColors (lineNumber + 1)
                    textSpanColors.Add (HighlightSpan (TextSpan(start, length), color, kind)))
        textEditor.Redraw ()

    let textEditor (_vg: ViewGlobals) =
        let textEditor = FSharpTextEditor ()
        textEditor.WordWrap <- true
        textEditor.Width <- 1080.
        textEditor

    let astTreeView (_vg: ViewGlobals) =
        let treeView = TreeView()
        treeView

    let exitMenuItem (vg: ViewGlobals) =
        let menuItem = MenuItem()
        menuItem.Header <- "_Exit"
        menuItem.Click.Add(fun _ -> vg.Exit())
        menuItem

    let fileMenuItem (vg: ViewGlobals) =
        let menuItem = MenuItem()
        menuItem.Header <- "_File"
        menuItem.Items.Add(exitMenuItem vg) |> ignore
        menuItem

    let mainMenu vg =
        let menu = Menu()
        menu.Items.Add(fileMenuItem vg) |> ignore
        menu

    let rec typeTreeItem onClick preHeader (visited: HashSet<Type * obj>) (inst: obj) =
        if inst <> null && visited.Add (inst.GetType(), inst) then
            let itemType = inst.GetType()
            let treeViewItem = TreeViewItem()
            treeViewItem.Selected.Add(fun _ -> if treeViewItem.IsSelected then onClick inst)
            treeViewItem.Header <- preHeader + ": " + itemType.Name
            itemType.GetFields (BindingFlags.Instance ||| BindingFlags.NonPublic ||| BindingFlags.Public)
            |> Seq.iter (fun field ->              
                let treeViewItemChild = 
                    if field.FieldType.IsPrimitive || field.FieldType = typeof<string> || field.FieldType = typeof<range> then
                        let inst = field.GetValue inst
                        let child = TreeViewItem(Header = field.Name + ": " + string inst)
                        child.Selected.Add(fun _ -> if child.IsSelected then onClick inst)
                        child
                    else
                        typeTreeItem onClick field.Name visited (field.GetValue inst)
                treeViewItem.Items.Add(treeViewItemChild) |> ignore)
            treeViewItem
        else
            TreeViewItem(Header = "null")

    let main vg =
        let checker = FSharpChecker.Create(keepAssemblyContents = true)
        let mainMenu = mainMenu vg
        DockPanel.SetDock(mainMenu, Dock.Top)

        let textEditor = textEditor vg

        let astTreeView = astTreeView vg

        let rangeToTextSpan (m: range) =
            let lines = textEditor.SourceText.Lines
            let startLine = lines.[m.StartLine - 1]
            let endLine = lines.[m.EndLine - 1]
            let start = startLine.Start + m.StartColumn
            TextSpan(start, (endLine.Start + m.EndColumn) - start)

        let highlightRange (m: range) =
            let textSpan = rangeToTextSpan m
            HighlightSpan(textSpan, highlight, HighlightSpanKind.Background)

        let onClick = fun (inst: obj) ->
            let itemType = inst.GetType()
            if itemType = typeof<range> then
                setHighlightSpans ([highlightRange (unbox inst)]) textEditor
            else
                let fieldOpt =
                    itemType.GetFields(BindingFlags.Instance ||| BindingFlags.NonPublic ||| BindingFlags.Public)
                    |> Seq.tryFind (fun field -> field.FieldType = typeof<range>)

                fieldOpt
                |> Option.iter (fun field -> 
                    setHighlightSpans ([highlightRange (unbox (field.GetValue inst))]) textEditor)

        let mutable cts = new CancellationTokenSource()
        textEditor.SourceTextChanged.Add(fun (sourceText, caretOffset, willCompletionTrigger) ->
            cts.Cancel()
            cts.Dispose()
            cts <- new CancellationTokenSource()
            astTreeView.Items.Clear()
            let computation =
                async {
                    do! Async.Sleep 200
                    let options = createProject "file1" []
                    let! _, answer = checker.ParseAndCheckFileInProject(options.SourceFiles.[0], 0, sourceText.ToFSharpSourceText(), options)
                    
                    match answer with
                    | FSharpCheckFileAnswer.Succeeded results -> 
                        let tree = results.ImplementationFile.Value
                        astTreeView.Items.Add(typeTreeItem onClick "TAST" (HashSet()) tree) |> ignore
                    | _ -> ()
                }
            Async.StartImmediate(computation, cancellationToken = cts.Token))

        let dockPanel = DockPanel()
        dockPanel.Children.Add mainMenu |> ignore
        dockPanel.Children.Add textEditor |> ignore
        dockPanel.Children.Add astTreeView |> ignore
        dockPanel

[<EntryPoint;STAThread>]
let main argv =
    let app = FSharpVisualizerApplication()
    let vg = ViewGlobals app
    app.Run(FrameworkWindow(app, fun () -> View.main vg))