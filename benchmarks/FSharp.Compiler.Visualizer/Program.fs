open System
open System.IO
open System.Windows
open System.Windows.Controls
open System.Collections.Generic
open System.Collections.Immutable
open System.Threading
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.Text
open FSharp.Compiler.Visualizer

[<Sealed>]
type FrameworkWindow (app: System.Windows.Application, init) as this =
    inherit Window ()

    do this.Content <- init ()

[<Sealed>]
type ViewGlobals(app: Application) =

    member _.Exit() = app.Shutdown()

let keywordColor = Drawing.Color.FromArgb (86, 156, 214)
let stringColor = Drawing.Color.FromArgb (214, 157, 133)

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

    let main vg =

        let mainMenu = mainMenu vg
        DockPanel.SetDock(mainMenu, Windows.Controls.Dock.Top)

        let textEditor = textEditor vg

        let astTreeView = astTreeView vg

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