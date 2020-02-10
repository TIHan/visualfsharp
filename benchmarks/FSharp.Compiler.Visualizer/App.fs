namespace FSharp.Compiler.Visualizer

open System.Windows
open System

[<Sealed>]
type FSharpVisualizerApplication () =
    inherit Application()

    do
        System.Text.Encoding.RegisterProvider(System.Text.CodePagesEncodingProvider.Instance);
        AppDomain.CurrentDomain.UnhandledException.Add (fun _ -> ())