namespace PsychicBarnacle

open System.Reflection

open Avalonia
open Avalonia.Themes.Fluent
open Avalonia.Controls.ApplicationLifetimes

open Elmish

open Avalonia.FuncUI
open Avalonia.FuncUI.Hosts
open Avalonia.FuncUI.Elmish

module Shell =

    type MainWindow() as this =
        inherit HostWindow()
        do
            let version =
                Assembly
                    .GetExecutingAssembly()
                    .GetName()
                    .Version
                    .ToString()

            base.Title <- $"Application {version}"

            base.Width <- 1200.0
            base.Height <- 800.0
            base.MinWidth <- 1200.0
            base.MinHeight <- 800.0

            Elmish.Program.mkProgram Main.init (Main.update this) Main.view
            |> Program.withHost this
            |> Program.runWithAvaloniaSyncDispatch ()
            // |> Program.run

type App() =
    inherit Application()

    override this.Initialize() =
        this.Styles.Add (FluentTheme())
        this.RequestedThemeVariant <- Styling.ThemeVariant.Light

    override this.OnFrameworkInitializationCompleted() =
        match this.ApplicationLifetime with
        | :? IClassicDesktopStyleApplicationLifetime as desktopLifetime ->
            desktopLifetime.MainWindow <- Shell.MainWindow()
        | _ -> ()

module Program =

    [<EntryPoint>]
    let main (args: string []) =
        AppBuilder
            .Configure<App>()
            .UsePlatformDetect()
            .UseSkia()
            .StartWithClassicDesktopLifetime(args)