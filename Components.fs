namespace PsychicBarnacle

module Components =

    open Avalonia.Controls
    open Avalonia.Layout

    open Avalonia.FuncUI
    open Avalonia.FuncUI.DSL
    open Avalonia.FuncUI.Types

    let greetingView (): IView =
        Component.create ("greetingView", fun ctx ->
            let state = ctx.useState 0
            StackPanel.create [
                StackPanel.children [
                    TextBlock.create [
                        TextBlock.text $"{state.Current}"
                        ]
                    Button.create [
                        Button.content "Inc"
                        Button.onClick (fun _ -> state.Set (state.Current + 1))
                        ]
                    Button.create [
                        Button.content "Dec"
                        Button.onClick (fun _ -> state.Set (state.Current - 1))
                        ]
                    ]
                ]
            )

    let view (): IView =
        Component.create ("mainView", fun ctx ->
            DockPanel.create [
                DockPanel.children [
                    // use other component
                    greetingView ()
                    ]
                ]
            )