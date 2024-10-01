namespace PsychicBarnacle

module AsyncOperation =

    open System

    open Elmish

    open Avalonia.Controls
    open Avalonia.Layout

    open Avalonia.FuncUI
    open Avalonia.FuncUI.DSL
    open Avalonia.FuncUI.Types

    type State = {
        Number: int
        }

    type Msg =
        | Increment
        | Decrement

    let init (): State * Cmd<Msg> =
        {
            Number = 0
        },
        Cmd.none

    let update (msg: Msg) (state: State): State * Cmd<Msg> =
        match msg with
        | Increment ->
            { state with Number = state.Number + 1 },
            Cmd.none
        | Decrement ->
            { state with Number = state.Number - 1 },
            Cmd.none

    let view (state: State) (dispatch: Msg -> unit): IView =
        // main dock panel
        DockPanel.create [
            DockPanel.children [
                StackPanel.create [
                    StackPanel.children [
                        TextBlock.create [
                            TextBlock.text $"{state.Number}"
                            ]
                        Button.create [
                            Button.content "Inc"
                            Button.onClick (fun _ -> Increment |> dispatch)
                            ]
                        Button.create [
                            Button.content "Dec"
                            Button.onClick (fun _ -> Decrement |> dispatch)
                            ]
                        ]
                    ]
                ]
            ]