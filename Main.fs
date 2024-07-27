namespace PsychicBarnacle

module Main =

    open System.IO
    open System.Threading

    open Elmish

    open Avalonia.Controls
    open Avalonia.Platform.Storage

    open Avalonia.FuncUI.DSL
    open Avalonia.FuncUI.Types

    type State = {
        Todo: string
        }

    type Msg =
        | Todo

    let init (): State * Cmd<Msg> =
        { Todo = "TODO" }, Cmd.none

    let update (window: Window) (msg: Msg) (state: State): State * Cmd<Msg> =
        match msg with
        | Todo ->
            state, Cmd.none

    let view (state: State) (dispatch: Msg -> unit): IView =
        DockPanel.create [
            DockPanel.children [
                TextBlock.create [
                    TextBlock.text state.Todo
                    ]
                ]
            ]