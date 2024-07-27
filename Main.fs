namespace PsychicBarnacle

module Main =

    open System
    open System.IO
    open System.Threading

    open Elmish

    open Avalonia.Controls
    open Avalonia.Platform.Storage

    open Avalonia.FuncUI.DSL
    open Avalonia.FuncUI.Types

    type Item = {
        Id: Guid
        Name: string
        Value: float
        }

    type State = {
        Items: Item []
        }

    type Msg =
        | Todo

    let init (): State * Cmd<Msg> =
        let items =
            Array.init 10 (fun i ->
                {
                    Id = Guid.NewGuid()
                    Name = $"Item {i}"
                    Value = float i
                }
                )
        { Items = items },
        Cmd.none

    let update (window: Window) (msg: Msg) (state: State): State * Cmd<Msg> =
        match msg with
        | Todo ->
            state, Cmd.none

    let view (state: State) (dispatch: Msg -> unit): IView =
        // main dock panel
        DockPanel.create [
            DockPanel.children [
                // left: item selector
                DockPanel.create [
                    DockPanel.dock Dock.Left
                    DockPanel.children [
                        ListBox.create [
                            ListBox.dataItems state.Items
                            ]
                        ]
                    ]
                // right: selected item
                DockPanel.create [
                    DockPanel.children [
                        TextBlock.create [ TextBlock.text "TODO display selected"]
                        ]
                    ]
                ]
            ]