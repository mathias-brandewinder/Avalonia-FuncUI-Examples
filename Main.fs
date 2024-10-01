namespace PsychicBarnacle

module Main =

    open System

    open Elmish

    open Avalonia.Controls
    open Avalonia.Layout

    open Avalonia.FuncUI
    open Avalonia.FuncUI.DSL
    open Avalonia.FuncUI.Types

    type State = {
        ListSelection: ListSelection.State
        }

    type Msg =
        | ListSelection of ListSelection.Msg

    let init (): State * Cmd<Msg> =
        let state, _ = ListSelection.init ()
        { ListSelection = state }, Cmd.none

    let update (window: Window) (msg: Msg) (state: State): State * Cmd<Msg> =
        match msg with
        | ListSelection msg ->
            let updatedState, _ = ListSelection.update msg state.ListSelection
            { state with
                ListSelection = updatedState
            },
            Cmd.none

    let view (state: State) (dispatch: Msg -> unit): IView =

        let tabs: List<IView> =
            [
                TabItem.create [
                    TabItem.header "List Selection"
                    TabItem.content (ListSelection.view state.ListSelection (ListSelection >> dispatch))
                    ]
            ]

        // main dock panel
        DockPanel.create [
            DockPanel.children [
                // left: item selector
                DockPanel.create [
                    DockPanel.children [
                        TabControl.create [
                            TabControl.tabStripPlacement Dock.Left
                            TabControl.viewItems tabs
                            ]
                        ]
                    ]
                ]
            ]