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
        AsyncOperation: AsyncOperation.State
        }

    type Msg =
        | ListSelection of ListSelection.Msg
        | AsyncOperation of AsyncOperation.Msg

    let init (): State * Cmd<Msg> =
        let listSelectionState, _ = ListSelection.init ()
        let asyncOperationState, _ = AsyncOperation.init ()
        {
            ListSelection = listSelectionState
            AsyncOperation = asyncOperationState
        },
        Cmd.none

    let update (window: Window) (msg: Msg) (state: State): State * Cmd<Msg> =
        match msg with
        | ListSelection msg ->
            let updatedState, _ = ListSelection.update msg state.ListSelection
            { state with
                ListSelection = updatedState
            },
            Cmd.none
        | AsyncOperation msg ->
            let updatedState, followUpCommand = AsyncOperation.update msg state.AsyncOperation
            { state with
                AsyncOperation = updatedState
            },
            Cmd.batch [ (Cmd.map AsyncOperation followUpCommand) ]

    let view (state: State) (dispatch: Msg -> unit): IView =

        let tabs: List<IView> =
            [
                TabItem.create [
                    TabItem.header "List Selection"
                    TabItem.content (ListSelection.view state.ListSelection (ListSelection >> dispatch))
                    ]
                TabItem.create [
                    TabItem.header "Async Operations"
                    TabItem.content (AsyncOperation.view state.AsyncOperation (AsyncOperation >> dispatch))
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