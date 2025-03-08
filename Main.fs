namespace PsychicBarnacle

module Main =

    open Elmish

    open Avalonia.Controls
    open Avalonia.Layout

    open Avalonia.FuncUI
    open Avalonia.FuncUI.DSL
    open Avalonia.FuncUI.Types

    type State = {
        ListSelection: ListSelection.State
        AsyncOperation: AsyncOperation.State
        TreeSelection: TreeSelection.State
        Layout: Layout.State
        }

    type Msg =
        | ListSelection of ListSelection.Msg
        | AsyncOperation of AsyncOperation.Msg
        | Tree of TreeSelection.Msg

    let init (): State * Cmd<Msg> =

        let listSelectionState, _ = ListSelection.init ()
        let asyncOperationState, _ = AsyncOperation.init ()
        let treeState, _ = TreeSelection.init ()
        let layout = Layout.init ()

        {
            ListSelection = listSelectionState
            AsyncOperation = asyncOperationState
            TreeSelection = treeState
            Layout = layout
        },
        Cmd.none

    let update (window: Window) (msg: Msg) (state: State): State * Cmd<Msg> =
        match msg with
        | ListSelection msg ->
            let updatedState, cmd = ListSelection.update msg state.ListSelection
            { state with
                ListSelection = updatedState
            },
            Cmd.map ListSelection cmd

        | AsyncOperation msg ->
            let updatedState, cmd = AsyncOperation.update msg state.AsyncOperation
            { state with
                AsyncOperation = updatedState
            },
            Cmd.map AsyncOperation cmd

        | Tree msg ->
            let updatedState, cmd = TreeSelection.update msg state.TreeSelection
            {
                state with TreeSelection = updatedState
            },
            Cmd.map Tree cmd

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
                TabItem.create [
                    TabItem.header "Tree Selection"
                    TabItem.content (TreeSelection.view state.TreeSelection (Tree >> dispatch))
                    ]
                TabItem.create [
                    TabItem.header "Layout"
                    TabItem.content (Layout.view state.Layout)
                    ]
            ]

        // main dock panel
        DockPanel.create [
            DockPanel.children [
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
