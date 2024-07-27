namespace PsychicBarnacle

module Main =

    open System

    open Elmish

    open Avalonia.Controls

    open Avalonia.FuncUI
    open Avalonia.FuncUI.DSL
    open Avalonia.FuncUI.Types

    type Item = {
        Id: Guid
        Name: string
        Value: float
        }

    type State = {
        Items: Item []
        SelectedItem: Option<Item>
        }

    type Msg =
        | SelectedItemChanged of Option<Item>

    let init (): State * Cmd<Msg> =
        let items =
            Array.init 10 (fun i ->
                {
                    Id = Guid.NewGuid()
                    Name = $"Item {i}"
                    Value = float i
                }
                )
        {
            Items = items
            SelectedItem = None
        },
        Cmd.none

    let update (window: Window) (msg: Msg) (state: State): State * Cmd<Msg> =
        match msg with
        | SelectedItemChanged selection ->
            { state with
                SelectedItem = selection
            },
            Cmd.none

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
                            ListBox.onSelectedItemChanged(fun selected ->
                                match selected with
                                | :? Item as selectedItem ->
                                    selectedItem
                                    |> Some
                                    |> SelectedItemChanged
                                    |> dispatch
                                | _ ->
                                    None
                                    |> SelectedItemChanged
                                    |> dispatch
                                )
                            ListBox.itemTemplate (
                                DataTemplateView<Item>.create(fun item ->
                                    TextBlock.create [ TextBlock.text $"{item.Id}"])
                                    )
                            ]
                        ]
                    ]
                // right: selected item
                DockPanel.create [
                    DockPanel.children [
                        match state.SelectedItem with
                        | None ->
                            TextBlock.create [
                                TextBlock.text "No item selected"
                                ]

                        | Some item ->
                            TextBlock.create [
                                TextBlock.text $"{item.Name}"
                                ]
                        ]
                    ]
                ]
            ]