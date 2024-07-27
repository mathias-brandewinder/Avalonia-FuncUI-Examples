namespace PsychicBarnacle

module Main =

    open System

    open Elmish

    open Avalonia.Controls
    open Avalonia.Layout

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
        | NameChanged of string

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
        | NameChanged name ->
            match state.SelectedItem with
            | None -> state, Cmd.none
            | Some selected ->
                let updatedItem = {
                    selected with
                        Name = name
                    }
                let updatedItems =
                    state.Items
                    |> Array.map (fun item ->
                        if item.Id = selected.Id
                        then updatedItem
                        else item
                        )
                { state with
                    Items = updatedItems
                    SelectedItem = Some updatedItem
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
                            StackPanel.create [
                                StackPanel.orientation Orientation.Vertical
                                StackPanel.children [
                                    TextBlock.create [
                                        TextBlock.text $"Item Id: {item.Id}"
                                        ]
                                    TextBox.create [
                                        TextBox.text item.Name
                                        TextBox.onTextChanged (fun text ->
                                            text
                                            |> NameChanged
                                            |> dispatch
                                            )
                                        ]
                                    NumericUpDown.create [
                                        NumericUpDown.value (decimal item.Value)
                                        ]
                                    ]
                                ]
                        ]
                    ]
                ]
            ]