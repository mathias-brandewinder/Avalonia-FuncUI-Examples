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
        SelectedItemId: Option<Guid>
        }
        with
        member this.ItemIds =
            this.Items
            |> Array.map (fun item -> item.Id)

    type Msg =
        | SelectedItemIdChanged of Option<Guid>
        | NameChanged of string
        | ValueChanged of float

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
            SelectedItemId = None
        },
        Cmd.none

    let update (window: Window) (msg: Msg) (state: State): State * Cmd<Msg> =
        match msg with
        | SelectedItemIdChanged selection ->
            { state with
                SelectedItemId = selection
            },
            Cmd.none
        | NameChanged name ->
            match state.SelectedItemId with
            | None -> state, Cmd.none
            | Some selectedId ->
                let item =
                    state.Items
                    |> Array.find (fun item -> item.Id = selectedId)
                let updatedItem = {
                    item with
                        Name = name
                    }
                let updatedItems =
                    state.Items
                    |> Array.map (fun item ->
                        if item.Id = selectedId
                        then updatedItem
                        else item
                        )
                { state with
                    Items = updatedItems
                },
                Cmd.none
        | ValueChanged value ->
            match state.SelectedItemId with
            | None -> state, Cmd.none
            | Some selectedId ->
                let item =
                    state.Items
                    |> Array.find (fun item -> item.Id = selectedId)
                let updatedItem = {
                    item with
                        Value = value
                    }
                let updatedItems =
                    state.Items
                    |> Array.map (fun item ->
                        if item.Id = selectedId
                        then updatedItem
                        else item
                        )
                { state with
                    Items = updatedItems
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
                            ListBox.dataItems state.ItemIds
                            ListBox.onSelectedItemChanged(fun selectedId ->
                                match selectedId with
                                | :? Guid as selectedItemId ->
                                    selectedItemId
                                    |> Some
                                    |> SelectedItemIdChanged
                                    |> dispatch
                                | _ ->
                                    None
                                    |> SelectedItemIdChanged
                                    |> dispatch
                                )
                            ListBox.itemTemplate (
                                DataTemplateView<Guid>.create(fun itemId ->
                                    TextBlock.create [ TextBlock.text $"{itemId}"])
                                    )
                            ]
                        ]
                    ]
                // right: selected item
                DockPanel.create [
                    DockPanel.children [
                        match state.SelectedItemId with
                        | None ->
                            TextBlock.create [
                                TextBlock.text "No item selected"
                                ]

                        | Some itemId ->
                            let item =
                                state.Items
                                |> Array.find (fun item -> item.Id = itemId)
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
                                        NumericUpDown.onValueChanged (fun value ->
                                            value.Value
                                            |> float
                                            |> ValueChanged
                                            |> dispatch
                                            )
                                        ]
                                    ]
                                ]
                        ]
                    ]
                ]
            ]