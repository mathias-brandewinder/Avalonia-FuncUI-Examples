namespace PsychicBarnacle

module ListSelection =

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
        IsIncluded: bool
        }

    type State = {
        Items: Item []
        SelectedItemId: Option<Guid>
        Filter: string
        }
        with
        member this.VisibleItems =
            this.Items
            |> Array.filter (fun item ->
                item.Name.Contains(this.Filter)
                )

    type Msg =
        | SelectedItemIdChanged of Option<Guid>
        | NameChanged of string
        | ValueChanged of float
        | FilterChanged of string
        | CreateItem
        | IsIncludedChanged of Guid

    let init (): State * Cmd<Msg> =
        let items =
            Array.init 10 (fun i ->
                {
                    Id = Guid.NewGuid()
                    Name = $"Item {i}"
                    Value = float i
                    IsIncluded = false
                }
                )
        {
            Items = items
            SelectedItemId = Some (items.[0].Id)
            Filter = ""
        },
        Cmd.none

    let update (msg: Msg) (state: State): State * Cmd<Msg> =
        match msg with
        | SelectedItemIdChanged selection ->
            { state with
                SelectedItemId = selection
            },
            Cmd.none

        | IsIncludedChanged itemId ->
            let updatedItems =
                state.Items
                |> Array.map (fun item ->
                    if item.Id = itemId
                    then { item with IsIncluded = not item.IsIncluded }
                    else item
                    )
            { state with
                Items = updatedItems
            },
            Cmd.none

        | NameChanged name ->
            match state.SelectedItemId with
            | None -> state, Cmd.none
            | Some selectedId ->
                let updatedItems =
                    state.Items
                    |> Array.map (fun item ->
                        if item.Id = selectedId
                        then { item with Name = name }
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
                let updatedItems =
                    state.Items
                    |> Array.map (fun item ->
                        if item.Id = selectedId
                        then { item with Value = value }
                        else item
                        )
                { state with
                    Items = updatedItems
                },
                Cmd.none

        | FilterChanged filter ->
            { state with
                Filter = filter
            },
            Cmd.none

        | CreateItem ->
            let item = {
                Id = Guid.NewGuid()
                Name = "NEW ITEM"
                Value = 0.0
                IsIncluded = false
                }
            { state with
                Items =
                    state.Items
                    |> Array.append (Array.singleton item)
            },
            Cmd.none

    module Selector =

        let view (state: State) (dispatch: Msg -> unit): IView =
            DockPanel.create [
                DockPanel.dock Dock.Left
                DockPanel.width 200
                DockPanel.children [
                    StackPanel.create [
                        StackPanel.orientation Orientation.Vertical
                        StackPanel.children [

                            TextBlock.create [ TextBlock.text "Filter" ]
                            TextBox.create [
                                TextBox.text state.Filter
                                TextBox.onTextChanged (fun text ->
                                    text |> FilterChanged |> dispatch
                                    )
                                ]

                            TextBlock.create [ TextBlock.text "Create" ]
                            Button.create [
                                Button.content "Create New"
                                Button.onClick (fun _ -> CreateItem |> dispatch)
                                ]

                            Border.create [
                                Border.dock Dock.Bottom
                                ]

                            TextBlock.create [ TextBlock.text "Items" ]
                            ListBox.create [
                                ListBox.dataItems (state.VisibleItems)
                                ListBox.selectedItem (
                                    match state.SelectedItemId with
                                    | None -> null
                                    | Some itemId ->
                                        state.VisibleItems
                                        |> Array.tryFind (fun item -> item.Id = itemId)
                                        |> function
                                            | None -> null
                                            | Some item -> box item
                                    )
                                ListBox.onSelectedItemChanged(
                                    (fun selected ->
                                        match selected with
                                        | :? Item as selectedItem ->
                                            match state.SelectedItemId with
                                            | None ->
                                                selectedItem.Id
                                                |> Some
                                                |> SelectedItemIdChanged
                                                |> dispatch
                                            | Some currentlySelectedId ->
                                                if currentlySelectedId <> selectedItem.Id
                                                then
                                                    selectedItem.Id
                                                    |> Some
                                                    |> SelectedItemIdChanged
                                                    |> dispatch
                                                else ignore ()
                                        | _ ->
                                            None
                                            |> SelectedItemIdChanged
                                            |> dispatch
                                        ),
                                    SubPatchOptions.Always
                                    )
                                ListBox.itemTemplate (
                                    DataTemplateView<Item>.create(fun item ->
                                        StackPanel.create [
                                            StackPanel.orientation Orientation.Horizontal
                                            StackPanel.children [
                                                CheckBox.create [
                                                    CheckBox.isChecked item.IsIncluded
                                                    CheckBox.onIsCheckedChanged (fun _ ->
                                                        item.Id
                                                        |> IsIncludedChanged
                                                        |> dispatch
                                                        )
                                                ]
                                                TextBlock.create [
                                                    TextBlock.text $"{item.Name}"
                                                    ]
                                                ]
                                            ]
                                        )
                                    )
                                ]
                                // We assign a unique key each time,
                                // forcing a refresh of the ListBox.
                                |> View.withKey (Guid.NewGuid().ToString())
                            ]
                        ]
                    ]
                ]

    let view (state: State) (dispatch: Msg -> unit): IView =
        // main dock panel
        DockPanel.create [
            DockPanel.children [
                // left: item selector
                DockPanel.create [
                    DockPanel.dock Dock.Left
                    DockPanel.width 200

                    DockPanel.children [
                        Selector.view state dispatch
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
                                            if text <> item.Name
                                            then
                                                text
                                                |> NameChanged
                                                |> dispatch
                                            )
                                        ]
                                    NumericUpDown.create [
                                        NumericUpDown.value (decimal item.Value)
                                        NumericUpDown.onValueChanged (fun value ->
                                            if value.Value <> (decimal item.Value)
                                            then
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