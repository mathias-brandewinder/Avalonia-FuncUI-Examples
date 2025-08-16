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
        | DeleteItem of Guid
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
                SelectedItemId = Some item.Id
            },
            Cmd.none

        | DeleteItem itemID ->
            { state with
                Items =
                    state.Items
                    |> Array.filter (fun item -> item.Id <> itemID)
                SelectedItemId = None
            },
            Cmd.none

    module Selector =

        module Filter =

            let view state dispatch =
                StackPanel.create [
                    StackPanel.children [
                        TextBlock.create [ TextBlock.text "Filter" ]
                        TextBox.create [
                            TextBox.text state.Filter
                            TextBox.onTextChanged (fun text ->
                                text |> FilterChanged |> dispatch
                                )
                            ]
                        ]
                    ]

        module ItemsList =

            let view (state: State) dispatch =
                DockPanel.create [
                    DockPanel.children [

                        TextBlock.create [
                            TextBlock.dock Dock.Top
                            TextBlock.text "Items"
                            TextBlock.fontSize 16
                            ]

                        Button.create [
                            Button.dock Dock.Top
                            Button.classes [ "wide" ]
                            Button.margin (0,10,0,0)
                            Button.content "Create New"
                            Button.onClick (fun _ ->
                                CreateItem
                                |> dispatch)
                            ]

                        ListBox.create [
                            ListBox.margin (0, 10, 0, 0)
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
                                    DockPanel.create [
                                        // StackPanel.orientation Orientation.Horizontal
                                        DockPanel.children [
                                            CheckBox.create [
                                                CheckBox.dock Dock.Left
                                                CheckBox.isChecked item.IsIncluded
                                                CheckBox.onIsCheckedChanged (fun _ ->
                                                    item.Id
                                                    |> IsIncludedChanged
                                                    |> dispatch
                                                    )
                                            ]
                                            Button.create [
                                                Button.dock Dock.Right
                                                Button.fontSize 8
                                                Button.content "X"
                                                Button.onClick (
                                                    (fun _ ->
                                                        item.Id
                                                        |> DeleteItem
                                                        |> dispatch
                                                    ),
                                                    SubPatchOptions.Always
                                                    )
                                                ]
                                            TextBlock.create [
                                                TextBlock.verticalAlignment VerticalAlignment.Center
                                                TextBlock.textTrimming Avalonia.Media.TextTrimming.CharacterEllipsis
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

        let view (state: State) (dispatch: Msg -> unit): IView =
            DockPanel.create [
                DockPanel.children [
                    // top section: filter
                    Border.create [

                        Border.dock Dock.Top

                        Border.classes [ "card" ]
                        Border.borderBrush "Black"

                        Border.child (
                            Filter.view state dispatch
                            )
                        ]

                    // bottom section: count of items
                    Border.create [
                        Border.dock Dock.Bottom

                        Border.classes [ "card"; "below" ]
                        Border.borderBrush "Black"

                        Border.child (
                            TextBlock.create [
                                TextBlock.text $"Total items: {state.Items.Length}"
                                ]
                            )
                        ]

                    // middle section / fill: list
                    Border.create [
                        Border.classes [ "below" ]
                        Border.child (
                            ItemsList.view state dispatch
                            )
                        ]
                    ]
                ]

    module SelectedItem =

        let view state dispatch: IView =
            DockPanel.create [
                DockPanel.children [
                    match state.SelectedItemId with
                    | None ->
                        TextBlock.create [
                            TextBlock.classes [ "watermark" ]
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

    let view (state: State) (dispatch: Msg -> unit): IView =
        // main dock panel
        DockPanel.create [
            DockPanel.margin 10
            DockPanel.children [
                // left section: item selector
                Border.create [
                    Border.dock Dock.Left
                    Border.width 200
                    Border.child (
                        Selector.view state dispatch
                        )
                    ]
                // left section: end

                // right section: selected item
                Border.create [
                    Border.classes [ "right" ]
                    Border.child (
                        SelectedItem.view state dispatch
                        )
                    ]
                // right section: end

                ]
            ]