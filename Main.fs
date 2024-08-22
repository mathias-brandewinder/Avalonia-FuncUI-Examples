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

    [<CustomEquality; NoComparison>]
    type Selector = {
        Id: Guid
        DisplayName: string
        }
        with
        interface IEquatable<Selector> with
            member this.Equals (other: Selector): bool =
                other.Id = this.Id
        override this.Equals other =
            match other with
            | :? Selector as s -> s.Id = this.Id
            | _ -> false
        override this.GetHashCode (): int =
            this.Id.GetHashCode()

    type State = {
        Items: Item []
        SelectedItemId: Option<Guid>
        ContainsA: bool
        }
        with
        member this.ItemIds =
            this.Items
            |> Array.filter (fun item ->
                if this.ContainsA
                then item.Name.Contains("A")
                else true
                )
            |> Array.map (fun item ->
                {
                    Id = item.Id
                    DisplayName = item.Name
                }
                )

    type Msg =
        | SelectedItemIdChanged of Option<Guid>
        | NameChanged of string
        | ValueChanged of float
        | ToggleContainsA
        | CreateItem

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
            ContainsA = false
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

        | ToggleContainsA ->
            { state with ContainsA = not (state.ContainsA) },
            Cmd.none

        | CreateItem ->
            let item = {
                Id = Guid.NewGuid()
                Name = "NEW ITEM"
                Value = 0.0
                }
            { state with
                Items = state.Items |> Array.append (Array.singleton item )
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
                        StackPanel.create [
                            StackPanel.orientation Orientation.Vertical
                            StackPanel.children [

                                TextBlock.create [ TextBlock.text "Filters" ]
                                CheckBox.create [
                                    CheckBox.content "Name contains A"
                                    CheckBox.isChecked state.ContainsA
                                    CheckBox.onIsCheckedChanged (fun _ -> ToggleContainsA |> dispatch)
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
                                        ListBox.dataItems state.ItemIds
                                        ListBox.onSelectedItemChanged(fun selected ->
                                            match selected with
                                            | :? Selector as selectedItem ->
                                                selectedItem.Id
                                                |> Some
                                                |> SelectedItemIdChanged
                                                |> dispatch
                                            | _ ->
                                                None
                                                |> SelectedItemIdChanged
                                                |> dispatch
                                            )
                                        ListBox.itemTemplate (
                                            DataTemplateView<Selector>.create(fun item ->
                                                TextBlock.create [ TextBlock.text $"{item.DisplayName}"])
                                                )
                                        ]
                                    ]
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