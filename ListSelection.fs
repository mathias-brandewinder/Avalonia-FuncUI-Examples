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
        Description: string
        }

    type State = {
        Items: Item []
        SelectedItemId: Option<Guid>
        SearchString: string
        }
        with
        member this.VisibleItems =
            this.Items
            |> Array.filter (fun item ->
                item.Name.Contains this.SearchString
                )

    type Msg =
        | SelectedItemIdChanged of Option<Guid>
        | CreateItem
        | DeleteItem of Guid
        | NameChanged of string
        | DescriptionChanged of string
        | SearchStringChanged of string

    let init (): State * Cmd<Msg> =
        let items =
            Array.init 10 (fun i ->
                {
                    Id = Guid.NewGuid()
                    Name = $"Item {i}"
                    Description = $"Item {i} description"
                }
                )
        {
            Items = items
            SelectedItemId = Some (items.[0].Id)
            SearchString = ""
        },
        Cmd.none

    let update (msg: Msg) (state: State): State * Cmd<Msg> =
        match msg with
        | SelectedItemIdChanged selection ->
            { state with
                SelectedItemId = selection
            },
            Cmd.none

        | CreateItem ->
            let item = {
                Id = Guid.NewGuid()
                Name = "NEW ITEM"
                Description = ""
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

        | SearchStringChanged search ->
            { state with SearchString = search },
            Cmd.none

        | NameChanged name ->
            match state.SelectedItemId with
            | None -> state, Cmd.none
            | Some selectedId ->
                let items =
                    state.Items
                    |> Array.map (fun item ->
                        if item.Id = selectedId
                        then { item with Name = name }
                        else item
                        )
                { state with Items = items },
                Cmd.none

        | DescriptionChanged description ->
            match state.SelectedItemId with
            | None -> state, Cmd.none
            | Some selectedId ->
                let items =
                    state.Items
                    |> Array.map (fun item ->
                        if item.Id = selectedId
                        then { item with Description = description }
                        else item
                        )
                { state with Items = items },
                Cmd.none

    module Selector =

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
                            Button.content "Create New"
                            Button.onClick (fun _ ->
                                CreateItem
                                |> dispatch
                                )
                            ]

                        ListBox.create [
                            ListBox.dataItems (state.VisibleItems)
                            ListBox.selectedItem (
                                match state.SelectedItemId with
                                | None -> null
                                | Some itemId ->
                                    state.Items
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
                                        DockPanel.children [
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
                        Border.child (
                            TextBox.create [
                                TextBox.dock Dock.Top
                                TextBox.watermark "Search"
                                TextBox.text state.SearchString
                                TextBox.onTextChanged (fun text ->
                                    text
                                    |> SearchStringChanged
                                    |> dispatch
                                    )
                                ]
                            )
                        ]

                    // bottom section: count of items
                    Border.create [
                        Border.dock Dock.Bottom

                        Border.classes [ "below" ]

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

        let view (item: Item) dispatch: IView =
            DockPanel.create [
                DockPanel.children [
                    StackPanel.create [
                        StackPanel.children [
                            TextBox.create [
                                TextBox.text item.Name
                                TextBox.onTextChanged (fun text ->
                                    text
                                    |> NameChanged
                                    |> dispatch
                                    )
                                ]
                            TextBox.create [
                                TextBox.text item.Description
                                TextBox.onTextChanged (fun text ->
                                    text
                                    |> DescriptionChanged
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
                        state.SelectedItemId
                        |> Option.bind (fun selectedItemID ->
                            state.Items
                            |> Array.tryFind (fun item ->
                                item.Id = selectedItemID
                                )
                            )
                        |> function
                        | None ->
                            TextBlock.create [
                                TextBlock.text "Select an Item"
                                ]
                            :> IView
                        | Some item ->
                            SelectedItem.view item dispatch
                        )
                    ]
                // right section: end
                ]
            ]