namespace PsychicBarnacle

module TreeSelection =

    open System

    open Elmish

    open Avalonia.Controls
    open Avalonia.Layout

    open Avalonia.FuncUI
    open Avalonia.FuncUI.DSL
    open Avalonia.FuncUI.Types

    type Node<'T> = {
        Item: 'T
        Branches: seq<Node<'T>>
        }

    [<RequireQualifiedAccess>]
    module Tree =

        let rec map (f: 'T -> 'U) (node: Node<'T>): Node<'U> =
            {
                Item = f node.Item
                Branches =
                    node.Branches
                    |> Seq.map (map f)
            }

        let rec nodes (node: Node<'T>): seq<Node<'T>> =
            seq {
                yield node
                yield!
                    node.Branches
                    |> Seq.collect nodes
                }

        let rec tryFind (f: 'T -> bool) (node: Node<'T>): Option<Node<'T>> =
            node
            |> nodes
            |> Seq.tryFind (fun node -> f node.Item)

    type Entity = {
        ID: Guid
        Name: string
        }

    let testTree: Node<Entity> =
        {
            Item = { ID = Guid.NewGuid (); Name = "Root" }
            Branches =
                [|
                    {
                        Item = { ID = Guid.NewGuid(); Name = "Alpha" }
                        Branches = [|
                            { Item = { ID = Guid.NewGuid(); Name = "Alpha Alpha" }; Branches = [||] }
                            { Item = { ID = Guid.NewGuid(); Name = "Alpha Bravo" }; Branches = [||] }
                            { Item = { ID = Guid.NewGuid(); Name = "Alpha Charlie"} ; Branches = [||] }
                            |]
                    }
                    {
                        Item = { ID = Guid.NewGuid(); Name = "Bravo" }
                        Branches = [| |]
                    }
                    {
                        Item = { ID = Guid.NewGuid(); Name = "Charlie" }
                        Branches = [|
                            { Item = { ID = Guid.NewGuid(); Name = "Charlie Alpha" }; Branches = [||] }
                            {
                                Item = { ID = Guid.NewGuid(); Name = "Charlie Bravo" }
                                Branches = [|
                                    { Item = { ID = Guid.NewGuid(); Name = "Charlie Bravo Alpha" }; Branches = [||] }
                                    { Item = { ID = Guid.NewGuid(); Name = "Charlie Bravo Bravo" }; Branches = [||] }
                                    |]
                            }
                            |]
                    }
                |]
        }
        // |> Tree.map (fun node -> { node with Name = "Changed: " + node.Name })

    type State = {
        Tree: Node<Entity>
        SelectedNodeID: Option<Guid>
        }
        with
        member this.SelectedNode () =
            this.SelectedNodeID
            |> Option.bind (fun selectedID ->
                this.Tree
                |> Tree.tryFind (fun node -> node.ID = selectedID)
                )

    type Msg =
        | SelectedNodeChanged of Option<Guid>
        | SelectedNodeRenamed of string

    let init (): State * Cmd<Msg> =
        {
            Tree = testTree
            SelectedNodeID = None
        },
        Cmd.none

    let update (msg: Msg) (state: State): State * Cmd<Msg> =
        match msg with
        | SelectedNodeChanged selectedNode ->
            { state with SelectedNodeID = selectedNode },
            Cmd.none
        | SelectedNodeRenamed name ->
            match state.SelectedNodeID with
            | None -> state, Cmd.none
            | Some selectedNode ->
                { state with
                    Tree =
                        state.Tree
                        |> Tree.map (fun node ->
                            if node.ID = selectedNode
                            then { node with Name = name }
                            else node
                            )
                },
                Cmd.none

    let nodeView: Node<Entity> -> IView =
        fun node ->
            DockPanel.create [
                DockPanel.children [
                    CheckBox.create [
                        CheckBox.dock Dock.Left
                        ]
                    TextBlock.create [
                        TextBlock.text $"{node.Item.Name}"
                        TextBlock.verticalAlignment VerticalAlignment.Center
                        ]
                    ]
                ]

    let view (state: State) (dispatch: Msg -> unit): IView =
        DockPanel.create [
            DockPanel.children [
                // Left: Tree selection
                Border.create [
                    Border.dock Dock.Left
                    Border.width 250
                    Border.child (
                        TreeView.create [
                            TreeView.isOpen true
                            TreeView.dataItems state.Tree.Branches
                            TreeView.selectedItem (
                                match state.SelectedNode () with
                                | None -> null
                                | Some item -> box item
                                )
                            TreeView.onSelectedItemChanged (
                                (fun selected ->
                                    match selected with
                                    | :? Node<Entity> as selectedItem ->
                                        match state.SelectedNodeID with
                                        | None ->
                                            selectedItem.Item.ID
                                            |> Some
                                            |> SelectedNodeChanged
                                            |> dispatch
                                        | Some currentlySelected ->
                                            if currentlySelected <> selectedItem.Item.ID
                                            then
                                                selectedItem.Item.ID
                                                |> Some
                                                |> SelectedNodeChanged
                                                |> dispatch
                                            else ignore ()
                                    | _ ->
                                        None
                                        |> SelectedNodeChanged
                                        |> dispatch
                                    ),
                                SubPatchOptions.Always
                                )
                            TreeView.itemTemplate(
                                DataTemplateView<Node<Entity>>.create(
                                    (fun node -> node.Branches),
                                    nodeView
                                    )
                                )
                            ]
                        )
                    ]

                // Right: Selected Node
                Border.create [
                    Border.child (
                        TextBox.create [
                            TextBox.text (
                                state.SelectedNode ()
                                |> Option.map (fun node -> node.Item.Name)
                                |> Option.defaultValue "Nothing selected"
                                )
                            TextBox.onTextChanged (fun text ->
                                let selectedNode = state.SelectedNode ()
                                match selectedNode with
                                | None -> ignore ()
                                | Some node ->
                                    if node.Item.Name <> text
                                    then
                                        text
                                        |> SelectedNodeRenamed
                                        |> dispatch
                                )
                            ]
                        )
                    ]
                ]
            ]
