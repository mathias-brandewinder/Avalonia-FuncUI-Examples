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
        Branches: Node<'T> []
        }

    [<RequireQualifiedAccess>]
    module Tree =

        let rec map (f: 'T -> 'U) (node: Node<'T>): Node<'U> =
            {
                Item = f node.Item
                Branches =
                    node.Branches
                    |> Array.map (map f)
            }

        let toSeq (node: Node<'T>): seq<Node<'T>> =
            let rec flatten (node: Node<'T>): seq<Node<'T>> =
                seq {
                    yield node
                    yield!
                        node.Branches
                        |> Seq.collect flatten
                    }
            node |> flatten

        let tryFind (predicate: 'T -> bool) (node: Node<'T>): Option<Node<'T>> =
            node
            |> toSeq
            |> Seq.tryFind (fun node -> predicate node.Item)

    type Entity = {
        ID: Guid
        Name: string
        }

    let testGuid = Guid "d9a1bdf3-6451-47e2-b264-0c2f7888b438"

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
                        Item = { ID = testGuid; Name = "Bravo" }
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

    type State = {
        TreeRoot: Node<Entity>
        SelectedNodeID: Option<Guid>
        }
        with
        member this.SelectedNode =
            this.SelectedNodeID
            |> Option.bind (fun selectedID ->
                this.TreeRoot
                |> Tree.tryFind (fun node -> node.ID = selectedID)
                )

    type Msg =
        | SelectedNodeIDChanged of Option<Guid>
        | SelectedNodeRenamed of Guid * string

    let init (): State * Cmd<Msg> =
        {
            TreeRoot = testTree
            SelectedNodeID = Some testGuid
        },
        Cmd.none

    let update (msg: Msg) (state: State): State * Cmd<Msg> =
        match msg with
        | SelectedNodeIDChanged selectedNode ->
            { state with SelectedNodeID = selectedNode },
            Cmd.none

        | SelectedNodeRenamed (nodeID, name) ->
            let updatedTree =
                state.TreeRoot
                |> Tree.map (fun node ->
                    if node.ID = nodeID
                    then { node with Name = name }
                    else node
                    )
            { state with
                TreeRoot = updatedTree
            },
            Cmd.none

    module SelectedNode =

        let view (state: State) dispatch: IView =
            match state.SelectedNodeID with
            | None ->
                DockPanel.create []
            | Some nodeID ->
                let node = state.SelectedNode |> Option.get
                DockPanel.create [
                    DockPanel.children [
                        TextBlock.create [
                            TextBlock.dock Dock.Top
                            TextBlock.text (nodeID.ToString ())
                            ]
                        DockPanel.create [
                            DockPanel.children [
                                TextBox.create [
                                    TextBox.dock Dock.Top
                                    TextBox.text (node.Item.Name)
                                    TextBox.onTextChanged (
                                        fun text ->
                                            if node.Item.Name <> text
                                            then
                                                (nodeID, text)
                                                |> SelectedNodeRenamed
                                                |> dispatch
                                        ,
                                        SubPatchOptions.Never
                                        )
                                    ]
                                Border.create []
                                ]
                            ]
                        ]

                    ]
                |> View.withKey (nodeID.ToString ())
                :> IView

    module TreeSelector =

        module Node =

            let view (node: Node<Entity>) dispatch: IView =
                DockPanel.create [
                    DockPanel.children [
                        TextBlock.create [
                            TextBlock.dock Dock.Top
                            TextBlock.text $"{node.Item.Name}"
                            ]
                        TextBlock.create [
                            TextBlock.fontSize 10
                            TextBlock.text $"{node.Item.ID}"
                            ]
                        ]
                    ]
                |> View.withKey (node.Item.ID.ToString ())
                :> IView

        let view (state: State) dispatch: IView =
            TreeView.create [
                TreeView.dataItems state.TreeRoot.Branches
                TreeView.selectedItem (
                    match state.SelectedNodeID with
                    | None -> null
                    | Some selectedID ->
                        let selectedNode =
                            state.TreeRoot
                            |> Tree.tryFind (fun node -> node.ID = selectedID)
                        selectedNode
                        |> function
                            | None -> null
                            | Some node -> box node
                    )
                TreeView.onSelectedItemChanged (
                    (fun selected ->
                        match selected with
                        | :? Node<Entity> as selectedItem ->
                            match state.SelectedNodeID with
                            | None ->
                                selectedItem.Item.ID
                                |> Some
                                |> SelectedNodeIDChanged
                                |> dispatch
                            | Some currentlySelected ->
                                if currentlySelected <> selectedItem.Item.ID
                                then
                                    selectedItem.Item.ID
                                    |> Some
                                    |> SelectedNodeIDChanged
                                    |> dispatch
                                else ignore ()
                        | _ ->
                            None
                            |> SelectedNodeIDChanged
                            |> dispatch
                        ),
                    SubPatchOptions.Always
                    )
                TreeView.itemTemplate(
                    DataTemplateView<Node<Entity>>.create (
                        (fun node -> node.Branches |> Seq.ofArray),
                        (fun node -> Node.view node dispatch)
                        )
                    )
                ]

    let view (state: State) (dispatch: Msg -> unit): IView =
        DockPanel.create [
            DockPanel.children [
                // Left: Tree selection
                Border.create [
                    Border.dock Dock.Left
                    Border.width 250
                    Border.child (
                        TreeSelector.view state dispatch
                        )
                    ]

                // Right: Selected Node
                Border.create [
                    Border.child (
                        SelectedNode.view state dispatch
                        )
                    ]
                ]
            ]
