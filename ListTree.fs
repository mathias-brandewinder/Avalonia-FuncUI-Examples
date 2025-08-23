namespace PsychicBarnacle

module ListTreeSelection =

    open System

    open Elmish

    open Avalonia.Controls
    open Avalonia.Layout

    open Avalonia.FuncUI
    open Avalonia.FuncUI.DSL
    open Avalonia.FuncUI.Types

    type Node<'T> = {
        Depth: int
        ID: Guid
        Data: 'T
        Branches: Guid []
        }

    type Tree<'T> = {
        RootID: Guid
        Nodes: Map<Guid, Node<'T>>
        }
        with
        member this.Root =
            this.Nodes
            |> Map.find this.RootID
        static member init (root: 'T) =
            let rootID = Guid.NewGuid ()
            {
                RootID = rootID
                Nodes =
                    Map.empty
                    |> Map.add
                        rootID
                        { Depth = 0; ID = rootID; Branches = Array.empty; Data = root }
            },
            rootID
        static member addChild (parentID: Guid, data: 'T) (tree: Tree<'T>) =
            let parentNode =
                tree.Nodes
                |> Map.find parentID

            let newID = Guid.NewGuid ()
            let newNode = { ID = newID; Branches = Array.empty; Data = data; Depth = parentNode.Depth + 1 }
            let updatedParent = {
                parentNode with
                    Branches =
                        parentNode.Branches
                        |> Array.append (Array.singleton newID)
                }
            { tree with
                Nodes =
                    tree.Nodes
                    |> Map.add parentID updatedParent
                    |> Map.add newID newNode
            },
            newID

        static member deleteNode (nodeID: Guid) (tree: Tree<'T>) =
            let rec children (nodeID: Guid): seq<Node<'T>> =
                seq {
                    yield (tree.Nodes[nodeID])
                    yield!
                        tree.Nodes[nodeID].Branches
                        |> Seq.collect children
                    }
            (tree, children nodeID)
            ||> Seq.fold (fun tree node ->
                { tree with Nodes = tree.Nodes |> Map.remove node.ID}
                )
            |> fun tree ->
                { tree with
                    Nodes =
                        tree.Nodes
                        |> Map.map (fun _ node ->
                            { node with
                                Branches =
                                    node.Branches
                                    |> Array.filter (fun childID -> childID <> nodeID)
                            }
                            )
                }

        member this.Display (): seq<Node<'T>> =
            let rec flatten (nodeID: Guid): seq<Node<'T>> =
                seq {
                    yield (this.Nodes[nodeID])
                    yield!
                        this.Nodes[nodeID].Branches
                        |> Seq.collect flatten
                    }
            this.RootID |> flatten

    type Data = {
        Name: string
        }

    let testTree =
        Tree.init { Name = "Root" }
        |> fun (tree, rootID) ->
            Tree.addChild (rootID, { Name = "1" }) tree
        |> fun (tree, node1) ->
            Tree.addChild (node1, { Name = "1.1" }) tree
        |> fun (tree, _) ->
            let tree, node2 = Tree.addChild (tree.RootID, { Name = "2" }) tree
            let tree, _ = Tree.addChild (node2, { Name = "2.1" }) tree
            let tree, _ = Tree.addChild (node2, { Name = "2.2" }) tree
            tree

    type State = {
        SelectedNodeID: Option<Guid>
        Tree: Tree<Data>
        }
        with
        member this.SelectedNode =
            this.SelectedNodeID
            |> Option.bind (fun selectedID ->
                this.Tree.Nodes
                |> Map.tryFind selectedID
                )

    type Msg =
        | SelectedNodeIDChanged of Option<Guid>
        | SelectedNodeRenamed of Guid * string
        | AddChild of Guid
        | DeleteNode of Guid

    let init (): State * Cmd<Msg> =
        {
            SelectedNodeID = testTree.RootID |> Some
            Tree = testTree
        },
        Cmd.none

    let update (msg: Msg) (state: State): State * Cmd<Msg> =
        match msg with
        | SelectedNodeIDChanged selectedNode ->
            { state with SelectedNodeID = selectedNode },
            Cmd.none

        | SelectedNodeRenamed (nodeID, name) ->
            let updatedTree =
                state.Tree.Nodes
                |> Map.map (fun _ node ->
                    if node.ID = nodeID
                    then { node with Data = { node.Data with Name = name } }
                    else node
                    )
            { state with
                Tree = { state.Tree with Nodes = updatedTree }
            },
            Cmd.none

        | AddChild nodeID ->
            let newNode = { Name = "NEW NODE" }
            let updatedTree, childID = Tree.addChild (nodeID, newNode) state.Tree
            { state with
                Tree = updatedTree
                SelectedNodeID = Some childID
            },
            Cmd.none

        | DeleteNode nodeID ->

            let updatedTree = Tree.deleteNode nodeID state.Tree
            { state with
                Tree = updatedTree
                SelectedNodeID = None
            },
            Cmd.none

    module TreeView =

        let view (state: State) dispatch: IView =
            ListBox.create [
                ListBox.dataItems (state.Tree.Display ())
                ListBox.itemTemplate (
                    DataTemplateView<Node<Data>>.create(fun node ->
                        Border.create [
                            Border.margin (20.0 * (float node.Depth), 0, 0, 0)
                            Border.child (
                                DockPanel.create [
                                    DockPanel.children [
                                        Button.create [
                                            Button.dock Dock.Right
                                            Button.content "+"
                                            Button.onClick (
                                                (fun _ ->
                                                    node.ID
                                                    |> AddChild
                                                    |> dispatch
                                                ),
                                                SubPatchOptions.Always
                                                )
                                            ]
                                        Button.create [
                                            Button.dock Dock.Right
                                            Button.content "x"
                                            Button.onClick (
                                                (fun _ ->
                                                    node.ID
                                                    |> DeleteNode
                                                    |> dispatch
                                                ),
                                                SubPatchOptions.Always
                                                )
                                            ]

                                        TextBlock.create [
                                            TextBlock.text node.Data.Name
                                            ]
                                        ]
                                    ]
                                )
                            ]
                        )
                    )
                ListBox.selectedItem (
                    match state.SelectedNodeID with
                    | None -> null
                    | Some itemId ->
                        (state.Tree.Display ())
                        |> Seq.tryFind (fun (node) -> node.ID = itemId)
                        |> function
                            | None -> null
                            | Some item -> box item
                    )
                ListBox.onSelectedItemChanged(
                    (fun selected ->
                        match selected with
                        | :? Node<Data> as selectedItem ->
                            match state.SelectedNodeID with
                            | None ->
                                selectedItem.ID
                                |> Some
                                |> SelectedNodeIDChanged
                                |> dispatch
                            | Some currentlySelectedId ->
                                if currentlySelectedId <> selectedItem.ID
                                then
                                    selectedItem.ID
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
                ]
            |> View.withKey (Guid.NewGuid().ToString ())
            :> IView

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
                                    TextBox.text (node.Data.Name)
                                    TextBox.onTextChanged (
                                        fun text ->
                                            if node.Data.Name <> text
                                            then
                                                (nodeID, text)
                                                |> SelectedNodeRenamed
                                                |> dispatch
                                        ,
                                        SubPatchOptions.Always
                                        )
                                    ]
                                Border.create []
                                ]
                            ]
                        ]

                    ]
                |> View.withKey (nodeID.ToString ())
                :> IView

    let view (state: State) (dispatch: Msg -> unit): IView =
        DockPanel.create [
            DockPanel.children [
                Border.create [
                    Border.dock Dock.Left
                    Border.width 250
                    Border.child (
                        TreeView.view state dispatch
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
