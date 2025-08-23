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
                        { ID = rootID; Branches = Array.empty; Data = root }
            }
        static member addChild (parentID: Guid, data: 'T) (tree: Tree<'T>) =
            let parentNode =
                tree.Nodes
                |> Map.find parentID

            let newID = Guid.NewGuid ()
            let newNode = { ID = newID; Branches = Array.empty; Data = data; }
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
            }
        member this.Display (rootID: Guid): seq<int * Node<'T>> =
            let rec flatten (depth: int, nodeID: Guid): seq<int * Node<'T>> =
                seq {
                    yield (depth, this.Nodes[nodeID])
                    yield!
                        this.Nodes[nodeID].Branches
                        |> Seq.collect (fun nodeID -> flatten (depth + 1, nodeID))
                    }
            (0, this.RootID) |> flatten

    type Data = {
        Name: string
        }

    let testTree =

        let node11 = { ID = Guid.NewGuid (); Branches = Array.empty; Data = { Name = "1.1" } }
        let node1 = { ID = Guid.NewGuid (); Branches = [| node11.ID |]; Data = { Name = "1" } }

        let node21 = { ID = Guid.NewGuid (); Branches = Array.empty; Data = { Name = "2.1" } }
        let node22 = { ID = Guid.NewGuid (); Branches = Array.empty; Data = { Name = "2.2" } }

        let node2 = { ID = Guid.NewGuid (); Branches = [| node21.ID; node22.ID |]; Data = { Name = "2" } }

        let root = { ID = Guid.NewGuid (); Branches = [| node1.ID; node2.ID |]; Data = { Name = "Root" } }

        {
            RootID = root.ID
            Nodes =
                [ node11; node1; node21; node22; node2; root ]
                |> List.map (fun node -> node.ID, node)
                |> Map.ofList
        }

    type State = {
        SelectedNodeID: Option<Guid>
        NewTree: Tree<Data>
        }
        with
        member this.SelectedNode =
            this.SelectedNodeID
            |> Option.bind (fun selectedID ->
                this.NewTree.Nodes
                |> Map.tryFind selectedID
                )

    type Msg =
        | SelectedNodeIDChanged of Option<Guid>
        | SelectedNodeRenamed of Guid * string

    let view2 (state: State) dispatch: IView =
        ListBox.create [
            ListBox.dataItems (state.NewTree.Display state.NewTree.RootID)
            ListBox.itemTemplate (
                DataTemplateView<int * Node<Data>>.create(fun (depth, node) ->
                    Border.create [
                        Border.margin (20.0 * (float depth), 0, 0, 0)
                        Border.child (
                            TextBlock.create [
                                TextBlock.text node.Data.Name
                                ]
                            )
                        ]
                    )
                )
            ListBox.selectedItem (
                match state.SelectedNodeID with
                | None -> null
                | Some itemId ->
                    state.NewTree.Nodes
                    |> Map.tryFind itemId
                    |> function
                        | None -> null
                        | Some item -> box item
                )
            ListBox.onSelectedItemChanged(
                (fun selected ->
                    match selected with
                    | :? (int * Node<Data>) as (_, selectedItem) ->
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

    let init (): State * Cmd<Msg> =
        {
            SelectedNodeID = testTree.RootID |> Some
            NewTree = testTree
        },
        Cmd.none

    let update (msg: Msg) (state: State): State * Cmd<Msg> =
        match msg with
        | SelectedNodeIDChanged selectedNode ->
            { state with SelectedNodeID = selectedNode },
            Cmd.none

        | SelectedNodeRenamed (nodeID, name) ->
            let updatedTree =
                state.NewTree.Nodes
                |> Map.map (fun nodeID node ->
                    if node.ID = nodeID
                    then { node with Data = { node.Data with Name = name } }
                    else node
                    )
            { state with
                NewTree = { state.NewTree with Nodes = updatedTree }
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
                                    TextBox.text (node.Data.Name)
                                    TextBox.onTextChanged (
                                        fun text ->
                                            if node.Data.Name <> text
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
                // |> View.withKey node.Item.Name // (nodeID.ToString ())
                // :> IView

    let view (state: State) (dispatch: Msg -> unit): IView =
        DockPanel.create [
            DockPanel.children [
                Border.create [
                    Border.dock Dock.Left
                    Border.width 250
                    Border.child (
                        view2 state dispatch
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
