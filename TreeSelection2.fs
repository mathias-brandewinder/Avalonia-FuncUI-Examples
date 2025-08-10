namespace PsychicBarnacle

module TreeSelection2 =

    open System

    open Elmish

    open Avalonia.Controls
    open Avalonia.Layout

    open Avalonia.FuncUI
    open Avalonia.FuncUI.DSL
    open Avalonia.FuncUI.Types

    type Node = {
        ID: Guid
        Branches: seq<Node>
        }

    type Entity = {
        ID: Guid
        Name: string
        }

    let entities =
        Array.init 10 (fun i ->
            { ID = Guid.NewGuid (); Name = $"Entity {i}"}
            )

    let testTree: Node =
        {   ID = entities.[0].ID
            Branches = [|
                {
                    ID = entities.[1].ID
                    Branches = [| { ID = entities.[2].ID; Branches = Seq.empty } |]
                }
                {
                    ID = entities.[3].ID
                    Branches = [| |]
                }
                {
                    ID = entities.[4].ID
                    Branches = [|
                        {
                            ID = entities.[5].ID
                            Branches = [| { ID = entities.[6].ID; Branches = Seq.empty } |] }
                        |]
                }
                |]
        }

    type State = {
        TreeRoot: Node
        Entities: Map<Guid, Entity>
        SelectedNodeID: Option<Guid>
        }

    type Msg =
        | SelectedNodeIDChanged of Option<Guid>
        | SelectedNodeRenamed of Guid * string

    let init (): State * Cmd<Msg> =
        {
            TreeRoot = testTree
            SelectedNodeID = None
            Entities =
                entities
                |> Array.map (fun e -> e.ID, e)
                |> Map.ofArray
        },
        Cmd.none

    let update (msg: Msg) (state: State): State * Cmd<Msg> =
        match msg with
        | SelectedNodeIDChanged selectedNode ->
            { state with SelectedNodeID = selectedNode },
            Cmd.none
        | SelectedNodeRenamed (id, name) ->
            { state with
                Entities =
                    state.Entities
                    |> Map.map (fun key value ->
                        if key = id
                        then { ID = key; Name = name }
                        else value
                        )
            },
            Cmd.none

    module SelectedNode =

        let view (state: State) dispatch: IView =
            match state.SelectedNodeID with
            | None ->
                DockPanel.create []
            | Some nodeID ->
                let entity = state.Entities.[nodeID]
                DockPanel.create [
                    DockPanel.children [
                        TextBlock.create [
                            TextBlock.dock Dock.Top
                            TextBlock.text (nodeID.ToString ())
                            ]
                        DockPanel.create [
                            DockPanel.children [
                                TextBox.create [
                                    TextBox.text (entity.Name)
                                    TextBox.onTextChanged (
                                        fun text ->
                                            (nodeID, text)
                                            |> SelectedNodeRenamed
                                            |> dispatch
                                        ,
                                        SubPatchOptions.OnChangeOf state.SelectedNodeID
                                        )
                                    ]
                                ]
                            ]
                        ]

                    ]
                |> View.withKey (nodeID.ToString ())
                :> IView

    module TreeSelector =

        module Node =

            let view (state: State) (node: Node) dispatch: IView =
                let entity = state.Entities.[node.ID]
                DockPanel.create [
                    DockPanel.children [
                        TextBlock.create [
                            TextBlock.dock Dock.Top
                            TextBlock.text $"{entity.Name}"
                            ]
                        TextBlock.create [
                            TextBlock.fontSize 10
                            TextBlock.text $"{node.ID}"
                            ]
                        ]
                    ]

        let view (state: State) dispatch: IView =
            TreeView.create [
                TreeView.dataItems state.TreeRoot.Branches
                // TreeView.selectedItem (
                //     match state.SelectedNodeID with
                //     | None -> null
                //     | Some selectedID -> box selectedID
                //     )
                TreeView.onSelectedItemChanged (
                    (fun selected ->
                        match selected with
                        | :? Node as selectedItem ->
                            match state.SelectedNodeID with
                            | None ->
                                selectedItem.ID
                                |> Some
                                |> SelectedNodeIDChanged
                                |> dispatch
                            | Some currentlySelected ->
                                if currentlySelected <> selectedItem.ID
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

                TreeView.itemTemplate(
                    DataTemplateView<Node>.create (
                        (fun node -> node.Branches),
                        (fun node -> Node.view state node dispatch)
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
