namespace PsychicBarnacle

module TreeSelection =

    open System

    open Elmish

    open Avalonia.Controls
    open Avalonia.Layout

    open Avalonia.FuncUI
    open Avalonia.FuncUI.DSL
    open Avalonia.FuncUI.Types

    type Node = {
        Name: string
        Branches: seq<Node>
        }

    let testTree =
        {
            Name = "Root"
            Branches =
                [|
                    {
                        Name = "Alpha"
                        Branches = [|
                            { Name = "Alpha Alpha"; Branches = [||] }
                            { Name = "Alpha Bravo"; Branches = [||] }
                            { Name = "Alpha Charlie"; Branches = [||] }
                            |]
                    }
                    {
                        Name = "Bravo"
                        Branches = [| |]
                    }
                    {
                        Name = "Charlie"
                        Branches = [|
                            { Name = "Charlie Alpha"; Branches = [||] }
                            {
                                Name = "Charlie Bravo"
                                Branches = [|
                                    { Name = "Charlie Bravo Alpha"; Branches = [||] }
                                    { Name = "Charlie Bravo Bravo"; Branches = [||] }
                                    |]
                            }
                            |]
                    }
                |]
        }

    type State = {
        Tree: Node[]
        }

    type Msg =
        | TODO

    let init (): State * Cmd<Msg> =
        {
            Tree =
                testTree.Branches
                |> Array.ofSeq
        },
        Cmd.none

    let update (msg: Msg) (state: State): State * Cmd<Msg> =
        match msg with
        | TODO -> state, Cmd.none

    let nodeView: Node -> IView =
        fun node ->
            DockPanel.create [
                DockPanel.children [
                    CheckBox.create [
                        CheckBox.dock Dock.Left
                        ]
                    TextBlock.create [
                        TextBlock.text $"{node.Name}"
                        TextBlock.verticalAlignment VerticalAlignment.Center
                        ]
                    ]
                ]

    let view (state: State) (dispatch: Msg -> unit): IView =
        DockPanel.create [
            DockPanel.children [
                TreeView.create [
                    TreeView.isOpen true
                    TreeView.dataItems state.Tree
                    TreeView.itemTemplate(
                        DataTemplateView<Node>.create(
                            (fun node -> node.Branches),
                            nodeView
                            )
                        )
                    ]
                ]
            ]
