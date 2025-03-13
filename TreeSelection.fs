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

    let testTree =
        {
            Item = "Root"
            Branches =
                [|
                    {
                        Item = "Alpha"
                        Branches = [|
                            { Item = "Alpha Alpha"; Branches = [||] }
                            { Item = "Alpha Bravo"; Branches = [||] }
                            { Item = "Alpha Charlie"; Branches = [||] }
                            |]
                    }
                    {
                        Item = "Bravo"
                        Branches = [| |]
                    }
                    {
                        Item = "Charlie"
                        Branches = [|
                            { Item = "Charlie Alpha"; Branches = [||] }
                            {
                                Item = "Charlie Bravo"
                                Branches = [|
                                    { Item = "Charlie Bravo Alpha"; Branches = [||] }
                                    { Item = "Charlie Bravo Bravo"; Branches = [||] }
                                    |]
                            }
                            |]
                    }
                |]
        }
        |> Tree.map (fun name -> "Changed: " + name)

    type State = {
        Tree: Node<string>
        }

    type Msg =
        | TODO

    let init (): State * Cmd<Msg> =
        {
            Tree = testTree
        },
        Cmd.none

    let update (msg: Msg) (state: State): State * Cmd<Msg> =
        match msg with
        | TODO -> state, Cmd.none

    let nodeView: Node<string> -> IView =
        fun node ->
            DockPanel.create [
                DockPanel.children [
                    CheckBox.create [
                        CheckBox.dock Dock.Left
                        ]
                    TextBlock.create [
                        TextBlock.text $"{node.Item}"
                        TextBlock.verticalAlignment VerticalAlignment.Center
                        ]
                    ]
                ]

    let view (state: State) (dispatch: Msg -> unit): IView =
        DockPanel.create [
            DockPanel.children [
                TreeView.create [
                    TreeView.isOpen true
                    TreeView.dataItems state.Tree.Branches
                    TreeView.itemTemplate(
                        DataTemplateView<Node<string>>.create(
                            (fun node -> node.Branches),
                            nodeView
                            )
                        )
                    ]
                ]
            ]
