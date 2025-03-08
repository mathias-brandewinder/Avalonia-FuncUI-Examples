namespace PsychicBarnacle

module Layout =

    open System

    open Elmish

    open Avalonia.Controls
    open Avalonia.Layout
    open Avalonia.Media

    open Avalonia.FuncUI
    open Avalonia.FuncUI.DSL
    open Avalonia.FuncUI.Types

    let longText = "Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book. It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged. It was popularised in the 1960s with the release of Letraset sheets containing Lorem Ipsum passages, and more recently with desktop publishing software like Aldus PageMaker including versions of Lorem Ipsum."

    type State = {
        Items: list<string>
        }

    let init (): State =
        {
            Items =
                List.init 20 (fun i -> $"Item {i}")
        }

    module Selection =

        let view (state: State) =
            DockPanel.create [
                DockPanel.children [
                    TextBlock.create [
                        TextBlock.dock Dock.Top
                        TextBlock.text "TITLE"
                        TextBlock.background (Colors.LightBlue.ToString())
                        ]
                    TextBlock.create [
                        TextBlock.dock Dock.Bottom
                        TextBlock.text "FOOTER"
                        TextBlock.background (Colors.LightGreen.ToString())
                        ]
                    ListBox.create [
                        ListBox.dataItems state.Items
                        ]
                    ]
                ]

    let view (state: State) =
        DockPanel.create [

            DockPanel.children [
                // Left: Selection
                Border.create [
                    Border.dock Dock.Left
                    Border.minWidth 150
                    Border.child (
                        Selection.view state
                        )
                    ]

                // Right: Edit Selected
                TextBlock.create [
                    TextBlock.dock Dock.Top
                    TextBlock.text "TITLE"
                    TextBlock.background (Colors.LightYellow.ToString())
                    ]
                TextBlock.create [
                    TextBlock.dock Dock.Top
                    TextBlock.text "SUBSECTION"
                    TextBlock.background (Colors.Orange.ToString())
                    ]
                TextBlock.create [
                    TextBlock.text longText
                    TextBlock.textWrapping TextWrapping.WrapWithOverflow
                    ]
                ]
            ]