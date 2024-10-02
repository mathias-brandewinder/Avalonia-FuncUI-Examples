namespace PsychicBarnacle

module AsyncOperation =

    open System
    open System.Threading

    open Elmish

    open Avalonia.Controls
    open Avalonia.Layout
    open Avalonia.Threading

    open Avalonia.FuncUI
    open Avalonia.FuncUI.DSL
    open Avalonia.FuncUI.Types

    module AsyncCalls =

        // The trick here was to use
        // Program.runWithAvaloniaSyncDispatch () in Program,
        // instead of Program.run.
        let increment (x: int) =
            async {
                do! Async.Sleep 2000
                return x + 1
                }

        let decrement (x: int) =
            async {
                do! Async.Sleep 2000
                return x - 1
                }

    type State = {
        Number: int
        }

    type Msg =
        | StartedIncrement
        | CompletedIncrement of int
        | Decrement

    let init (): State * Cmd<Msg> =
        {
            Number = 0
        },
        Cmd.none

    let update (msg: Msg) (state: State): State * Cmd<Msg> =
        match msg with
        | StartedIncrement ->
            let cmd = Cmd.OfAsync.perform (AsyncCalls.increment) state.Number CompletedIncrement
            state,
            cmd
        | CompletedIncrement value ->
            { state with Number = value },
            Cmd.none
        | Decrement ->
            { state with Number = state.Number - 1 },
            Cmd.none

    let view (state: State) (dispatch: Msg -> unit): IView =
        // main dock panel
        DockPanel.create [
            DockPanel.children [
                StackPanel.create [
                    StackPanel.children [
                        TextBlock.create [
                            TextBlock.text $"{state.Number}"
                            ]
                        Button.create [
                            Button.content "Increment asynchronously, with delay"
                            Button.onClick (fun _ -> StartedIncrement |> dispatch)
                            ]
                        Button.create [
                            Button.content "Decrement, synchronous / immediate"
                            Button.onClick (fun _ -> Decrement |> dispatch)
                            ]
                        ]
                    ]
                ]
            ]