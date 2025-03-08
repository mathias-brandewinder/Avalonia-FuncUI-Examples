namespace PsychicBarnacle

module AsyncOperation =

    open System
    open Elmish

    open Avalonia
    open Avalonia.Controls
    open Avalonia.Media

    open Avalonia.FuncUI.DSL
    open Avalonia.FuncUI.Types

    let respondToRequest (request: string) =
        task {
            // Create an artificial delay
            do! Async.Sleep 1000
            return $"{DateTime.Now}: Request was {request}"
            }

    type State = {
        Request: string
        Response: string
        }

    type Msg =
        | UpdateRequest of string
        | SendRequest
        | ReceivedResponse of string

    let init (): State * Cmd<Msg> =
        {
            Request = ""
            Response = ""
        },
        Cmd.none

    let update (msg: Msg) (state: State): State * Cmd<Msg> =
        match msg with
        | UpdateRequest text ->
            { state with Request = text }, Cmd.none
        | SendRequest ->
            let deferredCmd =
                Cmd.OfTask.perform
                    respondToRequest
                    state.Request
                    ReceivedResponse
            state, deferredCmd
        | ReceivedResponse response ->
            { state with Response = response }, Cmd.none

    let view (state: State) (dispatch: Msg -> unit): IView =
        // main dock panel
        StackPanel.create [
            StackPanel.children [
                TextBox.create [
                    TextBox.watermark "Type your request"
                    TextBox.text $"{state.Request}"
                    TextBox.onTextChanged (fun text ->
                        text
                        |> UpdateRequest
                        |> dispatch
                        )
                    ]
                Button.create [
                    Button.content "Send Request"
                    Button.onClick (fun _ ->
                        SendRequest
                        |> dispatch
                        )
                    ]
                TextBlock.create [
                    TextBlock.text state.Response
                    ]
                ]
            ]
