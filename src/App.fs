module App.View

open Elmish
open Fable.React
open Fable.React.Props
open Fulma
open Fable.FontAwesome

type Model = { editor: Editor.Model }

type Msg = Editor of Editor.Msg

let init _ = { editor = Editor.init () }, Cmd.none

let private update msg model =
    match msg with
    | Editor msg -> { model with editor = Editor.update msg model.editor }, Cmd.none

let private view model dispatch = Editor.view model.editor (Editor >> dispatch)

open Elmish.Debug
open Elmish.HMR

Program.mkProgram init update view
|> Program.withReactSynchronous "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
