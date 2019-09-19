module View

open Elmish
open Fable.React
open Fable.React.Props
open Fulma
open Fable.FontAwesome

type Model = { myLists: MyLists.Model }

type Msg = MyLists of MyLists.Msg

let init _ = { myLists = MyLists.init () }, Cmd.none

let private update msg model =
    // match msg with
    // | Editor msg -> { model with editor = Editor.update msg model.editor }, Cmd.none
    model, Cmd.none

let private view model dispatch =
    MyLists.view model.myLists (MyLists >> dispatch)

open Elmish.Debug
open Elmish.HMR

Program.mkProgram init update view
|> Program.withReactSynchronous "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
