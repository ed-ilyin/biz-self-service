module App.Editor
open Fable.React
open Fable.React.Props
open Fable.Core.JsInterop

type TypeDesc =
    | Nothing
    | OneOf of Map<string, TypeDesc>
    | AllOf of Map<string, TypeDesc>
    | Checkbox
    | Number
    | String

type Model = {
    types: Map<string, TypeDesc>
    model: TypeDesc
    msg: TypeDesc
}

type Msg = ChangeModel of TypeDesc

let init () = {
    types = Map.empty
    model = OneOf Map.empty
    msg = Nothing
}

let update msg model =
    match msg with | ChangeModel typeDesc -> { model with model = typeDesc }

// VIEW
let typeDescToString = function
    | Nothing -> "Nothing"
    | OneOf _ -> "One of"
    | AllOf _ -> "All of"
    | Checkbox -> "Checkbox"
    | Number -> "Number"
    | String -> "String"

let stringToTypeDesc = function
    | "One of" -> OneOf Map.empty
    | "All of" -> AllOf Map.empty
    | "Checkbox" -> Checkbox
    | "Number" -> Number
    | "String" -> String
    | _ -> Nothing

let opt name = option [] [ str name ]

let rec typeDesc model dispatch =
    let dropdown =
        select [
            Value (typeDescToString model)
            OnChange (fun ev -> stringToTypeDesc !!ev.target?value |> ChangeModel |> dispatch)
            ] [ opt "Nothing"
                opt "One of"
                opt "All of"
                opt "Checkbox"
                opt "Number"
                opt "String"
            ]
    let content =
        match model with
            | OneOf options | AllOf options -> str "options"
            | _ -> nothing
    div [] [ dropdown; content ]

let view model dispatch =
    div [] [
        h1 [] [ str "Actor AKA Mailbox Processor AKA State Machine" ]
        h2 [] [ str "Model / State Description" ]
        typeDesc model.model dispatch
    ]
