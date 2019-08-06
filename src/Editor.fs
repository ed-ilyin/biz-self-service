module App.Editor
open Fable.React
open Fable.React.Props
open Fable.Core.JsInterop

type TypeDesc =
    | Nothing
    | OneOf of Map<string, TypeDesc>
    | AllOf of Map<string, TypeDesc>
    | ListOf of TypeDesc
    | Checkbox
    | Number
    | String

type Model = {
    types: Map<string, TypeDesc>
    model: TypeDesc
    msg: TypeDesc
}

type Msg =
    | Change of TypeDesc list

let init () = {
    types = Map.empty
    model =
        Map.ofList [
            "Issue ID", String
            "Issue Name", String
            "Comments", ListOf String
        ] |> OneOf
    msg = Nothing
}

let rec add update state = state

let rec change path state =
    match path, state with
    | [], _ | Nothing :: _, _ -> Nothing
    | ListOf _ :: tail, ListOf j -> change tail j |> ListOf
    | ListOf t :: tail, _ -> change tail t |> ListOf
    | other :: _, _ -> other

let rec remove item state = state

let update msg model =
    match msg with
    | Change path -> { model with model = change (List.rev path) model.model }

// VIEW
let typeDescToString = function
    | Nothing -> "Nothing"
    | OneOf _ -> "One of"
    | AllOf _ -> "All of"
    | ListOf _ -> "List of"
    | Checkbox -> "Checkbox"
    | Number -> "Number"
    | String -> "String"

let stringToTypeDesc = function
    | "One of" -> OneOf Map.empty
    | "All of" -> AllOf Map.empty
    | "List of" -> ListOf Nothing
    | "Checkbox" -> Checkbox
    | "Number" -> Number
    | "String" -> String
    | _ -> Nothing

let opt name = option [] [ str name ]

let listEditor list =
    Map.toList list
    |> List.map (fun (k,v) -> div [] [ str k ])
    |> div []

let rec typeDesc model path dispatch =
    let dropdown =
        select [
            Value (typeDescToString model)
            OnChange (fun ev ->
                stringToTypeDesc !!ev.target?value :: path
                    |> Change
                    |> dispatch
                )
        ] [ opt "Nothing"
            opt "One of"
            opt "All of"
            opt "List of"
            opt "Checkbox"
            opt "Number"
            opt "String"
        ]
    let content =
        match model with
            | OneOf list | AllOf list -> listEditor list
            | ListOf kind ->
                typeDesc kind (ListOf Nothing :: path) dispatch
            | _ -> nothing
    div [] [ dropdown; content ]

let view model dispatch =
    div [] [
        h1 [] [ str "Actor AKA Mailbox Processor AKA State Machine" ]
        h2 [] [ str "Model AKA State Type" ]
        typeDesc model.model [] dispatch
    ]
