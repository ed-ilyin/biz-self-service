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

type PathStep = Skip | Key of string | Value of TypeDesc

type Msg = Change of PathStep list

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
    | None :: tail, ListOf t -> change tail value t |> ListOf
    | Some key :: tail, OneOf list ->
        match Map.tryFind key list with
        | None -> value
        | Some t -> Map.add key (change tail value t) list |> OneOf
    | Some key :: tail, AllOf list ->
        match Map.tryFind key list with
        | None -> value
        | Some t -> Map.add key (change tail value t) list |> AllOf
    | _ -> value

let rec remove item state = state

let update msg model =
    match msg with
    | Change path ->
        { model with
              model = change (List.rev path) value model.model }

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

let wrap = div [ Class "wrap" ]
let row = tr [ Class "row" ]
let cell = td [ Class "cell" ]

let inputKey value dispatch =
    input [ Value value; OnChange (fun ev -> dispatch !!ev.target?value)]

let rec typeDesc model path dispatch =
    let listEditor list =
        let body =
            Map.toList list |> List.map (fun (k,v) ->
                row [
                    cell [inputKey k (ChangeKey >> dispatch)]
                    cell [typeDesc v (Some k::path) dispatch ]
                ]
            ) |> tbody []
        table[Class "table"][
            body
            tfoot[][
                row [
                    cell [input []]
                    cell [typeDesc Nothing path dispatch ]
                ]
            ]
        ]
    let dropdown =
        select [
            Class "menulist"
            Value (typeDescToString model)
            OnChange (fun ev ->
                ChangeValue (path, stringToTypeDesc !!ev.target?value)
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
    match model with
        | OneOf list | AllOf list -> wrap [ dropdown; listEditor list ]
        | ListOf kind ->
            wrap [ dropdown; typeDesc kind (None :: path) dispatch ]
        | _ -> dropdown

let view model dispatch =
    div [] [
        h1 [] [ str "Actor AKA Mailbox Processor AKA State Machine" ]
        h2 [] [ str "Model AKA State Type" ]
        typeDesc model.model [] dispatch
    ]
