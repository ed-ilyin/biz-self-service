module rec App.MyLists
open Fable.React
open Fable.React.Props
open Fable.Core.JsInterop

type Model =
    {   current: Data.Id
        v: Map<Data.Id, Data.Vertex>
        e: Map<Data.Id, Data.Edge>
    }

type Msg = Data.Id

let init () =
    {   current = App.Data.today.id
        v = App.Data.v
        e = App.Data.e
    }

let update msg model = {model with current = msg}

let wrap = div [ Class "wrap" ]
let click classList key dispatch =
    div [
        Class classList
        OnClick (fun e ->
            do e?stopPropagation ()
            dispatch key
        )
    ]
let card = click "wrap card"
let parent = click "parent"
let s s = div [ Class "text" ] [ str s ]
let xSmall s = div [ Class "x-small" ] [ str s ]
let xxSmall s = div [ Class "xx-small" ] [ str s ]
let scroll c = div [Class "scroll"] [c]
let footer = div [Class "footer"]
let strip = div [Class "strip"]

let parents id listId model dispatch =
    model.e
    |> Map.filter (fun k e -> e.outV = id && e.inV <> listId)
    |> Map.toList
    |> List.choose (fun (_,e) ->
         model.v
         |> Map.tryFind e.inV
         |> Option.map (fun v -> v.id, v.label, v.name, e.label)
    )
    |> List.map (fun (vi, vl, vn, el) ->
         parent vi dispatch [xxSmall vl; s vn; xxSmall el]
    )
    |> wrap

let children id listId model dispatch depth =
    model.e
    |> Map.filter (fun k e -> e.inV = id)
    |> Map.toList
    |> List.map (fun (_,e) ->
         vertex e.outV listId model dispatch depth |> card e.outV dispatch
    )

let property = div[Class "property"]
let properties id model dispatch =
    match Map.tryFind id model.v with
    | None -> str "No such vertex"
    | Some v ->
        v.properties
        |> Map.toList
        |> List.map (fun (k, v) ->
            property[xxSmall k;sprintf "%A" v |> s]
        )
        |> wrap

let vertex id listId model dispatch depth =
    match Map.tryFind id model.v with
    | None -> [ str "Vertex not found" ]
    | Some v ->
        match depth with
        | None ->
            [   xSmall v.label
                s v.name
                parents id listId model dispatch
                properties id model dispatch
                children id id model dispatch depth |> wrap
            ]
        | Some 0 -> [str v.name]
        | Some d ->
            [
                xSmall v.label
                s v.name
                parents id listId model dispatch
                properties id model dispatch
                children id id model dispatch (Some (d - 1)) |> wrap
            ]

let view model dispatch =
    div [] [
        div [] [
            None
            |> vertex model.current model.current model dispatch
            |> div [Class "wrap"]
            div [Class "space"] []
        ]
        footer [
            Some 0
            |> children App.Data.lists.id App.Data.lists.id model dispatch
            |> strip
        ]
    ]
