module rec MyLists
open Fable.React
open Fable.React.Props
open Fable.Core.JsInterop

type Model = System.Guid

type Msg = System.Guid

let init () = App.Data.activeProjects.id

let update msg model = msg

let wrap = div [ Class "wrap" ]
let card key dispatch =
    div [
        Class "wrap card"
        Key (string key)
    ]
let parent key dispatch =
    div [
        Class "parent"
        OnClick (fun e -> dispatch key)
    ]
let s s = div [ Class "text" ] [ str s ]
let small s = div [ Class "small" ] [ str s ]
let xxSmall s = div [ Class "xx-small" ] [ str s ]
let scroll c = div [Class "scroll"] [c]
let footer = div [Class "footer"]
let strip = div [Class "strip"]

let parents id dispatch =
    App.Data.e
    |> Map.filter (fun k e -> e.outV = id)
    |> Map.toList
    |> List.choose (fun (_,e) ->
         App.Data.v
         |> Map.tryFind e.inV
         |> Option.map (fun v -> v.id, v.label, v.name, e.label)
    )
    |> List.map (fun (vi, vl, vn, el) ->
         parent vi dispatch [xxSmall vl; str vn; xxSmall el]
    )
    |> wrap

let children id dispatch depth =
    App.Data.e
    |> Map.filter (fun k e -> e.inV = id)
    |> Map.toList
    |> List.map (fun (_,e) ->
         vertex e.outV dispatch depth |> card e.outV dispatch
    )

let vertex id dispatch depth =
    match Map.tryFind id App.Data.v with
    | None -> [ str "No such Vertex" ]
    | Some v ->
        match depth with
        | None ->
            [
                wrap [small v.label; s v.name]
                parents id dispatch
                children id dispatch depth |> wrap
            ]
        | Some 0 -> [s v.name]
        | Some d ->
            [
                wrap [small v.label; s v.name]
                parents id dispatch
                children id dispatch (Some (d - 1)) |> wrap
            ]

let view model dispatch =
    div [] [
        None |> vertex model dispatch |> wrap
        footer
            [Some 0 |> children App.Data.lists.id dispatch |> strip]
    ]
