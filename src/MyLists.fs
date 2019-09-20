module rec MyLists
open Fable.React
open Fable.React.Props
open Fable.Core.JsInterop

type Model = System.Guid

type Msg = unit

let init () = App.Data.activeProjects.id

let wrap = div [ Class "wrap" ]
let card = div [ Class "wrap card" ]
let s s = div [ Class "text" ] [ str s ]
let parent = div [ Class "parent" ]
let small s = div [ Class "small" ] [ str s ]
let xxSmall s = div [ Class "xx-small" ] [ str s ]

let parents id =
    App.Data.e
    |> Map.filter (fun k e -> e.outV = id)
    |> Map.toList
    |> List.choose (fun (_,e) ->
         App.Data.v
         |> Map.tryFind e.inV
         |> Option.map (fun v -> v.label, v.name, e.label)
    )
    |> List.map (fun (vl,vn,el) -> parent [xxSmall vl; str vn; xxSmall el])
    |> wrap

let children id depth =
    App.Data.e
    |> Map.filter (fun k e -> e.inV = id)
    |> Map.toList
    |> List.map (fun (_,e) -> vertex e.outV depth)
    |> wrap

let vertex id depth =
    match Map.tryFind id App.Data.v with
    | None -> str "No such Vertex"
    | Some v ->
        card [
            wrap [small v.label; s v.name]
            parents id
            children id depth
        ]

let view model dispatch =
    div [] [children model None; children App.Data.lists.id (Some 0)]
