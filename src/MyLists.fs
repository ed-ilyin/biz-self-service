module rec MyLists
open Fable.React
open Fable.React.Props
open Fable.Core.JsInterop

type Model = System.Guid

type Msg = unit

let init () = App.Data.me.id

let wrap = div [ Class "wrap" ]
let wrapBorder = div [ Class "wrap-border" ]
let s s = div [ Class "text" ] [ str s ]
let parent s = div [ Class "parent" ] [ str s ]

let parents id =
    App.Data.e
    |> Map.filter (fun k e -> e.outV = id)
    |> Map.toList
    |> List.choose (fun (_,e) ->
         App.Data.v
         |> Map.tryFind e.inV
         |> Option.map (fun v -> v.label, v.name, e.label)
    )
    |> List.map (fun (vl,vn,el) -> sprintf "%s %s %s" vl vn el |> parent)
    |> wrap

let children id =
    App.Data.e
    |> Map.filter (fun k e -> e.inV = id)
    |> Map.toList
    |> List.map (fun (_,e) -> vertex e.outV )
    |> wrap

let vertex id =
    match Map.tryFind id App.Data.v with
    | None -> str "No such Vertex"
    | Some v -> wrapBorder [ parents id; sprintf "%s %s" v.label v.name |> s; children id ]

let view model dispatch =
    div [] [
        h1 [] [ str "My Lists" ]
        vertex model
    ]

// do printfn "%A" App.Data.graph
 
