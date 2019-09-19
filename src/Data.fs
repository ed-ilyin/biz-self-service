module App.Data
open System

type Id = Guid

type Value =
    | String of string

type Vertex =
    {   id: Id
        label: string
        name: string
        properties: Map<string,Value>
    }

type Edge =
    {   id: Id
        label: string
        inV: Id
        outV: Id
    }

type Msg =
    | AddV of Vertex
    | AddE of Edge

let newV label name properties =
    {   id = Guid.NewGuid ()
        label = label
        name = name
        properties = Map.ofList properties
    }

let newE (v1:Vertex) label (v2:Vertex) =
    {   id = Guid.NewGuid ()
        label = label
        inV = v1.id
        outV = v2.id
    }

let addV v events = AddV v :: events

let addE v1 h v2 events = AddE (newE v1 h v2) :: events

let addV2V v1 edgeLabel v2 events =
    AddV v2 :: events |> addE v1 edgeLabel v2

let addNewV2V vs e label name properties events =
    let v2 = newV label name properties
    List.fold (fun es v1 -> addE v1 e v2 es) (AddV v2 :: events) vs

let me = newV "👨🏽‍💼" "Ed Ilyin" []
let lists = newV "📋" "Lists" []
let tasks = newV "📋" "Tasks" []
let projects = newV "📋" "Projects" []
let pGerman = newV "🥅" "Говорю по немецки" []
let events =
    []
    |> addV me
    |> addV2V me "🔘" lists
    |> addV2V lists "🔘" tasks
    |> addV2V lists "🔘" projects
    |> addV2V projects "🔘" pGerman
    |> addNewV2V [tasks;pGerman] "🔘" "☑️" "Пройди один урок немецкого в Duolingo" []

let eventsFolder event (vertexes,edges) =
    match event with
    | AddV v -> Map.add v.id v vertexes, edges
    | AddE e -> vertexes, Map.add e.id e edges

let graph = List.foldBack eventsFolder events (Map.empty,Map.empty)
let v = fst graph
let e = snd graph
