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
let lists = newV "list" "Lists" []
let tasks = newV "list" "Tasks" []
let activeProjects = newV "list" "Active Projects" []
let areas = newV "list" "Areas" []
let wishes = newV "list" "Wishes" []
let pGerman = newV "goal" "Говорю по немецки" []
let pGTD = newV "goal" "Я всегда знаю чем лучше заняться" []
let wGerman = newV "wish" "Я разговариваю с немцами по немецки" []
let wDreams = newV "wish" "Я осуществил свои мечты" []
let daily = newV "list" "Daily" []
let events =
    []
    |> addV me
    |> addV2V me "🔘" lists
    |> addV2V lists "🔘" activeProjects
    |> addV2V lists "🔘" tasks
    |> addV2V lists "🔘" daily
    |> addV2V lists "🔘" areas
    |> addV2V lists "🔘" wishes
    |> addV2V activeProjects "🔘" pGerman
    |> addV2V activeProjects "🔘" pGTD
    |> addV2V wDreams "🔘" pGTD
    |> addV2V wishes "🔘" wGerman
    |> addV2V wishes "🔘" wDreams
    |> addV2V wGerman "🔘" pGerman
    |> addNewV2V [pGerman;daily] "🔘" "task" "Открой следующий урок немецкого в Duolingo и пройди его" []
    |> addNewV2V [pGTD;daily] "🔘" "task" "Прочти первую запись в списке Входящие и разбери её" []
    |> addNewV2V [pGTD;daily] "🔘" "task" "Прочти первое письмо и разбери его" []
    |> addNewV2V [areas] "🔘" "area" "ALSO" []

let eventsFolder event (vertexes,edges) =
    match event with
    | AddV v -> Map.add v.id v vertexes, edges
    | AddE e -> vertexes, Map.add e.id e edges

let graph = List.foldBack eventsFolder events (Map.empty,Map.empty)
let v = fst graph
let e = snd graph
