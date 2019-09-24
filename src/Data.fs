module App.Data
open System

type Id = Guid

type Value =
    | String of string
    | DateTime of DateTime

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

let newE (inV:Id) label (outV:Id) =
    {   id = Guid.NewGuid ()
        label = label
        inV = inV
        outV = outV
    }

let addV v events = AddV v :: events

let addE (inV:Vertex) h (outV:Vertex) events =
    AddE (newE inV.id h outV.id) :: events

let addV2V v1 edgeLabel v2 events =
    AddV v2 :: events |> addE v1 edgeLabel v2

let addNewV2V vs e label name properties events =
    let v2 = newV label name properties
    List.fold (fun es v1 -> addE v1 e v2 es) (AddV v2 :: events) vs
let d s = DateTime.Parse s |> DateTime
let me = newV "👨🏽‍💼" "Ed Ilyin" []
let lists = newV "list" "Lists" []
let tasks = newV "list" "Tasks" []
let today = newV "list" "Today" []
let activeProjects = newV "list" "Active Projects" []
let somedayMaybe = newV "list" "Someday / Maybe" []
let areas = newV "list" "Areas" []
let wishes = newV "list" "Wishes" []
let pGerman = newV "goal" "Говорю по немецки" []
let pGTD = newV "goal" "Моя обезъяна всегда знает чем лучше заняться" []
let wGerman = newV "wish" "Я разговариваю с немцами по немецки" []
let wDreams = newV "wish" "Я осуществил свои мечты" []
let daily = newV "list" "Daily" []
let addNewProjectAndTask project task events =
    let project = newV "project" project []
    events
    |> addV2V activeProjects "🔘" project
    |> addNewV2V [project;tasks] "🔘" "task" task []
let events =
    []
    |> addV me
    |> addV2V me "🔘" lists
    |> addV2V lists "🔘" activeProjects
    |> addV2V lists "🔘" somedayMaybe
    |> addV2V lists "🔘" tasks
    |> addV2V lists "🔘" today
    |> addV2V lists "🔘" daily
    |> addV2V lists "🔘" areas
    |> addV2V lists "🔘" wishes
    |> addV2V activeProjects "🔘" pGerman
    |> addV2V activeProjects "🔘" pGTD
    |> addV2V wDreams "🔘" pGTD
    |> addV2V wishes "🔘" wGerman
    |> addV2V wishes "🔘" wDreams
    |> addV2V wGerman "🔘" pGerman
    |> addNewV2V [pGerman;daily] "🔘" "task"
        "Открой следующий урок немецкого в Duolingo и пройди его" []
    |> addNewV2V [pGTD;daily] "🔘" "task"
        "Прочти первую запись в списке Входящие и разбери её" []
    |> addNewV2V [pGTD;daily] "🔘" "task"
        "Прочти первое письмо и разбери его" []
    |> addNewV2V [areas] "🔘" "area" "ALSO" []
    |> addNewV2V [tasks] "🔘" "task"
        "Remind MathiasBo and MichaelPe to answer on your questions"
        ["start", d"2019-09-25"]
    |> addNewProjectAndTask "Я получил деньги за поездку в Вену"
        "Найди папку с отчётом за командировку в Вену"
let eventsFolder event (vertexes,edges) =
    match event with
    | AddV v -> Map.add v.id v vertexes, edges
    | AddE e -> vertexes, Map.add e.id e edges
let startPassed vertexes id =
    match vertexes |> Map.tryFind id with
    | None -> true
    | Some vertex ->
        match vertex.properties |> Map.tryFind "start" with
        | None -> true
        | Some (DateTime dateTime) -> dateTime < DateTime.Now
        | Some (String _) -> true
let addToVertex fromId toId (v, e) =
    v, Map.filter (fun _ e -> e.inV = fromId && startPassed v e.outV) e
        |> Map.fold (fun edges _ edge ->
             let newEdge = newE toId edge.label edge.outV
             Map.add newEdge.id newEdge edges
        ) e
let v, e =
    List.foldBack eventsFolder events (Map.empty,Map.empty)
    |> addToVertex daily.id today.id
    |> addToVertex tasks.id today.id
