type Dependency = Dep of char * char

let parse ( str : string ) = Dep (str.[5], str.[36])

type Node = { Parents: char Set; Children: char Set }

let reduce xs = 
    let map = Map.empty

    xs
        |> Seq.fold (fun map (Dep (master, slave)) -> 

            let map1 = 
                let m = map |> Map.tryFind master
                let newMaster = 
                    match m with
                    | Some m -> { m with Children = m.Children |> Set.add slave }
                    | None -> { Parents = Set.empty; Children = Set.singleton slave }
                in map |> Map.add master newMaster

            let map2 = 
                let s = map1 |> Map.tryFind slave
                let newSlave = 
                    match s with
                    | Some s -> { s with Parents = s.Parents |> Set.add master }
                    | None -> { Parents = Set.singleton master ; Children = Set.empty }
                in map1 |> Map.add slave newSlave

            map2

        ) map

let layout nodes = 
    let seed = reduce nodes 
    
    Seq.unfold (fun state -> 
        let nextMove = 
            state
            |> Map.filter (fun k v -> v.Parents.IsEmpty)
            |> Map.toSeq 
            |> Seq.map fst
            |> Seq.sort
            |> Seq.tryHead

        nextMove |> Option.map ( fun move ->
            let newMap = 
                state 
                |> Map.remove move
                |> Map.map ( fun k v -> { v with Parents = v.Parents |> Set.remove move})

            (move, newMap)
        )
    ) seed
    |> Seq.toArray 
    |> System.String

// part 2

let cost' duration (work:char) = 
    int work - int 'A' + 1 + duration 

type Worker = 
    | Working of remaining:int * job: char
    | Free

type State = {
    Second: int;
    Workers: Worker array;
    Work: Map<char, Node>;
}

let assign cost workers jobs = 

    let workers = List.ofSeq workers
    let moves = List.ofSeq jobs

    let rec go workers moves = 
        match workers, moves with
        | Free::rest, m::ms -> (Working ((cost m), m)) :: go rest ms
        | w::rest, ms -> w :: go rest ms
        | _ -> []

    go workers moves

let layoutTimed seed assignJobs = 

    let getJobsInProgress workers = 
        workers |> Seq.choose ( function | Working (_,j) -> Some j | _ -> None )

    let isJobInProgress job workers =
        let inProgress = getJobsInProgress workers
        inProgress |> Seq.contains job |> not

    let completedJobs workers = 
        workers
        |> Seq.choose ( function | Working (1, j) -> Some j | _ -> None )

    let progressJobExecution workers = 
        workers 
        |> Seq.map (function 
                        | Free -> Free
                        | Working (1, _) -> Free
                        | Working (s, j) -> Working (s-1, j)
        ) 

    let updateWorkPlan completedWork plan = 
        completedWork
        |> Seq.fold (fun state job ->
            state 
            |> Map.remove job
            |> Map.map ( fun k v -> { v with Parents = v.Parents |> Set.remove job })
        ) plan

    let findAvailableJobs work workers = 
        work
        |> Map.filter (fun k v -> v.Parents.IsEmpty && isJobInProgress k workers)
        |> Map.toSeq 
        |> Seq.map fst
        |> Seq.sort
        
    let isWorkCompleted work workers = 
        work = Map.empty && workers |> List.forall ((=)Free)

    Seq.unfold (fun (state:State) -> 

        let completedWorkThisTurn = completedJobs state.Workers
        let workStateThisTurn = updateWorkPlan completedWorkThisTurn state.Work
        let progressedWorkers = progressJobExecution state.Workers
        let availableJobs = findAvailableJobs workStateThisTurn progressedWorkers
        let assignedWorkers = assignJobs progressedWorkers availableJobs

        if isWorkCompleted workStateThisTurn assignedWorkers then
            None
        else
            let time = state.Second + 1
            let state = 
                 { state with 
                    Second = time; 
                    Workers = assignedWorkers |> Array.ofList; 
                    Work = workStateThisTurn }
            Some (state, state)

    ) seed


let testInput = 
    [|
        "Step C must be finished before step A can begin."
        "Step C must be finished before step F can begin."
        "Step A must be finished before step B can begin."
        "Step A must be finished before step D can begin."
        "Step B must be finished before step E can begin."
        "Step D must be finished before step E can begin."
        "Step F must be finished before step E can begin."
    |] |> Array.map parse

let input = 
    System.IO.File.ReadAllLines( __SOURCE_DIRECTORY__ + "\Day7.txt" )
    |> Array.map parse

let test1 = layout testInput

let answer1 = layout input

let test2 = 
    let zero = { Second = 0; Workers = Array.create 2 Free; Work = reduce testInput }
    let assign = assign (cost' 0)
    layoutTimed zero assign 
    |> Seq.last
    |> fun x -> x.Second

let answer2 = 
    let zero = { Second = 0; Workers = Array.create 5 Free; Work = reduce input }
    let assign = assign (cost' 60)
    layoutTimed zero assign 
    |> Seq.last
    |> fun x -> x.Second