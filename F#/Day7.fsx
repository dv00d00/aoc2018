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

let zero input = { Second = 0; Workers = Array.create 5 Free; Work = reduce input }

zero testInput

let assign cost workers moves = 

    let rec go workers moves = 
        match workers, moves with
        | Free::rest, m::ms -> (Working ((cost m), m)) :: go rest ms
        | w::rest, ms -> w :: go rest ms
        | _ -> []

    go workers moves

let layoutTimed seed assign = 

    Seq.unfold (fun (state:State) -> 

        let nextMoves = 
            state.Work
            |> Map.filter (fun k v -> v.Parents.IsEmpty)
            |> Map.toSeq 
            |> Seq.map fst
            |> Seq.sort
            |> List.ofSeq

        let workers = 
            state.Workers 
            |> Array.map (function 
                          | Free -> Free
                          | Working (s, _) when s = 0 -> Free
                          | Working (s, j) -> Working (s-1, j)
            ) |> List.ofArray

        let newWorkers = assign workers nextMoves
        
        let completedWorkThisTurn = 
            let inProgress = 
                newWorkers
                |> Seq.choose (
                    function 
                    | Working (t, j) -> Some j 
                    | _ -> None
                )

            Set.intersect (set nextMoves) (set inProgress)
            
        let newWorkState = 
            takenWorkThisTurn
            |> Seq.fold (fun state job ->
                state 
                |> Map.remove job
                |> Map.map ( fun k v -> { v with Parents = v.Parents |> Set.remove job })
            ) state.Work

        if newWorkState = Map.empty && newWorkers |> List.forall ((=)Free) then
            None
        else
            let time = state.Second + 1
            let state = 
                 { state with 
                    Second = time; 
                    Workers = newWorkers |> Array.ofList; 
                    Work = newWorkState }
            Some (state, state)

    ) seed

let test2 = 
    let zero = { Second = 0; Workers = Array.create 2 Free; Work = reduce testInput }
    let assign = assign (cost' 0)
    layoutTimed zero assign 
    |> Seq.map (fun x -> x.Second, x.Workers )
    |> Seq.toArray



let answer2 = 
    let zero = { Second = 0; Workers = Array.create 5 Free; Work = reduce input }
    let assign = assign (cost' 60)
    layoutTimed zero assign 
    |> Seq.last