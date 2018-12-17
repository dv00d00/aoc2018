module rec Solution = 

    type Point = int * int
    type Origin = Point
    type Distance = int

    type Status = ClosestTo of Origin * Distance | ClosestToMany

    type Universe = Map<Point, Status> * Origin[]

    let parse (str:string) : Point = 
        let [|x;y|] = str.Split(',') |> Array.map int
        (x,y)

    let testInput = 
        [| "1, 1"; "1, 6"; "8, 3"; "3, 4"; "5, 5"; "8, 9"; |] 
        |> Array.map parse

    let buildUniverse inpt : Universe = 
        inpt 
        |> Array.map (fun pt -> pt, ClosestTo (pt, 0))
        |> Map.ofArray
        |> fun uni -> uni, inpt

    let distance (x1,y1) (x2,y2) = abs (x1 - x2) + abs (y1 - y2)

    let grow (x,y) = [| (x+1, y); (x-1, y); (x, y+1); (x, y-1) |]

    let step (universe: Universe) : Universe =
        let (state, origins) = universe
        let points = state |> Map.toSeq |> Seq.map fst
        
        let newPoints = 
            points 
            |> Seq.collect ( fun existingPoint -> 
                existingPoint 
                |> grow 
                |> Array.filter ( fun newPoint -> state |> Map.tryFind newPoint = None )
            )

        let markedPoints = 
            newPoints 
            |> Seq.map (fun newPoint -> 
                let distancesToOrigins = origins |> Array.map (fun o -> o, distance o newPoint)
                let shortestDistance = distancesToOrigins |> Array.map snd |> Array.min
                let candidates = distancesToOrigins |> Array.filter ( fun (o, distance) -> distance = shortestDistance )

                let status =
                    match candidates with
                    | [| (origin, distance) |] -> ClosestTo (origin, distance)
                    | _ -> ClosestToMany

                newPoint, status
            )

        let nextState =  markedPoints |> Seq.fold ( fun map (k,v) -> map |> Map.add k v) state
        nextState, origins
   
    let process start = 
        Seq.unfold (fun st -> let next = step st in Some (next, next )) start

open Solution

let prodInput = [|
    (249,60);(150,332);(174,83);(287,329);(102,338);(111,201);(259,96);(277,161);(143,288);(202,311);(335,55);
    (239,148);(137,224);(48,214);(186,87);(282,334);(147,157);(246,191);(241,181);(286,129);(270,287);(79,119);
    (189,263);(324,280);(316,279);(221,236);(327,174);(141,82);(238,317);(64,264);(226,151);(110,110);(336,194);
    (235,333);(237,55);(230,137);(267,44);(258,134);(223,42);(202,76);(159,135);(229,238);(197,83);(173,286);
    (123,90);(314,165);(140,338);(347,60);(108,76);(268,329);
|]

let sim input count = 

    let start = input |> buildUniverse
    let consecutiveStates = process start |> Seq.skip count |> Seq.take 2

    consecutiveStates 
    |> Seq.map (fun x -> 
        x
        |> fst 
        |> Map.toSeq 
        |> Seq.groupBy (fun (p, s) -> 
            match s with 
            | ClosestTo (o, d) -> Some o
            | _ -> None
        )
        |> Map.ofSeq
        |> Map.filter ( fun k v -> k <> None )
        |> Map.map (fun k v -> v |> Seq.length )
        |> Map.toArray
        |> Array.sortByDescending snd
    )

let someStates = sim prodInput 80 |> Seq.toArray

let intersections= 
    someStates 
    |> Array.map ( fun st -> Set.ofArray st )
    |> Array.reduce ( Set.intersect )

let max = intersections |> Set.map snd |> Set.maxElement

