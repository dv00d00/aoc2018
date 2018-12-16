open System
open System.Text.RegularExpressions

let parseLine line = 
    let regex = "^position=<([- ]?\d+), ([- ]?\d+)> velocity=<([- ]?\d+), ([- ]?\d+)>$"

    let [|x;y;vx;vy;|]= 
        Regex.Match(line, regex).Groups 
        |> Seq.cast<Group> 
        |> Seq.skip 1
        |> Seq.map (fun g -> g.Value |> int32)
        |> Array.ofSeq

    ((x,y), (vx,vy))

let testInput = 
    [|
        "position=< 9,  1> velocity=< 0,  2>"
        "position=< 7,  0> velocity=<-1,  0>"
        "position=< 3, -2> velocity=<-1,  1>"
        "position=< 6, 10> velocity=<-2, -1>"
        "position=< 2, -4> velocity=< 2,  2>"
        "position=<-6, 10> velocity=< 2, -2>"
        "position=< 1,  8> velocity=< 1, -1>"
        "position=< 1,  7> velocity=< 1,  0>"
        "position=<-3, 11> velocity=< 1, -2>"
        "position=< 7,  6> velocity=<-1, -1>"
        "position=<-2,  3> velocity=< 1,  0>"
        "position=<-4,  3> velocity=< 2,  0>"
        "position=<10, -3> velocity=<-1,  1>"
        "position=< 5, 11> velocity=< 1, -2>"
        "position=< 4,  7> velocity=< 0, -1>"
        "position=< 8, -2> velocity=< 0,  1>"
        "position=<15,  0> velocity=<-2,  0>"
        "position=< 1,  6> velocity=< 1,  0>"
        "position=< 8,  9> velocity=< 0, -1>"
        "position=< 3,  3> velocity=<-1,  1>"
        "position=< 0,  5> velocity=< 0, -1>"
        "position=<-2,  2> velocity=< 2,  0>"
        "position=< 5, -2> velocity=< 1,  2>"
        "position=< 1,  4> velocity=< 2,  1>"
        "position=<-2,  7> velocity=< 2, -2>"
        "position=< 3,  6> velocity=<-1, -1>"
        "position=< 5,  0> velocity=< 1,  0>"
        "position=<-6,  0> velocity=< 2,  0>"
        "position=< 5,  9> velocity=< 1, -2>"
        "position=<14,  7> velocity=<-2,  0>"
        "position=<-3,  6> velocity=< 2, -1>"
    |]
    |> Array.map parseLine

type Light = Location * Velocity
and Location = int * int
and Velocity = int * int

let update ((x,y), (vx,vy)) = ((x + vx, y + vy), (vx, vy))
let updateAll = Array.map update

let lines (xys:_ [,]) = [|

    let height = xys |> Array2D.length1
    let width = xys |> Array2D.length2

    for x = 0 to height - 1 do
        let result = Array.zeroCreate width
        for y = 0 to width - 1 do
            result.[y] <- xys.[x,y]
        yield result
|]

let getBox xs =     
    xs |> Array.fold( fun (minx, maxx, miny, maxy) ((x,y), _) -> 
        ((min minx x), (max maxx x), (min miny y), (max maxy y))
    ) (Int32.MaxValue, Int32.MinValue,Int32.MaxValue,Int32.MinValue)

let draw (xs:Light[]) = 

    let (minx,maxx,miny,maxy) = getBox xs

    let width = abs (maxx - minx + 1 ) 
    let height = abs (maxy - miny + 1 ) 

    let screen = Array2D.create height width ' '

    xs 
    |> Array.iter (fun ((x,y), _) ->  
        let x = abs x - abs minx
        let y = abs y - abs miny
        screen.[y, x] <- '#'
    )

    let result = 

        screen 
        |> lines
        |> Array.map ( System.String )
        |> String.concat "\n"

    Console.WriteLine "================="
    Console.WriteLine result

    System.IO.File.WriteAllText  (@"E:\New folder\New Text Document.txt", result)
    
let area (minx,maxx,miny,maxy) = 
    abs (int64 (maxy - miny) * int64 (maxx - minx))

let answer1 input = 
    let area = getBox >> area

    Seq.unfold (fun st -> let next = updateAll st in Some (next,next)) input
    |> Seq.pairwise 
    |> Seq.skipWhile ( fun (s1,s2) -> area s2 < area s1)
    |> Seq.head
    |> fst
    |> draw

let answer2 input = 
    let area = getBox >> area

    Seq.unfold (fun st -> let next = updateAll st in Some (next,next)) input
    |> Seq.map area
    |> Seq.indexed
    |> Seq.pairwise
    |> Seq.skipWhile ( fun ((_, s1),(_, s2)) -> s2 < s1)
    |> Seq.head
    |> (snd >> fst)

let input = System.IO.File.ReadAllLines( __SOURCE_DIRECTORY__ + "\Day10.txt" ) |> Array.map parseLine

answer1 testInput
answer1 input

answer2 testInput
answer2 input
