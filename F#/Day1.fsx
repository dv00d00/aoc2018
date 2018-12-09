open System
open System.IO

let pint = Int32.Parse

let input = File.ReadAllLines( __SOURCE_DIRECTORY__ + "\Day1.txt" )

let answer1 = input |> Array.map pint |> Array.sum

let findAsnwerTwo input = 
    
    let rec repeat xs = seq {
        yield! xs
        yield! repeat xs
    }

    let sums = 
        input 
        |> Array.map pint
        |> repeat
        |> Seq.scan (+) 0

    sums
        |> Seq.scan (fun (acc, _) it -> let next = acc |> Set.add it in (next, Some it) ) (Set.empty, None)
        |> Seq.pairwise
        |> Seq.choose (fun ((left, _), (right, a)) -> if left.Count = right.Count then Some a else None)
        |> Seq.head

let answer2 = findAsnwerTwo input