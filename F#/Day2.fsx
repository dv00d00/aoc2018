open System.IO

let input = File.ReadAllLines( __SOURCE_DIRECTORY__ + "\Day2.txt" )

let answer1 = 

    let scanCode code = 
        let counts = 
            code 
            |> Seq.groupBy id 
            |> Seq.map (fun (k,v) -> v |> Seq.length)
            |> Seq.filter (fun x -> x = 3 || x = 2)
            |> Seq.toArray

        let inline int x = if x then 1 else 0

        let twos = counts |> Array.contains 2 |> int
        let threes = counts |> Array.contains 3 |> int
        (twos, threes)

    let inline sum' (a,b) (a',b') = (a+a',b+b')

    let (twos, threes) = 
        input
        |> Array.map scanCode
        |> Array.reduce sum'

    in twos * threes

let answer2 =

    let uniquePairs (xs : _ array ) = seq {
        for i = 0 to xs.Length-1 do
            for j = i+1 to xs.Length-1 do
                yield (xs.[i],xs.[j])
    }

    let singleDistinctElement xs ys  = 
    
        let diffs = 
            Seq.zip xs ys
            |> Seq.indexed
            |> Seq.choose (fun (idx, (x,y)) -> if x <> y then Some idx else None)
            |> Array.ofSeq

        match diffs with
        | [|x|] -> Some x
        | _ -> None

    uniquePairs input 
    |> Seq.pick ( fun (a,b) -> 
        singleDistinctElement a b 
        |> Option.map ( fun idx -> a.Remove(idx, 1) ) 
    )
