open System

module Step1 = 

    let step xs = 

        let inline eq a b = Char.ToLower a = Char.ToLower b
    
        let (|Upper|Lower|) x = 
            if Char.IsLower x then Upper else Lower

        let reacts a b = 
            match a,b with
            | Upper, Lower when eq a b -> true
            | Lower, Upper when eq a b -> true
            | _ -> false

        let rec go processed rest = 
            match rest with
            | a::b::rest when reacts a b -> ( List.rev processed ) @ rest
            | a::rest -> go ( a :: processed ) rest
            | _ -> List.rev processed

        go [] xs

    let unroll xs = 
    
        let xs = xs |> List.ofSeq

        xs |> Seq.unfold (fun polymerSoFar -> 
            let next = step polymerSoFar
            if next = polymerSoFar
                then None
                else Some (next, next)
        ) 
        |> Seq.last
        |> fun x -> x.Length

module Step2 = 

    let inline remove (xs: 'a array) i count : 'a array = 
        let result = Array.zeroCreate (xs.Length - count)
        System.Array.Copy(xs, 0, result, 0, i)
        System.Array.Copy(xs, i + count, result, i, xs.Length - i - count)
        result

    let step (xs: byte[]) = 

        let inline reacts (a:byte) (b:byte) = b - a = 32uy || a - b = 32uy

        seq {
            for i = 0 to xs.Length - 2 do
                if reacts xs.[i] xs.[i+1] then
                    yield remove xs i 2
        } 
        |> Seq.tryHead
        |> Option.defaultValue xs

    let unroll xs = 
        xs 
        |> Seq.unfold (fun polymerSoFar -> 
            let next = step polymerSoFar
            if next = polymerSoFar
                then None
                else Some (next, next)
        ) 
        |> Seq.last
        |> fun x -> x.Length

    let solution (xs:string) = 
    
        let az = [|'a'..'z'|] |> Array.map ( fun c -> 
            let s = c.ToString()
            let S = s.ToUpper()
            (s, S)
        )

        let inputs = 
            az
            |> Array.map (fun (c, C) -> xs.Replace(c, "").Replace(C, ""))
            |> Array.distinct
            |> Array.map (System.Text.Encoding.ASCII.GetBytes)

        inputs
            |> Array.Parallel.map unroll
            |> Array.min

let input = System.IO.File.ReadAllText( __SOURCE_DIRECTORY__ + "\Day5.txt" )

let answer1 = Step1.unroll input

let answer2 = Step2.solution input
