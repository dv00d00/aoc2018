let state = "#..#.#..##......###...###".ToCharArray()
let rules = 
    [|
        "...##" , '#'
        "..#.." , '#'
        ".#..." , '#'
        ".#.#." , '#'
        ".#.##" , '#'
        ".##.." , '#'
        ".####" , '#'
        "#.#.#" , '#'
        "#.###" , '#'
        "##.#." , '#'
        "##.##" , '#'
        "###.." , '#'
        "###.#" , '#'
        "####." , '#'
    |] 
    |> Array.map (fun (a,b) -> (a.ToCharArray(), b))

let pentuplesOnInfinitePlane (p:char[]) = seq {
    yield [|'.';'.';'.';'.'; p.[0] |] // -2
    yield [|'.';'.';'.'; p.[0]; p.[1] |] // - 1
    yield [|'.';'.'; p.[0]; p.[1]; p.[2] |] // 0
    yield [|'.'; p.[0]; p.[1]; p.[2]; p.[3] |] // 1

    yield! p |> Array.windowed 5 

    let l = p.Length - 1

    yield [| p.[l-3]; p.[l-2]; p.[l-1]; p.[l]; '.' |]
    yield [| p.[l-2]; p.[l-1]; p.[l]; '.'; '.' |]
    yield [| p.[l-1]; p.[l]; '.'; '.'; '.' |]
    yield [| p.[l]; '.'; '.'; '.'; '.' |]
}



pentuplesOnInfinitePlane ( "1234567890".ToCharArray() )
|> Seq.length

let answer1 state rules count = 

    let (offset, plane) = 
        Seq.unfold (fun (offset, state) -> 

            let pents = state |> pentuplesOnInfinitePlane |> Array.ofSeq

            let raw = 
                pents
                |> Array.map (fun pent -> 
                    let matchinRule = rules |> Array.tryFind ( fun (rule, res) -> rule = pent )
                    matchinRule |> Option.map snd |> Option.defaultValue '.'
                ) 

            let hasPlant c = c = '#'

            let start = Array.findIndex hasPlant raw
            let end' = Array.findIndexBack hasPlant raw

            let nextState = raw.[start..end']

            let nextOffset = 
                let firstMatchIdx =
                    pents 
                    |> Array.findIndex ( fun pent -> 
                        rules |> Array.exists (fun (rule, c) -> rule = pent && c = '#')  
                    )
                offset + (firstMatchIdx - 2)

            let next = (nextOffset, nextState)

            Some (next, next)) (0,state)
        |> Seq.skip (count - 1)
        |> Seq.head

    //(offset, plane)

    plane 
    |> Array.mapi (fun idx it -> 
        if it = '#' 
            then idx + offset
            else 0 
        )
    |> Array.sum


let prodState = "##.#..########..##..#..##.....##..###.####.###.##.###...###.##..#.##...#.#.#...###..###.###.#.#".ToCharArray()
let prodRules = 
    [|
        "####.".ToCharArray(),'#'
        "##.#.".ToCharArray(),'.'
        ".##.#".ToCharArray(),'.'
        "..##.".ToCharArray(),'.'
        ".....".ToCharArray(),'.'
        ".#.#.".ToCharArray(),'#'
        ".###.".ToCharArray(),'.'
        ".#.##".ToCharArray(),'.'
        "#.#.#".ToCharArray(),'.'
        ".#...".ToCharArray(),'#'
        "#..#.".ToCharArray(),'#'
        "....#".ToCharArray(),'.'
        "###..".ToCharArray(),'.'
        "##..#".ToCharArray(),'#'
        "#..##".ToCharArray(),'#'
        "..#..".ToCharArray(),'.'
        "#####".ToCharArray(),'.'
        ".####".ToCharArray(),'#'
        "#.##.".ToCharArray(),'#'
        "#.###".ToCharArray(),'#'
        "...#.".ToCharArray(),'.'
        "###.#".ToCharArray(),'.'
        "#.#..".ToCharArray(),'#'
        "##...".ToCharArray(),'#'
        "...##".ToCharArray(),'#'
        ".#..#".ToCharArray(),'.'
        "#....".ToCharArray(),'.'
        "#...#".ToCharArray(),'.'
        ".##..".ToCharArray(),'#'
        "..###".ToCharArray(),'.'
        "##.##".ToCharArray(),'.'
        "..#.#".ToCharArray(),'#'
    |]

answer1 prodState prodRules 20
answer1 state rules 20

[200..300]
|> Seq.map (answer1 prodState prodRules)
|> Array.ofSeq
|> Array.pairwise
|> Array.map ( fun (a,b) -> b-a)

answer1 prodState prodRules 200

(50000000000L - 200L) * 45L + 9120L