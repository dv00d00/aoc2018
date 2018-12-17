type Header = Header of children:int * metadata:int
type Tree = Node of children:Tree list * metadata:int list
type Tape = int list

let testInput = 
    let raw = "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"
    let ints = raw.Split(' ') |> Array.map int32
    ints |> List.ofArray

let actualInput = 
    let raw = System.IO.File.ReadAllText( __SOURCE_DIRECTORY__ + "\Day8.txt" )
    let ints = raw.Split(' ') |> Array.map int32
    ints |> List.ofArray

module rec Solution = 

    let rec fold (Node (children, md)) : int = 
        let curSum = List.sum md
        let restSum = List.sumBy fold children
        curSum + restSum

    let rec parseChildren childrenCount (xs: Tape) : Tree list * Tape = 
            
        List.replicate childrenCount ()
        |> List.fold (fun (result, tape) _ -> 
            
            let node, tape = parseNode tape
            (result @ [node], tape)

        ) ([], xs)

    let rec parseNode (xs : int list) : Tree * Tape = 

        match xs with
        | c :: m :: rest -> 

            let (children:Tree list, rest) = parseChildren c rest 
            let metadata = rest |> List.take m
            let rest = rest |> List.skip m

            Node (children, metadata), rest
        
        | [] -> Node ([],[]), []

    let parseTree = Solution.parseNode >> fst
            
    let rec fold2 (Node (children, md)) : int = 

        match children with
        | [] -> List.sum md
        | _ -> 
            let md = md |> Array.ofList
            let cd = children |> Array.ofList

            md 
            |> Array.map ( fun idx -> cd |> Array.tryItem (idx - 1) |> Option.map fold2 |> Option.defaultValue 0 )
            |> Array.sum

        
    let answer1 input = 
        let tree = parseTree input
        fold tree

    let answer2 input = 
        let tree = parseTree input
        fold2 tree
      
        
Solution.answer1 actualInput
Solution.answer1 testInput

Solution.answer2 testInput
Solution.answer2 actualInput

