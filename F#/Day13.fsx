open System
open System.Threading

type Position = int * int
type Direction = Up | Down | Left | Right
type Orientation = Vertical | Horizontal
type TurnDirection = Left | Straight | Right

let turnTo direction turn = 
    match direction,turn with
    | Direction.Up, TurnDirection.Left -> Direction.Left
    | Direction.Up, TurnDirection.Right -> Direction.Right

    | Direction.Down, TurnDirection.Left -> Direction.Right
    | Direction.Down, TurnDirection.Right -> Direction.Left

    | Direction.Left, TurnDirection.Left -> Direction.Down
    | Direction.Left, TurnDirection.Right -> Direction.Up

    | Direction.Right, TurnDirection.Left -> Direction.Up
    | Direction.Right, TurnDirection.Right -> Direction.Down

    | direction, TurnDirection.Straight -> direction

let move (x,y) direction = 
    match direction with
    | Direction.Up -> (x, y - 1)
    | Direction.Down -> (x, y + 1)
    | Direction.Left -> (x - 1, y)
    | Direction.Right -> (x + 1, y)

type Track = 
    | Straight of Orientation
    | Intersection 
    | Turn of TurnKind
and TurnKind = LeanLeft | LeanRight

type Cart = { Direction: Direction; OnNextIntersection: TurnDirection }

type Board = char[,]
type Tracks = Map<Position, Track>
type Carts = Map<Position, Cart>

type Index = Carts * Tracks 

module Input =

    let parse path = 
        let chars = 
            System.IO.File.ReadAllLines path
            |> Array.map (fun x -> x.ToCharArray())

        Array2D.init (chars.Length) (chars.[0].Length) (fun x y -> chars.[x].[y])

    let test = parse (__SOURCE_DIRECTORY__ + @"\Day13_1.txt")
    let prod = parse (__SOURCE_DIRECTORY__ + @"\Day13_2.txt")
    let test2 = parse (__SOURCE_DIRECTORY__ + @"\Day13_3.txt")

let cells (board:Board) : Position seq = 
    seq {
        for x = 0 to board.GetUpperBound(0) do
        for y = 0 to board.GetUpperBound(1) do
            yield (x,y)
    }

let parse: Board -> Index =
    fun board -> 
    let zero = (Map.empty, Map.empty)
    
    let cart d = { Direction = d; OnNextIntersection = Left }

    let folder (carts, tracks) (x,y) = 
        let item = board.[x,y]
        match item with
        | '|' -> (carts, tracks |> Map.add (y,x) (Track.Straight Vertical))
        | '-' -> (carts, tracks |> Map.add (y,x) (Track.Straight Horizontal))
        | '+' -> (carts, tracks |> Map.add (y,x) (Track.Intersection))
        | '/' -> (carts, tracks |> Map.add (y,x) (Track.Turn LeanRight))
        | '\\' -> (carts, tracks |> Map.add (y,x) (Track.Turn LeanLeft))
        | '^' -> (
                    carts |> Map.add (y,x) (cart Direction.Up),
                    tracks |> Map.add (y,x) (Track.Straight Vertical)
                 )
        | 'v' -> (
                    carts |> Map.add (y,x) (cart Direction.Down),
                    tracks |> Map.add (y,x) (Track.Straight Vertical)
                 )
        | '>' -> (
                    carts |> Map.add (y,x) (cart Direction.Right),
                    tracks |> Map.add (y,x) (Track.Straight Horizontal)
                 )
        | '<' -> (
                    carts |> Map.add (y,x) (cart Direction.Left),
                    tracks |> Map.add (y,x) (Track.Straight Horizontal)
                 )
        | _ -> (carts, tracks)

    board 
    |> cells
    |> Seq.fold folder zero

let followTurn turn direction = 
    match (turn, direction) with
    | LeanLeft, Up -> Direction.Left
    | LeanLeft, Down -> Direction.Right
    | LeanLeft, Direction.Left -> Direction.Up
    | LeanLeft, Direction.Right -> Direction.Down

    | LeanRight, Up -> Direction.Right
    | LeanRight, Down -> Direction.Left
    | LeanRight, Direction.Left -> Direction.Down
    | LeanRight, Direction.Right -> Direction.Up

let moveCart tracks pos cart = 

    let track = tracks |> Map.find pos

    match track with
    | Straight (trackOrientation) -> 
        match trackOrientation, cart.Direction with
        | Vertical, Direction.Up -> (move pos Up, cart)
        | Vertical, Direction.Down -> (move pos Down, cart)
        | Horizontal, Direction.Left -> (move pos Direction.Left, cart)
        | Horizontal, Direction.Right -> (move pos Direction.Right, cart)
        | _ -> failwith "invariant broken"

    | Turn (turn) -> 
        let newDirection = followTurn turn cart.Direction 
        let newPosition = move pos newDirection
        ( newPosition, { cart with Direction = newDirection })

    | Intersection -> 

        let nextTurn = function 
        | TurnDirection.Left -> TurnDirection.Straight 
        | TurnDirection.Straight -> TurnDirection.Right 
        | TurnDirection.Right -> TurnDirection.Left

        let turn = cart.OnNextIntersection
        let direction = turnTo cart.Direction turn
        let position = move pos direction
            
        (position, { cart with Direction = direction; OnNextIntersection = nextTurn turn })

type CartsState = Colided of Position | OK of Carts
let moveCartsOnAtAtime tracks carts = 
    carts 
    |> Map.toArray
    |> Array.sortBy ( fun ((x,y), _) -> (y,x))
    |> Array.scan ( fun cartsState (pos,cart) -> 
        match cartsState with
        | OK carts -> 

            let (newPos, newCart) = moveCart tracks pos cart

            if carts |> Map.containsKey newPos then
                Colided newPos
            else
                OK (carts |> Map.remove pos |> Map.add newPos newCart)
        | other -> other
    ) (OK carts)

let moveCartsAndRemoveCollided tracks carts = 
    carts 
    |> Map.toArray
    |> Array.sortBy ( fun ((x,y), _) -> (y,x))
    |> Array.fold ( fun cartsState (oldPos, _) -> 

        match cartsState |> Map.tryFind oldPos with
        | Some cart ->

            let (newPos, newCart) = moveCart tracks oldPos cart

            if cartsState |> Map.containsKey newPos then
                // crash
                cartsState |> Map.remove newPos |> Map.remove oldPos
            else
                // move
                cartsState |> Map.remove oldPos |> Map.add newPos newCart
        | _ -> cartsState

    ) carts

let debug (carts, tracks) =
    let height = 149
    let width = 149

    let screen = Array.init height (fun _ -> Array.create width '.')

    for ((x,y), track) in tracks |> Map.toArray do
        let chr = 
            match track with
            | Track.Straight(Orientation.Vertical) -> '|'
            | Track.Straight(Orientation.Horizontal) -> '-'
            | Turn _ -> 'o'
            | Intersection -> '+'

        screen.[y].[x] <- chr

    for ((x,y), crt) in carts |> Map.toArray do

        let chr = match crt.Direction with
                  | Direction.Left -> '<'
                  | Direction.Right -> '>'
                  | Direction.Up -> '^'
                  | Direction.Down -> 'v'

        screen.[y].[x] <- chr
            
    let str = 
        let lines = screen |> Array.map System.String
        String.concat "\n" lines

    
    Console.WriteLine str
    Console.WriteLine ""
    Thread.Sleep 300

let simulation (carts, tracks)  = 
    let tick = moveCartsOnAtAtime tracks

    Seq.unfold (fun carts -> 
        let cartsDuringThisTurn = tick carts
        let carts = cartsDuringThisTurn |> Array.last
        match carts with
        | OK carts -> Some (None, carts)
        | Colided pos -> Some (Some pos, Map.empty)
    ) carts
    |> Seq.choose id
    |> Seq.head

let simulation2 (carts, tracks)  = 
    let tick = moveCartsAndRemoveCollided tracks

    Seq.unfold (fun carts -> 
       
        let carts = tick carts 

        if carts.Count = 0 then
            None
        elif carts.Count = 1 then
            let (pos, cart) = carts |> Map.toArray |> Array.head
            Some (Some pos, Map.empty)
        else
            Some (None, carts)
    ) carts
    |> Seq.choose id
    |> Seq.head


simulation (parse Input.prod)
simulation (parse Input.test)

simulation2 (parse Input.test2)
simulation2 (parse Input.prod)

Input.prod.GetUpperBound(0)
Input.prod.GetUpperBound(1)