open System.IO
open System

type Claim = { Id: int; Rect: Rect }
and Rect = { X: int; Y: int; Width: int; Height: int }

let parseClaim input = 

    let inline split (c:char) (s:string) = 
        s.Split(c |> Array.singleton , StringSplitOptions.RemoveEmptyEntries)

    let [|id; rect|] = input |> split '@'
    let id = id.Replace("#","").Trim() |> int32
    let [|pos;size|] = rect |> split ':'
    let [|x;y|] = pos |> split ','
    let [|width;height|] = size |> split 'x'
    let [|x;y;width;height|] = [|x;y;width;height|] |> Array.map (fun x -> x.Trim() |> int32)

    { 
        Id = id;
        Rect = { X = x; Y = y; Width = width; Height = height; }
    }

let intersects1D a b =
    let test (start1, end1) (start2, end2) =

        start1 >= start2 && start1 <= end2 
            || 
        end1 >= start2 && end1 <= end2

    test a b || test b a

let inline decompose r = 
    let xs = ( r.X, r.X + r.Width - 1 )
    let ys = ( r.Y, r.Y + r.Height - 1 )
    xs, ys

let intersects2D a b =
    let xs1, ys1 = decompose a
    let xs2, ys2 = decompose b

    intersects1D xs1 xs2 && intersects1D ys1 ys2

let cells rect = seq {
    for x in rect.X..(rect.X + rect.Width - 1) do
    for y in rect.Y..(rect.Y + rect.Height - 1) do
    yield (x,y)
}

let answer1 input = 

    input
    |> Array.map (fun x -> x.Rect)
    |> Seq.collect cells
    |> Seq.groupBy id
    |> Seq.filter (fun (_, items) -> items |> Seq.length > 1)
    |> Seq.length

let answer2 input = 
    seq {
        for claim in input do
            let rest = input |> Array.filter ((<>)claim)
            let intersections = 
                rest |> Array.filter (fun it -> intersects2D it.Rect claim.Rect)
            if intersections = [||] then
                yield claim
    }
    |> Seq.head

let input = File.ReadAllLines( __SOURCE_DIRECTORY__ + "\Day3.txt" ) |> Array.map parseClaim

answer1 input
answer2 input
