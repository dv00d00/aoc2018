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

let intersects1D (start1, end1) (start2, end2) =
    start1 >= start2 && start1 <= end2 ||
    end1 >= start2 && end1 <= end2

let intersection1D (start1, end1) (start2, end2) =
    if intersects1D (start1, end1) (start2, end2) then
        Some ( max start1 start2 , min end1 end2 )
    else
        None

let inline decompose r = 
    let xs = ( r.X, r.X + r.Width - 1 )
    let ys = ( r.Y, r.Y + r.Height - 1 )
    xs, ys

let intersection2D a b = 

    let xs1, ys1 = decompose a
    let xs2, ys2 = decompose b

    let ix = intersection1D xs1 xs2
    let iy = intersection1D ys1 ys2

    match (ix, iy) with
    | Some (x1, x2), Some(y1, y2) -> 
        let rect = { X = x1; Y = y1; Width = x2 - x1 + 1; Height = y2 - y1 + 1 }
        Some rect
    | _ -> None

let t1 = "#1 @ 1,3: 4x4" |> parseClaim;
let t2 = "#2 @ 3,1: 4x4" |> parseClaim;
let t3 = "#3 @ 5,5: 2x2" |> parseClaim;

let uniquePairs (xs : _ array ) = seq {
    for i = 0 to xs.Length-1 do
    for j = i+1 to xs.Length-1 do
        yield (xs.[i], xs.[j])
}

let inline area r = r.Width * r.Height

let cells rect = seq {
    for x in rect.X..(rect.X + rect.Width - 1) do
    for y in rect.Y..(rect.Y + rect.Height - 1) do
    yield (x,y)
}

cells { X = 1; Y = 2; Width = 1; Height = 2 }

let answer1 input = 

    let intersections = 
        input
        |> uniquePairs
        |> Seq.choose (fun (a,b) -> intersection2D a.Rect b.Rect)
        |> Array.ofSeq

    intersections
    |> Seq.collect cells
    |> Seq.distinct
    |> Seq.length


let input = File.ReadAllLines( __SOURCE_DIRECTORY__ + "\Day3.txt" ) |> Array.map parseClaim

answer1 input

