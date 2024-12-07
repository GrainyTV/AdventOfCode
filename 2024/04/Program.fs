open System.IO

type Vec2 = { X: int32; Y: int32 }

let safeGet (m: char[,]) (x: int) (y: int) : char =
    if x >= 0 && x < m.GetLength(0) && y >= 0 && y < m.GetLength(1) then
        m[x, y]

    else
        char 0xA

let checkForOccurence (pos: Vec2) (m: char[,]) : int32 =
    let neighbors =
        [| { X = -1; Y = -1 }
           { X = -1; Y = 0 }
           { X = -1; Y = 1 }
           { X = 0; Y = 1 }
           { X = 1; Y = 1 }
           { X = 1; Y = 0 }
           { X = 1; Y = -1 }
           { X = 0; Y = -1 } |]

    neighbors
    |> Array.map (fun neighbor ->
        let c1: char = safeGet (m) (pos.X + neighbor.X) (pos.Y + neighbor.Y)
        let c2: char = safeGet (m) (pos.X + 2 * neighbor.X) (pos.Y + 2 * neighbor.Y)
        let c3: char = safeGet (m) (pos.X + 3 * neighbor.X) (pos.Y + 3 * neighbor.Y)
        string c1 + string c2 + string c3)
    |> Array.filter (fun str -> str = "MAS")
    |> Array.length


let rec search (idx: Vec2) (m: char[,]) (count: int32) : int32 =
    let i = idx.X
    let j = idx.Y

    if i >= m.GetLength(0) then
        count

    elif j >= m.GetLength(1) then
        search ({ X = i + 1; Y = 0 }) (m) (count)

    else
        match m[idx.X, idx.Y] with
        | 'X' -> search ({ X = i; Y = j + 1 }) (m) (count + checkForOccurence ({ X = i; Y = j }) (m))
        | _ -> search ({ X = i; Y = j + 1 }) (m) (count)

[<EntryPoint>]
let main _ : int32 =
    let input = File.ReadLines("input.txt") |> Seq.toArray
    assert (input.Length = input[0].Length)

    let n = input.Length
    let matrix = Array2D.init n n (fun i j -> input[i][j])

    let result = search ({ X = 0; Y = 0 }) (matrix) (0)
    printfn $"Final sum: {result}"
    0
