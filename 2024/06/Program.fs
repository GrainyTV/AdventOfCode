open System.IO

type Direction =
    | North = 0
    | East = 1
    | South = 2
    | West = 3

let rec findGuard (area: char[,]) (row: int32) (column: int32) : Option<int32 * int32> =
    if row >= area.GetLength(0) then
        Option.None

    elif column >= area.GetLength(1) then
        findGuard area (row + 1) 0

    elif area[row, column] = '^' then
        Option.Some(row, column)

    else
        findGuard area row (column + 1)

let rec checkGuardTravelPath
    (area: char[,])
    (location: int32 * int32)
    (facing: Direction)
    (result: Set<int32 * int32>)
    =
    let options =
        Map.ofList
            [ Direction.North, (-1, 0)
              Direction.East, (0, 1)
              Direction.South, (1, 0)
              Direction.West, (0, -1) ]

    let x = fst (location)
    let y = snd (location)

    let mutable xNew = x + fst (options[facing])
    let mutable yNew = y + snd (options[facing])

    if (xNew < 0 || xNew >= area.GetLength(0) || yNew < 0 || yNew >= area.GetLength(1)) then
        result.Add(location)

    elif area[xNew, yNew] = '#' then
        let updatedFacing = enum<Direction> ((int32 facing + 1) % 4)
        xNew <- x + fst (options[updatedFacing])
        yNew <- y + snd (options[updatedFacing])
        checkGuardTravelPath area (xNew, yNew) updatedFacing (result.Add(location))

    else
        checkGuardTravelPath area (xNew, yNew) facing (result.Add(location))

[<EntryPoint>]
let main _ : int32 =
    let input = File.ReadLines("input.txt") |> Seq.toArray

    let n = input.Length
    let area = Array2D.init n n (fun i j -> input[i][j])

    let guardLocation = findGuard area 0 0
    assert (guardLocation.IsSome)

    let distinctSpots =
        checkGuardTravelPath area guardLocation.Value Direction.North Set.empty

    let result = distinctSpots.Count
    printfn $"Final sum: {result}"
    0
