open System.IO

type Vec2 = {
    X: int32
    Y: int32
}

let rec evaluatePairs (allPairs: array<Vec2>) (fst: int32) (snd: int32) (results: Set<Vec2>) =
    if fst >= allPairs.Length then
        results

    else if snd >= allPairs.Length then
        evaluatePairs allPairs (fst + 1) (fst + 2) results

    else
        let t1 = -1
        let t2 = 2
        
        let p1: Vec2 = { X = (1 - t1) * allPairs[fst].X + t1 * allPairs[snd].X; Y = (1 - t1) * allPairs[fst].Y + t1 * allPairs[snd].Y }
        let p2: Vec2 = { X = (1 - t2) * allPairs[fst].X + t2 * allPairs[snd].X; Y = (1 - t2) * allPairs[fst].Y + t2 * allPairs[snd].Y }

        let updatedResult1 = results.Add(p1)
        let updatedResult2 = updatedResult1.Add(p2)

        evaluatePairs allPairs fst (snd + 1) updatedResult2

let rec findPointsOfInterest (area: char[,]) (result: Map<char, ResizeArray<Vec2>>) (pos: Vec2) =
    if pos.X >= area.GetLength(0) then
        result

    else if pos.Y >= area.GetLength(1) then
        findPointsOfInterest area result { X = pos.X + 1; Y = 0 }

    else
        let updatedResult = 
            match area[pos.X, pos.Y] with
            | '.' -> result
            | _ as keyOption -> 
                match result.TryFind keyOption with
                | Some list -> list.Add({ X = pos.X; Y = pos.Y }); result
                | None -> result.Add(keyOption, ResizeArray<Vec2>([ { X = pos.X; Y = pos.Y } ]))

        findPointsOfInterest area updatedResult { X = pos.X; Y = pos.Y + 1 }

[<EntryPoint>]
let main _ : int32 =
    let input = File.ReadLines("input.txt") |> Seq.toArray

    let n = input.Length
    let area = Array2D.init n n (fun i j -> input[i][j])

    let sortedTowerMap = findPointsOfInterest area Map.empty { X = 0; Y = 0 }
    let designatedLocations = 
        sortedTowerMap
        |> Map.toSeq
        |> Seq.map snd
        |> Seq.map (fun towers -> evaluatePairs (towers.ToArray()) 0 1 Set.empty)
        |> Seq.collect id
        |> Set.ofSeq
        |> Set.filter (fun pos -> pos.X >= 0 && pos.X < n && pos.Y >= 0 && pos.Y < n)

    let result = designatedLocations.Count
    printfn $"Final sum: {result}"
    0
