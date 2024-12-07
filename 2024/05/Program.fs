open System
open System.IO

let rec canTraverseGraph (nums: array<int32>) (graph: Map<int32, ResizeArray<int32>>) : bool =
    if nums.Length = 1 then
        true

    else
        let current = nums[0]

        match graph.TryFind current with
        | Some list ->
            let next = nums[1]

            if list.Contains(next) then
                canTraverseGraph nums[1..] graph

            else
                false

        | None -> false

let rec keyValueToHashMap (input: array<int32 * int32>) (result: Map<int32, ResizeArray<int32>>) =
    if Array.isEmpty input then
        result

    else
        let key, value = input[0]

        let updatedResult =
            match Map.tryFind key result with
            | Some list ->
                list.Add(value)
                result

            | None -> result.Add(key, ResizeArray<int32>([ value ]))

        keyValueToHashMap input[1..] updatedResult

[<EntryPoint>]
let main _ : int32 =
    let input = File.ReadLines("input.txt")

    let keyValues =
        input
        |> Seq.filter (fun str -> str.Contains('|'))
        |> Seq.map (fun str ->
            let nums = str.Split('|')
            assert (nums.Length = 2)

            (int32 nums[0], int32 nums[1]))
        |> Seq.toArray

    let dependencyGraph = keyValueToHashMap keyValues Map.empty

    let updateSubSums =
        input
        |> Seq.filter (fun str -> not (str.Contains('|') || String.IsNullOrEmpty(str)))
        |> Seq.map (fun str ->
            let nums = str.Split(',') |> Array.map int32
            assert (nums.Length > 0 && not (nums.Length % 2 = 0))

            if canTraverseGraph nums dependencyGraph then
                nums[nums.Length / 2]

            else
                0)

    let result = updateSubSums |> Seq.sum
    printfn $"Final sum: {result}"
    0
