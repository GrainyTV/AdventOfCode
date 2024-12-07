open System.IO

[<EntryPoint>]
let main _ : int32 =
    let input = File.ReadLines("input.txt")

    let parsedLines =
        input
        |> Seq.map (fun str -> (str.Substring(0, 5) |> int32, str.Substring(8, 5) |> int32))

    let aColumn = parsedLines |> Seq.map (fun (x, y) -> x) |> Seq.sort
    let bColumn = parsedLines |> Seq.map (fun (x, y) -> y) |> Seq.sort

    let differences = Seq.zip aColumn bColumn |> Seq.map (fun (x, y) -> abs (x - y))
    let result = differences |> Seq.sum

    printfn $"Final sum: {result}"
    0
