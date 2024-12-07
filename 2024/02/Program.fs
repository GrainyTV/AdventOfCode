open System.IO

let isGradualChange (report: string) : bool =
    let values = report.Split(' ') |> Array.map (int32)
    let differences = values |> Array.pairwise |> Array.map (fun (x, y) -> x - y)

    differences |> Array.forall (fun x -> x >= -3 && x <= -1)
    || differences |> Array.forall (fun x -> x >= 1 && x <= 3)

[<EntryPoint>]
let main _ : int32 =
    let input = File.ReadLines("input.txt")

    let workingReactors =
        input
        |> Seq.map isGradualChange
        |> Seq.map (fun x -> if x = true then 1 else 0)
        |> Seq.sum

    printfn $"Final sum: {workingReactors}"
    0
