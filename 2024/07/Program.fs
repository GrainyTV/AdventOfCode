open System.IO

let rec calculate (operators: string) (operands: array<uint64>) (result: uint64) =
    if operators.Length = 0 && Array.isEmpty operands then
        result

    else
        match operators[0] with
        | '*' when result = 0UL -> calculate (operators.Substring(1)) operands[2..] (operands[0] * operands[1])
        | '*' -> calculate (operators.Substring(1)) operands[1..] (operands[0] * result)
        | '+' when result = 0UL -> calculate (operators.Substring(1)) operands[2..] (operands[0] + operands[1])
        | '+' -> calculate (operators.Substring(1)) operands[1..] (operands[0] + result)
        | _ -> raise (System.ArgumentException("Operator can only be + or * char."))

let rec generateOperators (n: int32) : list<string> =
    if n = 0 then
        [ "" ]

    else
        let previousList = generateOperators (n - 1)
        List.collect (fun str -> [ str + "*"; str + "+" ]) previousList

let evaluateLine (line: string) : (uint64 * list<uint64>) =
    let parts = line.Split(':')
    assert (parts.Length = 2)

    let testResult = uint64 parts[0]
    let operands = parts[1].TrimStart().Split(' ') |> Array.map (uint64)
    let operatorOptions = generateOperators (operands.Length - 1)

    let evaluations =
        operatorOptions |> List.map (fun operators -> calculate operators operands 0UL)

    (testResult, evaluations)

[<EntryPoint>]
let main _ : int32 =
    let input = File.ReadLines("input.txt")
    let testCases = input |> Seq.map (evaluateLine)

    let result =
        testCases
        |> Seq.filter (fun case -> List.contains (fst (case)) (snd (case)))
        |> Seq.map (fst)
        |> Seq.sum

    printfn $"Final sum: {result}"
    0
