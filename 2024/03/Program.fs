open System
open System.IO

type Token =
    | CharM
    | CharU
    | CharL
    | ParenLeft
    | ParenRight
    | Comma
    | Digit of d: char
    | Undefined

type ParseProperty(tokens) =
    member val Tokens: array<Token> = tokens with get, set
    member val Iter: int32 = 1 with get, set
    member val Previous: Token = Undefined with get, set
    member val LeftNumber: string = String.Empty with get, set
    member val RightNumber: string = String.Empty with get, set
    member val ThereWasComma: bool = false with get, set
    member val Done: bool = false with get, set

type ParseResult =
    struct
        val Shift: int32
        val Multiplication: Option<int32>

        new(s: int32) =
            { Shift = s
              Multiplication = Option.None }

        new(s: int32, m: int32) =
            { Shift = s
              Multiplication = Option.Some(m) }
    end

let charToToken (chr: char) : Token =
    match chr with
    | 'm' -> CharM
    | 'u' -> CharU
    | 'l' -> CharL
    | '(' -> ParenLeft
    | ')' -> ParenRight
    | ',' -> Comma
    | d when Char.IsDigit(d) -> Digit d
    | _ -> Undefined

let isDigit (token: Token) : bool =
    match token with
    | Digit _ -> true
    | _ -> false

let rec parseExpression (properties: ParseProperty) : ParseResult =
    if properties.Tokens.Length < 1 then
        ParseResult(properties.Iter)

    else
        let current = properties.Tokens[0]

        let isFinal =
            match current with
            | CharM when properties.Previous = Undefined -> false
            | CharU when properties.Previous = CharM -> false
            | CharL when properties.Previous = CharU -> false
            | ParenLeft when properties.Previous = CharL -> false

            | Digit d when properties.Previous = ParenLeft ->
                properties.LeftNumber <- properties.LeftNumber + string d
                false

            | Digit d when
                isDigit properties.Previous
                && properties.ThereWasComma = false
                && properties.LeftNumber.Length < 3
                ->
                properties.LeftNumber <- properties.LeftNumber + string d
                false

            | Comma when isDigit properties.Previous && properties.ThereWasComma = false ->
                properties.ThereWasComma <- true
                false

            | Digit d when properties.Previous = Comma && properties.ThereWasComma ->
                properties.RightNumber <- properties.RightNumber + string d
                false

            | Digit d when
                isDigit properties.Previous
                && properties.ThereWasComma
                && properties.RightNumber.Length < 3
                ->
                properties.RightNumber <- properties.RightNumber + string d
                false

            | ParenRight when isDigit properties.Previous && properties.ThereWasComma ->
                properties.Done <- true
                true

            | _ -> true

        if isFinal && properties.Done then
            ParseResult(properties.Iter, int32 properties.LeftNumber * int32 properties.RightNumber)

        elif isFinal then
            ParseResult(properties.Iter)

        else
            properties.Previous <- current
            properties.Iter <- properties.Iter + 1
            properties.Tokens <- properties.Tokens[1..]
            parseExpression (properties)

let rec evaluateLine (startIndex: int32) (results: ResizeArray<int32>) (tokens: array<Token>) : array<int32> =
    if startIndex >= tokens.Length then
        results |> Seq.toArray

    else
        let parsedSegment = parseExpression (ParseProperty(tokens[startIndex..]))

        if parsedSegment.Multiplication.IsSome then
            results.Add(parsedSegment.Multiplication.Value)

        evaluateLine (startIndex + parsedSegment.Shift) results tokens

[<EntryPoint>]
let main _ : int32 =
    let input = File.ReadLines("input.txt")

    let result =
        input
        |> Seq.map (fun str ->
            let tokens = str |> Seq.map charToToken |> Seq.toArray
            evaluateLine 0 (ResizeArray<int32>()) tokens)
        |> Seq.map (Array.sum)
        |> Seq.sum

    printfn $"Final sum: {result}"
    0
