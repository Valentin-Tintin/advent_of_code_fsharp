open System.IO
open Microsoft.FSharp.Collections

let input = File.ReadAllText "input.txt"

let isNotDistinct (arr: char []) : bool =
    // <> == !=
    arr |> Seq.distinct |> Seq.length <> arr.Length

let getFirstNDistinctElements (input: string, n: int) =
    let firstDistinctStartingIndex =
        input
        |> Seq.windowed n
        |> Seq.takeWhile isNotDistinct
        |> Seq.length
    // offset by sliding window size
    firstDistinctStartingIndex + n


let part1 =
    getFirstNDistinctElements (input, 4)

let part2 =
    getFirstNDistinctElements (input, 14)


printf $"Part 1: {part1}\n"
printf $"Part 2: {part2}\n"
