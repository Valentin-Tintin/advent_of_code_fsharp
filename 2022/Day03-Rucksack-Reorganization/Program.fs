﻿open System.IO
let input = File.ReadAllLines("input.txt")

let PriorityMap = seq { 1..26 }
                |> Seq.zip (seq { 'a' .. 'z' })
                |> Seq.append (seq { 27..52 } |> Seq.zip (seq { 'A' .. 'Z' }))
                |> Map.ofSeq

let splitLineInHalf (line: string) : string * string =
    (line[.. line.Length / 2 - 1], line[line.Length / 2 ..])

let findDuplicate (line: string * string) : char =
    let length = (fst line).Length

    let emptySeq =
        seq { for _ in 1..length -> 0 }

    let firstHalfMap =
        emptySeq |> Seq.zip (fst line) |> Map.ofSeq

    snd line
    |> Seq.fold
        (fun state current ->
            if firstHalfMap.ContainsKey(current) then
                state |> Seq.append (Seq.singleton current)
            else
                state)
        Seq.empty<char>
    |> Seq.head



let getPriorityValue (value: char) =
    PriorityMap[value]

let findCommonChar (chars: string[]): char =
    let set =  chars |> Seq.map Seq.toList |> Seq.map Set.ofList |> Set.intersectMany
    Seq.head set

let part1 =
    input
    |> Seq.map splitLineInHalf
    |> Seq.map findDuplicate
    |> Seq.map getPriorityValue
    |> Seq.sum

let part2 =
    input
    |> Seq.splitInto (input.Length/3)
    |> Seq.map findCommonChar
    |> Seq.map getPriorityValue
    |> Seq.sum

printf $"Part1: {part1}\n"
printf $"Part2: {part2}"
