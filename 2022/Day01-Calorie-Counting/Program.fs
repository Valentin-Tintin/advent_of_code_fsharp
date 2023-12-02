open System.IO

let fileString =
    File.ReadAllText "input.txt"

let splitStringByNewLine (str: string) : string [] = str.Split "\n"
let splitStringOnEmptyLine (str: string) : string [] = str.Split "\n\n"

let accCaloriesPerElf =
    splitStringOnEmptyLine fileString
    |> Seq.map splitStringByNewLine
    |> Seq.map (fun calorieStrings -> calorieStrings |> Seq.map int |> Seq.sum)

let maxCalories =
    accCaloriesPerElf |> Seq.max

let maxCaloriesTop3 =
    accCaloriesPerElf
    |> Seq.sortDescending
    |> Seq.take 3
    |> Seq.sum


printfn $"Part1: {maxCalories} \nPart2: {maxCaloriesTop3}"
