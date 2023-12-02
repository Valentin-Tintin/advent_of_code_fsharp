open System
open System.IO
open System.Text.RegularExpressions
open Microsoft.FSharp.Core

let input = File.ReadAllText "input.txt"

let splitInput = input.Split "\n\n"

let cratesInput =
    (Seq.head splitInput).Split "\n"

let numberOfCrates =
    int (
        (Seq.last cratesInput).Trim().Split " "
        |> Seq.last
    )


let parseChar (charArr: char []) =
    let inputAsString = String(charArr)

    let found =
        Regex.Match(inputAsString, "[A-Z]")

    match found.Value with
    | "" -> None
    | rest -> Some(char rest)

let initialCrates =
    cratesInput
    |> Seq.rev
    |> Seq.tail
    |> Seq.rev
    |> Seq.map (fun x ->
        Seq.splitInto numberOfCrates x
        |> Seq.map parseChar)
    |> Seq.transpose
    |> Seq.map (fun inner ->
        inner
        |> Seq.rev
        |> Seq.choose (fun ch ->
            match ch with
            | Some x -> Some x
            | _ -> None)
        |> List.ofSeq)
    |> List.ofSeq
    

let parseMovementInstruction (input: string) : (int * int * int) =
    let split = input.Split " "
    (int(Seq.item 1 split),int(Seq.item 3 split),int(Seq.item 5 split))

let removeCrates (cratesInput: List<List<char>>, n: int, src: int) : List<List<char>> * List<char> =
    let temp = cratesInput[src]
    let length = temp |> List.length
    let removed = temp |> List.skip (length - n) |> List.rev
    let tail = temp |> List.take (length - n)    
    cratesInput |> List.updateAt src tail, removed

    
let removeCratesPart2 (cratesInput: List<List<char>>, n: int, src: int) : List<List<char>> * List<char> =
    let temp = cratesInput[src]
    let length = temp |> List.length
    let removed = temp |> List.skip (length - n)
    let tail = temp |> List.take (length - n)    
    cratesInput |> List.updateAt src tail, removed


let rec insertCrates (cratesInput: List<List<char>>, toAdd: List<char>, dest: int) :List<List<char>> =
    let temp = cratesInput[dest]
    let added = temp @ toAdd
       
    cratesInput |> List.updateAt dest added
    


let rec moveCratesPart1 (n: int, src: int, dest: int) input : List<List<char>> =
    let srcCorrected = src - 1
    let destCorrected = dest - 1
    let updatedCrates, removed =
        removeCrates (input, n, srcCorrected)
    
    insertCrates (updatedCrates, removed, destCorrected)

let rec moveCratesPart2 (n: int, src: int, dest: int) input : List<List<char>> =
    let srcCorrected = src - 1
    let destCorrected = dest - 1

    let updatedCrates, removed =
        removeCratesPart2 (input, n, srcCorrected)
    insertCrates (updatedCrates, removed, destCorrected)

    
    
let part1 =
    (Seq.last splitInput).Split "\n"
    |> Seq.map parseMovementInstruction
    |> Seq.fold (fun state n -> moveCratesPart1 n state)  initialCrates
  


let part2 =
    (Seq.last splitInput).Split "\n"
    |> Seq.map parseMovementInstruction
    |> Seq.fold (fun state n -> moveCratesPart2 n state ) initialCrates

for ch in part1 |> Seq.map Seq.last do
    printf $"{ch}"
    
printf "\n"

for ch in part2 |> Seq.map Seq.last do
    printf $"{ch}"

