open System
open System.IO
open Microsoft.FSharp.Collections
open Microsoft.FSharp.Core

let input =
    File.ReadAllLines "input.txt"
    |> Seq.map Seq.toList
    |> Seq.map (List.map(fun chara -> Int32.Parse(string chara) ) )
    |> List.ofSeq


let findVisibleFolder acc current =
    match acc with
    | lst, max when max < current -> (lst @ List.singleton (current, true), current)
    | lst, max when max >= current -> (lst @ List.singleton (current, false), max)
    | _ -> failwith "todo"



let newTest = [2;5;5;1;2]


let getViewingDistance (input: List<int>, max: int) =
    let distance = input |> List.takeWhile(fun x -> x <= max) |> List.length
    if distance = 0 then 1 else distance

let folder2 acc current = 
    match acc with
        | lst, [head] -> (lst @ List.singleton (head ,0)  , List<int>.Empty)
        | lst, head :: tail -> (lst @ List.singleton (head, getViewingDistance (tail, head)), tail) 
   

// (value, r ,l , up , down)

let getDistancesInRow row = 
    let viewingDistanceRight = fst (row |> List.fold folder2  (List<int * int>.Empty, row)) 
    let viewingDistanceLeft = fst (row |> List.rev |> List.fold folder2  (List<int * int>.Empty, row |> List.rev)) |> List.rev
    
    let zipped = viewingDistanceLeft |> List.zip viewingDistanceRight 
    zipped |> List.map(fun value -> match value with | ((i,l),(_,r)) -> (i, r, l))

let horizontalScores = input |> List.map getDistancesInRow
let verticalScores = input |> List.transpose |> List.map getDistancesInRow |> List.transpose

let mergeStuff (toMerge: List<int * int * int> * List<int * int * int>): List< int *int *int *int *int> =
    fst toMerge |> List.zip (snd toMerge) |> List.map(fun value -> match value with | (i,r,l),(_,u,d) -> (i,r,l,u,d))

let all = horizontalScores |> List.zip verticalScores |> List.map mergeStuff



let rec findVisibleDirectional (input: List<int>) : List<int * bool> =
    match input
          |> List.fold findVisibleFolder (List<int * bool>.Empty, -1)
        with
    | (lst, _) -> lst

let mergeTree x =
    match x with
    | (num, a), (_, b) -> (num, a || b)
    
let mergeTreeScore x =
    match x with
    | (num, a), (_, b) -> (num, a + b)

let findVisible (row: List<int>) : List<int * bool> =
    let leftToRight = findVisibleDirectional row

    let rightToLeft =
        findVisibleDirectional (row |> List.rev)
        |> List.rev

    leftToRight
    |> List.zip rightToLeft
    |> List.map mergeTree


let toList (input: (int * bool) list) : List<int * bool> = input

let mergeRows (input: List<int * bool> * List<int * bool>) =
    fst input
    |> List.zip (snd input)
    |> List.map mergeTree



let horizontal =
    input |> List.map findVisible

let vertical =
    input
    |> List.transpose
    |> List.map findVisible
    |> List.transpose
    |> List.map toList

let final =
    horizontal
    |> List.zip vertical
    |> List.map mergeRows

let count =
    final
    |> List.fold
        (fun a b ->
            a
            + (b
               |> List.fold
                   (fun ia ib ->
                       match ib with
                       | _, true -> ia + 1
                       | _, false -> ia)
                   0))
        0

printf $"Part 1: {count}\n"
