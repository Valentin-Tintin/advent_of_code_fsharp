
open System.IO
let input = File.ReadAllLines "input.txt"


let parseSet (rawSet: string) : Set<int> =
    let numbers = rawSet.Split "-"
    let numSeq = seq { Seq.head numbers |> int .. Seq.last numbers |> int }
    Set.ofSeq numSeq

let parseInputLine (line: string) : Set<int> * Set<int> =
    let splitLine =  line.Split ","
    Seq.head splitLine |> parseSet, Seq.last splitLine |> parseSet
    

let compareSets (a: Set<int>, b: Set<int>): bool =
    Set.isSubset a b || Set.isSubset b a
    
    
let compareIntersectingSets (a: Set<int>, b: Set<int>): bool =
    let intersections = Set.intersect a b
    intersections.Count > 0 
let part1 = input
            |> Seq.map parseInputLine
            |> Seq.where compareSets
            |> Seq.length
            
let part2 = input
            |> Seq.map parseInputLine
            |> Seq.where compareIntersectingSets
            |> Seq.length
           
           
printfn $"{part1}\n"
printfn $"{part2}"