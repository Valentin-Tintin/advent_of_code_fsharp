open System.IO
open System.Text.RegularExpressions

let fileString =
    File.ReadAllText "input.txt"
let splitStringByNewLine (str: string) : string [] = str.Split "\n"


let (|ParseGameId|_|)  input =
   let m = Regex.Match(input, "Game (\d+)")
   if (m.Success) then Some (int m.Groups.[1].Value) else None


let (|ParseColor|_|) color (input:string) =
    let m = Regex.Match(input, $"(?<count>\d*) {color}")
    if (m.Success) then Some (int m.Groups.["count"].Value) else None

let parseSetString (str: string) : int * int * int =
    str.Split(",") |> Seq.fold  (fun (r,g,b) splt -> match splt with
                                                        | ParseColor "red" red -> (red,g,b)
                                                        | ParseColor "green" green -> (r,green,b)
                                                        | ParseColor "blue" blue -> (r,g,blue)
                                                        | _ -> failwith "todo"
                                                        ) (0,0,0)


let lines = splitStringByNewLine fileString

let parseLine (line: string) : int * (int * int * int) List =
    let splitOnGame = line.Split(":")
    let gameId = match Seq.head splitOnGame with
                 | ParseGameId id -> id
                 | _ -> failwith "unknown game id"
    let sets = splitOnGame[1].Split(";") |> Seq.map parseSetString |> Seq.toList
    (gameId, sets)
    
let games = splitStringByNewLine fileString |> Seq.map parseLine

let part1 = games
            |> Seq.filter (fun x -> snd x
                                    |> Seq.forall (fun (r,g,b) -> (r <= 12 && g <= 13 && b <= 14)))
            |> Seq.map fst
            |> Seq.sum

let part2 = games
            |> Seq.map (fun game -> snd game
                                    |> Seq.fold (fun (r1,g1,b1) (r2,g2,b2) -> (max r1 r2, max g1 g2, max b1 b2)) (0,0,0))
            |> Seq.map (fun (r,g,b) -> r*g*b)
            |> Seq.sum 


printf $"{part1}\n"
printf $"{part2}"