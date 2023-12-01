open System
open System.IO

let fileString =
    File.ReadAllText "input.txt"
let splitStringByNewLine (str: string) : string [] = str.Split "\n"

let input =
 splitStringByNewLine fileString

let foundTripleCharNum (chars: char List): int Option =
    let str = String.Concat chars
    match str with
    | "one" -> Some 1
    | "two" -> Some 2
    | "six" -> Some 6
    | "ten" -> Some 10
    | _ -> None
    

let foundFourCharNum (chars: char List): int Option =
    let str = String.Concat chars
    match str with
    | "four" -> Some 4
    | "five" -> Some 5
    | "nine" -> Some 9
    | _ -> None

let foundFiveCharNum (chars: char List): int Option =
    let str = String.Concat chars
    match str with
    | "three" -> Some 3
    | "seven" -> Some 7
    | "eight" -> Some 8
    | _ -> None

let getXCharsInOrder (chars: char List) (count: int) (rev: bool): char List =
    if rev then chars |> List.take count |> List.rev else chars |> List.take count

let rec findFirstNum (chars: char List) (rev: bool) : int * tail: char List =
     match chars with
        | head :: _ when Char.IsNumber head -> (int) (Char.GetNumericValue head), chars
        | _ :: _ when
                    List.length chars >= 3 && let maybeNum = foundTripleCharNum (getXCharsInOrder chars 3 rev)
                    match maybeNum with
                        | Some _ -> true
                        | None -> false
                    -> ((getXCharsInOrder chars 3 rev) |> foundTripleCharNum).Value , chars
        | _ :: _ when
                    List.length chars >= 4 && let maybeNum = foundFourCharNum (getXCharsInOrder chars 4 rev) 
                    match maybeNum with
                        | Some _ -> true
                        | None -> false
                    -> ((getXCharsInOrder chars 4 rev) |> foundFourCharNum).Value , chars
        | _ :: _ when
                    List.length chars >= 5 && let maybeNum = foundFiveCharNum (getXCharsInOrder chars 5 rev)
                    match maybeNum with
                        | Some _ -> true
                        | None -> false
                    -> (getXCharsInOrder chars 5 rev |> foundFiveCharNum).Value , chars             
        | head :: tail when not (Char.IsNumber head)-> findFirstNum tail rev
        | _ -> -1, []

let part2 =
     input
     |> Seq.map Seq.toList
     |> Seq.map (fun a -> findFirstNum a false)
     |> Seq.map (fun (num, tail) -> num , fst(findFirstNum (tail |> List.rev) true))
     |> Seq.map (fun (a, b) -> string a + string b)
     |> Seq.map Int32.Parse
     |> Seq.sum  

    
printf $"{part2}"