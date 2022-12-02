open System.IO

let lines =
    File.ReadLines "input.txt"
   
let draw = 3
let win = 6

let calculateLineScore (line: char * char) : int=
    match line with
        |'A', A -> match A with | 'X' ->  1 + draw | 'Y' -> 2 + win |'Z' -> 3
        |'B', B -> match B with | 'X' ->  1 | 'Y' -> 2 + draw |'Z' -> 3 + win
        |'C', C -> match C with | 'X' ->  1 + win | 'Y' -> 2 |'Z' -> 3 + draw
    
    
let transformGameResult (line: char * char) : char * char=
    match line with
        |'A', A -> match A with | 'X' ->  ('A', 'Z') | 'Y' -> ('A', 'X') |'Z' -> ('A','Y')
        |'B', B -> match B with | 'X' ->  ('B', 'X') | 'Y' -> ('B', 'Y') |'Z' -> ('B','Z')
        |'C', C -> match C with | 'X' ->  ('C', 'Y') | 'Y' -> ('C', 'Z') |'Z' -> ('C','X')
    
    
let tuples = lines
            |> Seq.map(fun line -> line.Split " ")
            |> Seq.map(fun splt -> (char splt[0],char splt[1])) 
let part1 = tuples
            |> Seq.map calculateLineScore
            |> Seq.sum

let part2 = tuples
            |> Seq.map transformGameResult
            |> Seq.map calculateLineScore
            |> Seq.sum
            
printf $"Part1: {part1}\n"
printf $"Part2: {part2}"