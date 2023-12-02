open System
open System.IO
open Microsoft.FSharp.Core

let input = File.ReadAllLines "input.txt"


let parseLine (line: string) : int * int =
    let split = line.Split " "
    let value = int (split |> Array.last)

    match split |> Array.head with
    | "R" -> 0 + value, 0
    | "L" -> 0 - value, 0
    | "U" -> 0, 0 + value
    | "D" -> 0, 0 - value
    | _ -> failwith "todo"


let inputs =
    input |> Array.map parseLine |> List.ofArray


let HeadTail = ((0, 0), (0, 0))


let splitMoves (move: int * int) =
    match move with
    | x, 0 when x > 0 -> List.init x (fun _ -> (1, 0))
    | x, 0 when x < 0 -> List.init (-x) (fun _ -> (-1, 0))
    | 0, y when y > 0 -> List.init y (fun _ -> (0, 1))
    | 0, y when y < 0 -> List.init -y (fun _ -> (0, -1))

let moveInDirection (position: int * int, direction: int * int) : int * int =
    let positionX, positionY = position
    let directionX, directionY = direction
    (positionX + directionX, positionY + directionY)

let calcSingleMove (currentPosition: (int * int) * (int * int), move: int * int) : (int * int) * (int * int) =
    let head, (tailX, tailY) = currentPosition

    let newHeadPositionX, newHeadPositionY =
        moveInDirection (head, move)

    let diff =
        (newHeadPositionX - tailX, newHeadPositionY - tailY)

    
    let newTail =
        match diff with
        | x, y when Math.Abs x = 1 && Math.Abs y = 1  -> (tailX, tailY)
        | x, y when Math.Abs x + Math.Abs y <= 1 -> (tailX, tailY)
        | x, 0 when x > 0 && x % 2  = 0 -> (tailX + fst move, tailY)
        | 0, y when y > 0 && y % 2 = 0 -> (tailX, tailY + snd move)
        | x, y -> moveInDirection ((tailX, tailY), (x - fst move, y - snd move))

    ((newHeadPositionX, newHeadPositionY), newTail)


let calcMoves (currentPosition: (int * int) * (int * int), moves: List<int * int>) : List<(int * int) * (int * int)> =
    moves |> List.fold(fun acc move ->  calcSingleMove ((List.head acc),move) :: acc ) (List.singleton currentPosition)

let c = inputs |> List.fold (fun acc move ->  calcMoves(List.head acc, splitMoves move) @ acc   ) (List.singleton HeadTail) |> List.distinct  

let distinctPositions = c |> List.map(fun x -> snd  x) |>  List.distinct |> List.length

let testDiagonal = calcSingleMove (((4,4),(4,3)), (-1,0))



printf $"{distinctPositions}"
