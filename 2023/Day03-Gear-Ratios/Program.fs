open System
open System.IO

let fileString = File.ReadAllText "input.txt"
let splitStringByNewLine (str: string) : string[] = str.Split "\n"

let lines = splitStringByNewLine fileString |> Seq.toList

type number =
    { value: int
      xStart: int
      xEnd: int
      y: int }

type point =
    { x: int
      y: int
      value: char
      isSymbol: bool }

let (|ParseNumericValue|_|) (input: string) : int Option =
    let (isInt, value) = Int32.TryParse input
    if isInt then Some value else None

let (|ParseCharNumericValue|_|) (input: char) : int Option =
    match (string input) with
    | ParseNumericValue a -> Some a
    | _ -> None

let parsePoint (value: char) (x: int) (y: int) : point =
    let isSymbol =
        match value with
        | ParseCharNumericValue _ -> false
        | '.' -> false
        | _ -> true

    { x = x
      y = y
      value = value
      isSymbol = isSymbol }


let parseMapLine (line: string) (y: int) : point List =
    line
    |> Seq.toList
    |> Seq.indexed
    |> Seq.map (fun (x, value) -> parsePoint value x y)
    |> Seq.toList

let parseMap (lines: string List) : point List List =
    lines |> List.indexed |> List.map (fun (y, line) -> parseMapLine line y)


let createEngineNumberFromPoints (points: point List) : number =
    let start = points |> List.minBy (fun point -> point.x)
    let xEnd = points |> List.maxBy (fun point -> point.x)
    let y = (List.head points).y

    let value =
        match points |> Seq.fold (fun str point -> str + string point.value) "" with
        | ParseNumericValue num -> num
        | _ -> failwith "no number"

    { value = value
      xStart = start.x
      xEnd = xEnd.x
      y = y }

let parseEnginePartNumbers (line: point List) : number List =
    let x =
        line
        |> Seq.fold
            (fun (pointAgg, numbers) point ->
                match point.value with
                | ParseCharNumericValue _ when (point.x + 1).Equals(List.length line) ->
                    (List.empty<point>, numbers @ [ (createEngineNumberFromPoints (pointAgg @ [ point ])) ])
                | ParseCharNumericValue num -> (pointAgg @ [ point ], numbers)
                | _ when not (List.isEmpty pointAgg) ->
                    (List.empty<point>, numbers @ [ (createEngineNumberFromPoints pointAgg) ])
                | _ when List.isEmpty pointAgg -> (List.empty<point>, numbers)
                | _ -> failwith "todo")
            (List.empty<point>, List.empty<number>)

    snd x

let getAdjacentCoords (x: int, y: int) (maxX: int) (maxY: int) : (int * int) List =
    let all =
        seq { for x in x - 1 .. x + 1 -> seq { for y in y - 1 .. y + 1 -> (x, y) } }

    all
    |> Seq.collect id
    |> Seq.filter (fun coord ->
        match coord with
        | innerX, innerY when innerX < 0 || innerY < 0 -> false
        | innerX, innerY when innerX > maxX || innerY > maxY -> false
        | innerX, innerY when innerX.Equals x && innerY.Equals y -> false
        | _ -> true)
    |> Seq.toList

let map = parseMap lines
let maxX = List.length map[0] - 1
let maxY = List.length map - 1

let parts = map |> List.map parseEnginePartNumbers |> List.collect id

let enginePartNumberHasAdjacentSymbol (num: number) (maxX: int) (maxY: int) : bool =
    let cords = seq { for i in num.xStart .. num.xEnd -> (i, num.y) }

    not (
        cords
        |> Seq.map (fun coord -> getAdjacentCoords coord maxX maxY)
        |> Seq.collect id
        |> Seq.distinct
        |> Seq.filter (fun (x, y) -> (map[y][x]).isSymbol)
        |> Seq.isEmpty
    )


let validParts =
    parts |> Seq.filter (fun num -> enginePartNumberHasAdjacentSymbol num maxX maxY)

let part1 = validParts |> Seq.sumBy (fun num -> num.value)


let maybeGears =
    map |> List.collect id |> List.filter (fun point -> point.value.Equals '*')

let gears =
    maybeGears
    |> Seq.map (fun point ->
        let gearAdjacentCoords = getAdjacentCoords (point.x, point.y) maxX maxY

        let possibleParts =
            validParts
            |> Seq.filter (fun part ->
                let partCoords = seq { for i in part.xStart .. part.xEnd -> (i, part.y) }

                gearAdjacentCoords
                |> Seq.exists (fun gearCord -> partCoords |> Seq.contains gearCord))

        possibleParts)
    |> Seq.filter (fun partsList -> (Seq.length partsList).Equals 2)

let part2 =
    gears |> Seq.map (fun x -> (Seq.head x).value * (Seq.last x).value) |> Seq.sum

printf $"{part1}\n"
printf $"{part2}\n"
let stop = 1
