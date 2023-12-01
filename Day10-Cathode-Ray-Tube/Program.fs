open System.IO

let input =
    File.ReadAllLines "input.txt" |> List.ofArray

type Command =
    | Noop of Cost: int
    | Add of Value: int * Cost: int


let parseCommand (line: string) : Command =
    let split = line.Split " "

    match split[0] with
    | "noop" -> Noop(1)
    | "addx" -> Add(int split[1], 2)
    | _ -> failwith "not covered"

let handleCommand (state: int * int, command: Command) : List<int * int> =
    let lastX, lastCycle = state

    match command with
    | Noop cost -> List.singleton (lastX, lastCycle + cost)
    | Add (value, cost) ->
        let lastX, lastCycle = state

        let processingCycles =
            List.init (cost - 1) (fun x -> (lastX, lastCycle + 1 + x))

        processingCycles
        @ List.singleton (lastX + value, (snd (List.last processingCycles)) + 1)


let initialState = List.singleton (1, 0)

let commands =
    input |> List.map parseCommand

let history =
    commands
    |> List.fold (fun acc current -> acc @ handleCommand (List.last acc, current)) initialState

let signals = [ 20; 60; 100; 140; 180; 220 ]

let inRange (index: int, register: int) =
    let range =
        seq { register - 1 .. register + 1 } |> List.ofSeq

    range |> List.contains index


let drawRow (row: List<int * int>) =
    row
    |> List.indexed
    |> List.map (fun pixel ->
        let index = fst pixel
        let register = fst (snd pixel)

        if inRange (index, register) then
            printf "#"
        else
            printf ".") |> ignore
    printf "\n"

let part1 =
    signals
    |> List.map (fun x -> (fst history[x - 1]) * x)
    |> List.sum

printf $"Part1: {part1} \n"
printf "Part2: \n"

let part2 =
    history |> List.chunkBySize 40 |> List.map drawRow

