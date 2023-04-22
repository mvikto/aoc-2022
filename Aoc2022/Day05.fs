module Aoc2022.Day05

open System.IO

type Supplies = { Stacks: Map<int, list<char>> }

type MoveCrates = { Count: int; From: int; To: int }

module Supplies =
    let empty = { Stacks = Map.empty }

    let addCrate (stackIndex: int) (crate: char) (supplies: Supplies) : Supplies =
        supplies.Stacks
        |> Map.change stackIndex (function
            | Some crates -> crate :: crates |> Some
            | None -> Some [ crate ])
        |> fun stacks -> { Stacks = stacks }

    let moveCrates { Count = count; From = from; To = to' } (supplies: Supplies) : Supplies =
        let fromStack = supplies.Stacks |> Map.find from
        let toStack = supplies.Stacks |> Map.find to'

        let rec inner from to' count =
            match from with
            | fromTop :: fromRest when count > 0 -> inner fromRest (fromTop :: to') (count - 1)
            | _ -> from, to'

        let newFromStack, newToStack = inner fromStack toStack count

        supplies.Stacks
        |> Map.add from newFromStack
        |> Map.add to' newToStack
        |> fun stacks -> { Stacks = stacks }

    let moveCrates2 { Count = count; From = from; To = to' } (supplies: Supplies) : Supplies =
        let fromStack = supplies.Stacks |> Map.find from
        let toStack = supplies.Stacks |> Map.find to'

        let cratesToMove, newFromStack = fromStack |> List.splitAt count
        let newToStack = cratesToMove @ toStack

        supplies.Stacks
        |> Map.add from newFromStack
        |> Map.add to' newToStack
        |> fun stacks -> { Stacks = stacks }
 
type Input =
    { Supplies: Supplies
      Moves: list<MoveCrates> }

module Input =
    let load path =
        let text = File.ReadAllText path

        let stackPart, movePart =
            text |> String.split (String.replicate 2 String.newLine) |> Seq.exactly2

        let supplies =
            stackPart
            |> String.split String.newLine
            |> Seq.rev
            |> Seq.skip 1
            |> Seq.fold
                (fun supplies line ->
                    line
                    |> Seq.chunkBySize 4
                    |> Seq.indexed
                    |> Seq.fold
                        (fun supplies (crateIndex, crateString) ->
                            match crateString[1] with
                            | ' ' -> supplies
                            | c -> supplies |> Supplies.addCrate (crateIndex + 1) c)
                        supplies)
                Supplies.empty

        let moves =
            movePart
            |> String.split String.newLine
            |> Seq.map (fun line ->
                match line |> String.split " " with
                | [| "move"; count; "from"; from; "to"; to' |] ->
                    { Count = int count
                      From = int from
                      To = int to' }
                | _ -> failwith "oopsie")
            |> List.ofSeq

        { Supplies = supplies; Moves = moves }

let solve path =
    let input = Input.load path

    let solveWith mover =
        input.Moves
        |> Seq.fold mover input.Supplies
        |> fun s -> s.Stacks
        |> Map.values
        |> Seq.map List.head
        |> String.ofChars
 
    solveWith (fun supplies move -> supplies |> Supplies.moveCrates move)
    |> printfn "Part 1 : %A"

    solveWith (fun supplies move -> supplies |> Supplies.moveCrates2 move)
    |> printfn "Part 2 : %A"

