module Aoc2022.Day02

open System.IO

type Shape =
    | Rock
    | Paper
    | Scissors

module Shape =
    let score =
        function
        | Rock -> 1
        | Paper -> 2
        | Scissors -> 3

    let ofAbc =
        function
        | 'A' -> Rock
        | 'B' -> Paper
        | 'C' -> Scissors
        | _ -> failwith "not ABC"

    let ofXyz =
        function
        | 'X' -> Rock
        | 'Y' -> Paper
        | 'Z' -> Scissors
        | _ -> failwith "not XYZ"

    let values = [ Rock; Paper; Scissors ]

type Outcome =
    | Win
    | Loss
    | Draw

module Outcome =
    let ofXyz =
        function
        | 'X' -> Loss
        | 'Y' -> Draw
        | 'Z' -> Win
        | _ -> failwith "not XYZ"

    let score =
        function
        | Win -> 6
        | Draw -> 3
        | Loss -> 0

type Turn = { Mine: Shape; Opponents: Shape }

module Turn =
    let outcome turn =
        match turn.Mine, turn.Opponents with
        | Rock, Scissors
        | Scissors, Paper
        | Paper, Rock -> Win
        | m, o when o = m -> Draw
        | _ -> Loss

    let score turn =
        let outcomeScore = turn |> outcome |> Outcome.score
        let shapeScore = Shape.score turn.Mine
        outcomeScore + shapeScore

type Input = { Turns: Turn [] }

module Input =
    let load1 path =
        let turns =
            File.ReadLines path
            |> Seq.map (fun l ->
                { Opponents = Shape.ofAbc l.[0]
                  Mine = Shape.ofXyz l.[2] })
            |> Seq.toArray

        { Turns = turns }

    let load2 path =
        let turns =
            File.ReadLines path
            |> Seq.map (fun l ->
                let opponents = Shape.ofAbc l[0]
                let desiredOutcome = Outcome.ofXyz l[2]

                { Opponents = opponents
                  Mine =
                    Shape.values
                    |> Seq.find (fun shape ->
                        let turn: Turn = { Opponents = opponents; Mine = shape }
                        Turn.outcome turn = desiredOutcome) })
            |> Seq.toArray

        { Turns = turns }

let solve path =
    let input1 = Input.load1 path
    let part1 = input1.Turns |> Seq.sumBy Turn.score
    printfn $"Part1 {part1}"

    let input2 = Input.load2 path
    let part2 = input2.Turns |> Seq.sumBy Turn.score
    printfn $"Part2 {part2}"
