module Aoc2022.Day03

open System.IO

type Item = Item of c: char

module Item =
    let priority (Item c) : int =
        match c with
        | _ when c >= 'a' && c <= 'z' -> int (c - 'a') + 1
        | _ when c >= 'A' && c <= 'Z' -> int (c - 'A') + 1 + 26
        | _ -> failwith "bat item"

type Rucksack =
    { FirstCompartment: Item []
      SecondCompartment: Item [] }

type Input = { Rucksacks: Rucksack [] }

module Input =
    let load path =
        File.ReadLines path
        |> Seq.map (fun s ->
            s
            |> Seq.splitInto 2
            |> fun a ->
                let toItemArray = Array.map Item
                let first, second = a |> Seq.exactly2

                { FirstCompartment = toItemArray first
                  SecondCompartment = toItemArray second })
        |> Seq.toArray
        |> fun rucksacks -> { Rucksacks = rucksacks }

let solve path =
    let input = Input.load path

    let part1 =
        input.Rucksacks
        |> Seq.map (fun r ->
            r.FirstCompartment
            |> Array.find (fun item -> r.SecondCompartment |> Array.contains item))
        |> Seq.sumBy Item.priority

    printfn $"Part 1: {part1}"

    let part2 =
        input.Rucksacks
        |> Seq.chunkBySize 3
        |> Seq.sumBy (fun triplet ->
            triplet
            |> Seq.map (fun r -> r.FirstCompartment |> Seq.append r.SecondCompartment |> Set.ofSeq)
            |> Set.intersectMany
            |> Seq.exactlyOne
            |> Item.priority)

    printfn $"Part 2: {part2}"
