module Aoc2022.Day04

open System.IO

type SectionRange = { From: int; To: int }

module SectionRange =
    let create from to' = { From = from; To = to' }

    let contains other range =
        range.From <= other.From && range.To >= other.To

    let overlaps other range =
        let f a b = a.From <= b.From && a.To >= b.From
        f range other || f other range

type Input =
    { ElfPairs: (SectionRange * SectionRange) [] }

module Input =
    let load path =
        File.ReadLines path
        |> Seq.map (String.splitChar ',' >> (Array.map (String.splitChar '-' >> Array.map int)))
        |> Seq.map (function
            | [| [| s1From; s1To |]; [| s2From; s2To |] |] ->
                SectionRange.create s1From s1To, SectionRange.create s2From s2To
            | _ -> failwith "What")
        |> Seq.toArray
        |> fun pairs -> { ElfPairs = pairs }

let solve path =
    let input = Input.load path

    let part1 =
        input.ElfPairs
        |> Seq.filter (fun (a, b) -> SectionRange.contains b a || SectionRange.contains a b)
        |> Seq.length

    printfn $"Part1 : {part1}"

    let part2 =
        input.ElfPairs
        |> Seq.filter (fun (a, b) -> SectionRange.overlaps a b)
        |> Seq.length

    printfn $"Part2 : {part2}"
