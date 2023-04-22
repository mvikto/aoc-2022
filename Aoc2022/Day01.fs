module Aoc2022.Day01

open System.IO
 
type ElfInventory = { CalorieCounts: int [] }

type Input = { ElfInventories: ElfInventory [] }

module Input =
    let load path : Input =
        let lines = File.ReadLines path

        let elfs: ElfInventory seq =
            seq {
                use enumerator = lines.GetEnumerator()
                let buffer = ResizeArray()

                let flush () =
                    let elfInventory: ElfInventory = { CalorieCounts = buffer.ToArray() }
                    buffer.Clear()
                    elfInventory

                while enumerator.MoveNext() do
                    let line = enumerator.Current
                    if line = "" then yield flush () else buffer.Add(int line)

                yield flush ()
            }

        { ElfInventories = elfs |> Seq.toArray }

let solve inputPath =
    let input = Input.load inputPath

    let part1 =
        input.ElfInventories
        |> Seq.map (fun e -> e.CalorieCounts |> Array.sum)
        |> Seq.max

    printfn $"Part1 %A{part1}"

    let part2 =
        input.ElfInventories
        |> Seq.map (fun e -> e.CalorieCounts |> Array.sum)
        |> Seq.sortDescending
        |> Seq.take 3
        |> Seq.sum

    printfn $"Part2 %A{part2}"
