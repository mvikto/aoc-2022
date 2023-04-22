module Aoc2022.Day06

open System.Collections.Generic
open System.Diagnostics
open System.IO

let findMarker buffer windowSize =
    buffer
    |> Seq.windowed windowSize
    |> Seq.map Set.ofSeq
    |> Seq.findIndex (Set.count >> ((=) windowSize))
    |> (+) windowSize
 
 
let timed fn =
    let sw = Stopwatch.StartNew()
    let r = fn ()
    r, sw.Elapsed
 
let g a b = a < b

let solve path =
    let buffer = File.ReadAllBytes path
    let part1, part1Time = timed(fun () -> findMarker buffer 4)
    printfn $"part1 {part1} in %A{part1Time.Microseconds} us"
 
    let part2, part2Time = timed(fun () -> findMarker buffer 14)
    printfn $"part2 {part2} in {part2Time.Microseconds} us"
    ()
