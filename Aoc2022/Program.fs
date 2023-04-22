module Aoc2022.Program

open System.Reflection

[<EntryPoint>]
let main argv =
    let solveMethod =
        Assembly
            .GetExecutingAssembly()
            .GetType($"Aoc2022.Day{argv[0]}")
            .GetMethods()
        |> Seq.find (fun m -> m.Name = "solve")

    solveMethod.Invoke(null, [| $"input/{argv[0]}.txt" |]) |> ignore
    0
