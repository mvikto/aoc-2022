module Aoc2022.Day08

open System.IO

let ofLines (lines: string []) =
    let width = lines[0].Length
    let height = lines.Length
    Array2D.init width height (fun columnIndex rowIndex -> int (lines[columnIndex][rowIndex] - '0'))

let load path = File.ReadAllLines path |> ofLines

let getSample () =
    let sampleRaw =
        """30373
25512
65332
33549
35390"""

    sampleRaw |> String.splitChar '\n' |> ofLines

let countVisibleTrees arr =
    let visibleTrees (s: seq<_>) =
        seq {
            let mutable maxHeight = -1
            use e = s.GetEnumerator()

            while e.MoveNext() && maxHeight < 9 do
                let _, _, height = e.Current

                if height > maxHeight then
                    maxHeight <- height
                    yield e.Current
        }

    [ Array2D.ToSeq.rows
      Array2D.ToSeq.columns
      Array2D.ToSeq.rowsRev
      Array2D.ToSeq.columnsRev ]
    |> Seq.collect (fun f -> f arr |> Seq.collect visibleTrees)
    |> Seq.distinct
    |> Seq.length


type Tree = { Visibility: int; Height: int }

module Tree =
    let create height visibility =
        { Height = height
          Visibility = visibility }

let getTree state currentHeight =
    let rec go state idx =
        match state with
        | [] -> Tree.create currentHeight idx
        | blockingTree :: _ when blockingTree.Height >= currentHeight -> Tree.create currentHeight (idx + 1)
        | treeBeforeBlocking :: blockingTree :: _ when blockingTree.Height >= currentHeight ->
            Tree.create currentHeight (idx + treeBeforeBlocking.Visibility + 1)
        | _ :: xs -> go xs (idx + 1)

    match state with
    | [] -> Tree.create currentHeight 0
    | _ -> go state 0

let getVisibilityFromOneDirection s =
    seq {
        let mutable state = []

        for row, col, height in s do
            let tree = getTree state height
            state <- tree :: state
            yield row, col, tree.Visibility
    }


let findMaxScenicScore arr =
    [ Array2D.ToSeq.columns
      Array2D.ToSeq.columnsRev
      Array2D.ToSeq.rows
      Array2D.ToSeq.rowsRev ]
    |> Seq.map (fun f -> f arr)
    |> Seq.map (Seq.collect getVisibilityFromOneDirection)
    |> Seq.concat
    |> fun visibilities ->
        let scenicScore = Array2D.create (Array2D.length1 arr) (Array2D.length2 arr) 1
        visibilities
        |> Seq.iter (fun (row, col, v) -> scenicScore[row, col] <- scenicScore[row, col] * v)
        scenicScore
    |> fun scenicScore ->
        let mutable maxScenicScore = 0
        scenicScore |> Array2D.iter (fun v ->
            maxScenicScore <- max maxScenicScore v)
        maxScenicScore

let solve (path: string) =
    let arr = load path

    let answer = countVisibleTrees arr
    printfn $"part 1 is {answer}"

    findMaxScenicScore arr
    |> printfn "part2 answer is %A"
    ()
