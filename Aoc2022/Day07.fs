module Aoc2022.Day07

[<RequireQualifiedAccess>]
type DirNode =
    | Directory of Map<string, DirNode>
    | File of size: int

type Crumb =
    { DirectoryName: string
      Directory: Map<string, DirNode> }

[<RequireQualifiedAccess>]
type DirNodeWithSize =
    | Directory of totalSize: int * directory: Map<string, DirNodeWithSize>
    | File of size: int

module DirNodeWithSize =
    let size =
        function
        | DirNodeWithSize.File size -> size
        | DirNodeWithSize.Directory (totalSize, _) -> totalSize

    let ofDirNode (dirNode: DirNode) =
        let rec inner: DirNode -> DirNodeWithSize =
            function
            | DirNode.File size -> DirNodeWithSize.File size
            | DirNode.Directory map ->

                let subNodes = map |> Map.map (fun _ -> inner)

                let size =
                    subNodes
                    |> Map.values
                    |> Seq.sumBy (function
                        | DirNodeWithSize.File size -> size
                        | DirNodeWithSize.Directory (size, _) -> size)

                DirNodeWithSize.Directory(size, subNodes)

        inner dirNode

    type WalkItem =
        { DirectoryName: string
          DirectorySize: int
          Directory: Map<string, DirNodeWithSize> }

    let walkDirectories (node: DirNodeWithSize) : seq<WalkItem> =
        let rec inner dirName =
            function
            | DirNodeWithSize.Directory (size, map) ->
                seq {
                    { DirectoryName = dirName
                      Directory = map
                      DirectorySize = size }

                    yield!
                        (map
                         |> Map.toSeq
                         |> Seq.collect (fun (dirName, dirNode) -> inner dirName dirNode))
                }
            | DirNodeWithSize.File size -> Seq.empty

        inner "/" node



type Parser = { Crumbs: List<Crumb> }


module Parser =
    let init =
        { Crumbs =
            [ { Directory = Map.empty
                DirectoryName = "/" } ] }

    let cdBack parser =
        match parser.Crumbs with
        | current :: parent :: rest ->
            let newCurrentCrumb =
                { parent with
                    Directory =
                        parent.Directory
                        |> Map.add current.DirectoryName (DirNode.Directory current.Directory) }

            { Crumbs = newCurrentCrumb :: rest }
        | _ -> failwith "expected for crumbs to contain at least two elements"

    let cdToDirectory (d: string) (parser: Parser) : Parser =
        match parser with
        | { Crumbs = current :: _ } ->
            { Crumbs =
                { DirectoryName = d
                  Directory =
                    current.Directory
                    |> Map.tryFind d
                    |> Option.map (function
                        | DirNode.Directory d -> d
                        | DirNode.File _ -> failwith "expected a directory")
                    |> Option.defaultValue Map.empty }
                :: parser.Crumbs }
        | _ -> failwith "expected for crumbs to not be empty"

    let rec cdBackWhileNotRoot parser =
        match parser with
        | { Crumbs = [ _ ] } -> parser
        | _ -> cdBackWhileNotRoot (cdBack parser)

    let addFile fileName size parser =
        match parser with
        | { Crumbs = current :: rest } ->
            let newCurrent =
                { current with
                    Directory =
                        current.Directory
                        |> Map.add fileName (DirNode.File size) }

            { Crumbs = newCurrent :: rest }
        | _ -> parser

let load path =
    let lines = System.IO.File.ReadAllLines path

    lines
    |> Seq.fold
        (fun parser line ->
            match String.split " " line with
            | [| "$"; "cd"; ".." |] -> Parser.cdBack parser
            | [| "$"; "cd"; "/" |] -> Parser.cdBackWhileNotRoot parser
            | [| "$"; "cd"; directoryName |] -> Parser.cdToDirectory directoryName parser
            | [| "$"; "ls" |] -> parser
            | [| "dir"; _ |] -> parser
            | [| fileSize; fileName |] -> Parser.addFile fileName (int fileSize) parser
            | tokens -> failwith $"what? %A{tokens}")
        Parser.init
    |> Parser.cdBackWhileNotRoot
    |> function
        | { Crumbs = [ root ] } -> DirNode.Directory root.Directory
        | shit -> failwith $"Expected a single element in parser but found %A{shit}"

let solve path =
    let dirNodeWithSize = path |> load |> DirNodeWithSize.ofDirNode

    let part1 =
        dirNodeWithSize
        |> DirNodeWithSize.walkDirectories
        |> Seq.filter (fun x -> x.DirectorySize <= 100000)
        |> Seq.sumBy (fun x -> x.DirectorySize)

    printfn $"part1 {part1}"


    let part2 =
        let totalDiskSpace = 70000000
        let needToFree = 30000000
        let currentOccupied = dirNodeWithSize |> DirNodeWithSize.size
        let needToDeleteAtLeast = needToFree - (totalDiskSpace - currentOccupied)

        dirNodeWithSize
        |> DirNodeWithSize.walkDirectories
        |> Seq.map (fun x -> x.DirectorySize)
        |> Seq.filter (fun x -> x >= needToDeleteAtLeast)
        |> Seq.min

    printfn $"part 2 {part2}"
    ()
