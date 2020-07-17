namespace Adventofcode2018

module Day20 =

    open System.Collections.Generic
    open System.Linq

    [<Literal>]
    let InputFile = "Day20Input.txt"

    type Symbols =
        | North
        | East
        | South
        | West
        | OpenBracket
        | CloseBracket
        | Or
        | Start
        | End

    let makeSymbol =
        function
        | 'N' -> North
        | 'E' -> East
        | 'S' -> South
        | 'W' -> West
        | '(' -> OpenBracket
        | ')' -> CloseBracket
        | '|' -> Or
        | '^' -> Start
        | '$' -> End
        | _ -> failwith "unsupported symbol"

    type Tile =
        | NSDoor
        | WEDoor
        | Room

    type Position =
        { X: int
          Y: int }

    let (|Direction|_|) s =
        match s with
        | d when d = North || d = East || d = South || d = West -> Some d
        | _ -> None

    let goNorth pos = { pos with Y = pos.Y + 1 }
    let goSouth pos = { pos with Y = pos.Y - 1 }
    let goWest pos = { pos with X = pos.X - 1 }
    let goEast pos = { pos with X = pos.X + 1 }

    let gotoRoom (map: Dictionary<Position, Tile>) currentPos d =
        let f, tile =
            match d with
            | North -> goNorth, NSDoor
            | East -> goEast, WEDoor
            | South -> goSouth, NSDoor
            | West -> goWest, WEDoor
            | _ -> failwith "bad argument"

        let doorPos = f currentPos
        let roomPos = f (f currentPos)
        map.[doorPos] <- tile
        map.[roomPos] <- Room
        roomPos

    let parseSymbols (symbols: Symbols list) =
        let stack = Stack<Position list * Set<Position>>()
        let map = Dictionary<Position, Tile>()

        let rec helper
                (currentPositions: Position list)
                (starts: Position list)
                (ends: Set<Position>)
                (todo: Symbols list) =

            match todo.[0] with
            | Start ->
                let initialPos =
                    { X = 0
                      Y = 0 }
                map.[initialPos] <- Room
                let currentPositions' = [ initialPos ]
                stack.Push((List.empty, Set.empty))
                helper currentPositions' starts ends todo.[1..]
            | OpenBracket ->
                stack.Push((starts, ends))
                let starts' = currentPositions
                let ends' = Set.empty
                helper currentPositions starts' ends' todo.[1..]
            | Direction d ->
                let currentPositions' = currentPositions |> List.map (fun p -> gotoRoom map p d)
                helper currentPositions' starts ends todo.[1..]
            | Or ->
                let ends' = Set.union (Set.ofList currentPositions) ends // add currentPositions to ends
                let currentPositions' = starts
                helper currentPositions' starts ends' todo.[1..]
            | CloseBracket ->
                let currentPositions' = Set.union (Set.ofList currentPositions) ends |> Set.toList
                let starts', ends' = stack.Pop()
                helper currentPositions' starts' ends' todo.[1..]
            | End -> ()
            | _ -> failwith "bad state"

        helper List.empty List.empty Set.empty symbols
        map

    let parseLine (line: string) =
        let rec helper sofar todo =
            match todo with
            | [] -> sofar
            | c :: rest -> helper (sofar @ [ (makeSymbol c) ]) rest

        let chars = line.ToCharArray() |> List.ofArray
        helper List.empty chars

    let getInput path =
        System.IO.File.ReadAllLines path
        |> Array.map parseLine
        |> Array.head

    type DoorPath =
        { Path: Position list
          Doors: int }

    let getPossibleNextDoors (map: Dictionary<Position, Tile>) (visitedMap: Dictionary<Position, int>) currentPos =
        let northDoor = goNorth currentPos
        let southDoor = goSouth currentPos
        let westDoor = goWest currentPos
        let eastDoor = goEast currentPos
        [ (northDoor, goNorth northDoor)
          (southDoor, goSouth southDoor)
          (westDoor, goWest westDoor)
          (eastDoor, goEast eastDoor) ]
        |> List.filter (fun (d, r) ->
            map.ContainsKey(d) && not (visitedMap.ContainsKey(r)) && (map.[d] = NSDoor || map.[d] = WEDoor))

    let bfs (map: Dictionary<Position, Tile>) =
        let queue = Queue<DoorPath>()
        let shortestPathMap = Dictionary<Position, int>()

        let startingPos =
            { X = 0
              Y = 0 }

        let initialDoorPath =
            { Path = [ startingPos ]
              Doors = 0 }

        queue.Enqueue initialDoorPath
        shortestPathMap.[startingPos] <- 0

        while (queue.Count <> 0) do
            let currentDoorPath = queue.Dequeue()
            let possibleDoorsAndRooms = getPossibleNextDoors map shortestPathMap (List.last currentDoorPath.Path)

            for (d, r) in possibleDoorsAndRooms do
                let path = currentDoorPath.Path @ [ r ]
                let doors = currentDoorPath.Doors + 1

                let currentDoorPath' =
                    { Doors = doors
                      Path = path }
                queue.Enqueue(currentDoorPath')
                shortestPathMap.[r] <- doors

        shortestPathMap

    let day20() =
        let s = getInput InputFile
        let map = parseSymbols s
        let map2 = bfs map
        map2.OrderBy(fun kv -> kv.Value).Last()
