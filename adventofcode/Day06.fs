namespace Adventofcode2018

open System
open System.Linq
open System.Collections.Generic
open System.Collections.Generic

module Day06 =

    [<Literal>]
    let InputFile = "Day06Input.txt"

    type Position =
        {
            X   : int
            Y   : int
        }

    type Coordinate =
        {
            Id  : int
            Pos : Position
        }

    let parseLine (line : string) =
        let parts = line.Split(',')
        let x, y = int(parts.[0]), int(parts.[1])
        x, y

    let parseInput lines =
        Array.map parseLine lines

    // let isFreeToWest (coordCandidate : int * int) (coordToCheck : int * int) =
    //     not (fst coordToCheck < fst coordCandidate && snd coordToCheck = snd coordCandidate)

    // let isFreeToEast (coordCandidate : int * int) (coordToCheck : int * int) =
    //     not (fst coordToCheck > fst coordCandidate && snd coordToCheck = snd coordCandidate)

    // let isFreeToNorth (coordCandidate : int * int) (coordToCheck : int * int) =
    //     not (fst coordToCheck = fst coordCandidate && snd coordToCheck < snd coordCandidate)

    // let isFreeToSouth (coordCandidate : int * int) (coordToCheck : int * int) =
    //     not (fst coordToCheck = fst coordCandidate && snd coordToCheck > snd coordCandidate)

    // let isFreeInDirection checkFunc (coordCandidate : int * int) (coordsToCheck : List<int * int>) =
    //     (List.map (checkFunc coordCandidate) coordsToCheck).All(fun b-> b = true)

    // let isFreeInAnyDirection (coordsToCheck : List<int * int>) (coordCandidate : int * int) =
    //     isFreeInDirection isFreeToWest coordCandidate coordsToCheck ||
    //     isFreeInDirection isFreeToEast coordCandidate coordsToCheck ||
    //     isFreeInDirection isFreeToNorth coordCandidate coordsToCheck ||
    //     isFreeInDirection isFreeToSouth coordCandidate coordsToCheck

    let distance (a : Position) (b : Position) =
        let xDist = a.X - b.X |> abs
        let yDist = a.Y - b.Y |> abs
        xDist + yDist

    let findNearest (a : Position) (coords) =
        let distances = List.map (fun b -> (b.Id, distance a b.Pos)) coords
                        |> List.sortBy snd
        if (snd distances.[0]) <> snd distances.[1]
        then Some (fst distances.[0])
        else None

    let isInfinite (field : int [,]) (id : int) =
        let upperBound = field.[*, 0]
        let lowerBound = field.[*, Array2D.length2 field - 1]
        let leftBound = field.[0, *]
        let rightBound = field.[Array2D.length1 field - 1, *]
        upperBound.Contains(id) ||
        lowerBound.Contains(id) ||
        leftBound.Contains(id) ||
        rightBound.Contains(id)

    let day06 () =
        let input = System.IO.File.ReadAllLines InputFile
        let parsedLines = Array.toList(parseInput input)
        let coordinates = List.zip [1..parsedLines.Length] parsedLines
                          |> List.map (fun (id, c) -> { Id = id; Pos = { X = fst c; Y = snd c }})
        let xMax = coordinates.OrderByDescending(fun c -> c.Pos.X).First().Pos.X
        let yMax = coordinates.OrderByDescending(fun c -> c.Pos.Y).First().Pos.Y
        let field = Array2D.init (xMax + 1) (yMax + 1) (fun _ _ -> 0)

        let dic = Dictionary<int, int>()
        for x in [0 .. Array2D.length1 field - 1] do
            for y in [0 .. Array2D.length2 field - 1] do
                let nearestId = findNearest { X = x; Y = y } coordinates
                match nearestId with
                | Some id -> field.[x, y] <- id
                             if dic.ContainsKey(id)
                             then dic.[id] <- dic.[id] + 1
                             else dic.[id] <- 1
                | None    -> field.[x, y] <- 0
        
        let largest = dic.OrderByDescending(fun kv -> kv.Value)
                         .Where(fun c -> not(isInfinite field c.Key))
                         .First()
        largest.Value
