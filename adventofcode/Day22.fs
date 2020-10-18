namespace Adventofcode2018

module Day22 =

    open System
    open System.Collections.Generic

    [<Literal>]
    let InputFile = "Day22Input.txt"

    type RegionType =
        | Rocky
        | Wet
        | Narrow
        member this.ToInt() =
            match this with
            | Rocky -> 0
            | Wet -> 1
            | Narrow -> 2

    let getInput filePath =
        let lines = IO.File.ReadAllLines filePath
        let depth = lines.[0].Split([| ' ' |]).[1] |> Int32.Parse
        let targetPos =
            lines.[1].Split([| ' ' |]).[1].Split([| ',' |])
            |> fun a -> (Array.item 0 a |> Int32.Parse, Array.item 1 a |> Int32.Parse)
        depth, targetPos

    let rec calcErosionLevel x y depth target cache =
        let geoIndex = calcGeologicalIndex x y depth target cache
        (geoIndex + depth) % 20183

    and calcGeologicalIndex x y depth target (cache: Dictionary<(int * int), int>) =
        match (x, y) with
        | 0, 0 -> 0
        | _ when (x, y) = target -> 0
        | x, 0 -> x * 16807
        | 0, y -> y * 48271
        | _ -> if cache.ContainsKey (x, y)
               then cache.[(x,y)]
               else let v = (calcErosionLevel (x - 1) y depth target cache) * (calcErosionLevel x (y - 1) depth target cache)
                    cache.Add((x, y), v)
                    v

    let calcRegionType x y depth target (cache: Dictionary<(int * int), int>) =
        let erosionLevelMod = (calcErosionLevel x y depth target cache) % 3
        match erosionLevelMod with
        | 0 -> Rocky
        | 1 -> Wet
        | 2 -> Narrow
        | _ -> failwith "bad state"

    let calcTotalRiskLevel depth (targetX, targetY) =
        let cache = Dictionary<(int * int), int>()
        seq {
            for x in 0 .. targetX do
                for y in 0 .. targetY -> calcRegionType x y depth (targetX, targetY) cache |> fun t -> t.ToInt()
        }
        |> Seq.sum

    let day22() =
        let (depth, target) = getInput InputFile
        calcTotalRiskLevel depth target
