namespace Adventofcode2018

module Day10 =

    [<Literal>]
    let InputFile = "Day10Input.txt"

    type Point =
        {
            Xpos : int
            Ypos : int
            Xvel : int
            Yvel : int
        }

    let parseLine line =
        let pattern = @"position=\<\s*(\-?\d+),\s*(\-?\d+)\> velocity=\<\s*(\-?\d+),\s*(\-?\d+)\>"
        let regex = System.Text.RegularExpressions.Regex(pattern)
        let m = regex.Match(line)
        {
            Xpos = int(m.Groups.[1].Value)
            Ypos = int(m.Groups.[2].Value)
            Xvel = int(m.Groups.[3].Value)
            Yvel = int(m.Groups.[4].Value)
        }

    let movePoint point =
        { point with Xpos = point.Xpos + point.Xvel; Ypos = point.Ypos + point.Yvel }

    let generateLine points lineLength xOffset =
        let line = Array.init lineLength (fun _ -> '.')
        for p in points do
            let index = p.Xpos + xOffset
            line.[index] <- '#'
        Array.fold (fun state item -> state + string(item)) "" line

    let displayPoints points =
        let minX = (Array.minBy (fun p -> p.Xpos) points).Xpos // left
        let maxX = (Array.maxBy (fun p -> p.Xpos) points).Xpos // right
        let minY = (Array.minBy (fun p -> p.Ypos) points).Ypos // up
        let maxY = (Array.maxBy (fun p -> p.Ypos) points).Ypos // down
        let xOffSet = abs minX
        let lineLength = maxX + 1 + xOffSet
        if lineLength > 500
        then
            printfn "skipping length %d" lineLength
        else
            let pointLines = points
                             |> Array.groupBy (fun p -> p.Ypos)
                             |> Array.sortBy (fun (k, _) -> k)
            for y in [minY .. maxY] do
                let pointsInY = Array.tryFind (fun (k, _) -> k = y) pointLines
                if Option.isSome pointsInY
                then
                    let sortedLinePoints = Array.sortBy (fun p -> p.Xpos) (snd pointsInY.Value)
                    let line = generateLine sortedLinePoints lineLength xOffSet
                    printfn "%s" line
                else
                    let line = String.init lineLength (fun _ -> ".")
                    printfn "%s" line

    let day10 () =
        let lines = System.IO.File.ReadAllLines InputFile
        let points = Array.map parseLine lines
        printfn "--------------------- Initially: ---------------"
        displayPoints points
        for s in [1 .. 100000] do
            printfn "--------------------- After second %d: ---------------" s
            for p in [0 .. points.Length - 1] do
                points.[p] <- movePoint points.[p]
            displayPoints points
        0