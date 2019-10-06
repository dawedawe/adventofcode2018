namespace Adventofcode2018

module Day11 =

    open System.Linq

    let serial = 4151

    type Cell =
        {
            X : int
            Y : int
        }

    let calcPower serial cell =
        let rackId = cell.X + 10
        let level = rackId * cell.Y + serial
        let level' = level * rackId
        let level'' = (level' / 100) % 10
        let level''' = level'' - 5
        level'''

    let calcPowerOfSquare (topLeftX : int) (topLeftY : int) (grid : int [,]) squareSize =
        let maxIndex = squareSize - 1
        let mutable sum = 0
        for x in [0 .. maxIndex] do
            for y in [0 .. maxIndex] do
                sum <- sum + grid.[topLeftX + x, topLeftY + y]
        sum

    let findBestSquare grid squareSize =
        let maxIndex = Array2D.length1 grid - squareSize
        let squareValues = System.Collections.Generic.Dictionary<int * int, int>()
        for x in [0 .. maxIndex] do
            for y in [0 .. maxIndex] do
                let squareValue = calcPowerOfSquare x y grid squareSize
                squareValues.Add((x, y), squareValue)
        let orderedValues = squareValues.OrderByDescending(fun x -> x.Value)
        let keyValue = orderedValues.First()
        let x = fst keyValue.Key + 1
        let y = snd keyValue.Key + 1
        let value = keyValue.Value
        (x, y), value

    let day11 () =
        let myCalcPower = calcPower serial
        let grid = Array2D.create 300 300 0
                   |> Array2D.mapi (fun x y _ -> myCalcPower { X = x + 1; Y = y + 1})
        let (x, y), v = findBestSquare grid 3
        x, y, v

    let day11Part2 () =
        let myCalcPower = calcPower serial
        let grid = Array2D.create 300 300 0
                   |> Array2D.mapi (fun x y _ -> myCalcPower { X = x + 1; Y = y + 1})
        let sizeValues = System.Collections.Generic.Dictionary<int, (int * int) * int>()
        for squareSize in [1..300] do
            let best = findBestSquare grid squareSize
            sizeValues.Add(squareSize, best)
        let orderedSizeValues = sizeValues.OrderByDescending (fun x -> snd x.Value)
        orderedSizeValues.First()
