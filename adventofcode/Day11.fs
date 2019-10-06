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

    let calcPowerOfSquare (topLeftX : int) (topLeftY : int) (grid : int [,]) =
        let mutable sum = 0
        for x in [0 .. 2] do
            for y in [0 .. 2] do
                sum <- sum + grid.[topLeftX + x, topLeftY + y]
        sum

    let day11 () =
        let myCalcPower = calcPower serial
        let grid = Array2D.create 300 300 0
                   |> Array2D.mapi (fun x y _ -> myCalcPower { X = x + 1; Y = y + 1})
        
        let squareValues = System.Collections.Generic.Dictionary<int * int, int>()
        for x in [0 .. 297] do
            for y in [0 .. 297] do
                let squareValue = calcPowerOfSquare x y grid
                squareValues.Add((x, y), squareValue)

        let orderedValues = squareValues.OrderByDescending(fun x -> x.Value)
        let keyValue = orderedValues.First()
        let x = fst keyValue.Key + 1
        let y = snd keyValue.Key + 1
        (x, y)