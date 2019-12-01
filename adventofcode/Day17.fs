namespace Adventofcode2018

module Day17 =

    open System.Collections.Generic
    open System.Linq

    [<Literal>]
    let InputFile = "Day17Input.txt"

    let Source = (0, 500)   // y, x

    type ScanLine =
        {
            FixedDim : char
            FixedPos : int
            RangeDim : char
            Range    : (int * int)
        }


    let parseScanLine (s : string) =
        let regex = @"(\w+)=(\d+), (\w+)=(\d+)..(\d+)"
        let r = System.Text.RegularExpressions.Regex(regex)
        let m = r.Match(s)
        {
            FixedDim = m.Groups.[1].Value.[0]
            FixedPos = int(m.Groups.[2].Value)
            RangeDim = m.Groups.[3].Value.[0]
            Range = (int(m.Groups.[4].Value), int(m.Groups.[5].Value))
        }

    let getScan path =
        let clayPositions = List<(int * int)>()
        let lines = System.IO.File.ReadAllLines path
        for line in lines do
            let scanLine = parseScanLine line
            if scanLine.FixedDim = 'y'
            then
                for x in [(fst scanLine.Range) .. (snd scanLine.Range)] do
                    clayPositions.Add(scanLine.FixedPos, x)
            else
                for y in [(fst scanLine.Range) .. (snd scanLine.Range)] do
                    clayPositions.Add(y, scanLine.FixedPos)
        clayPositions

    let constructField (clayPositions : List<int * int>) =
        let rows = clayPositions.Max (fun (y, _) -> y) + 1
        let columns = clayPositions.Max (fun (_, x) -> x) + 1
        let field = Array.init rows (fun _  -> Array.init columns (fun _ -> '.'))
        for (y, x) in clayPositions do
            field.[y].[x] <- '#'
        field

    let displayField field =
        let mutable i = 0
        for l in field do
            let s = Array.fold (fun s c -> s + string(c)) "" l
            printfn "LINE %4d: %s" i s
            i <- i + 1

    let isLegalx (field : char [][]) x =
        x >= 0 && x < field.[0].Length

    let isLegaly (field : char [][]) y =
        y >= 0 && y < field.Length

    let isLegalyx (field : char [][]) (y, x) =
        isLegaly field y && isLegalx field x

    let tickleDownPossible (field : char [][]) (y, x) =
        let y' = y + 1
        isLegalyx field (y', x) && (field.[y'].[x] = '.' || field.[y'].[x] = '|')

    let tickleDown (field : char [][]) (y, x) =
        field.[y].[x] <- '|'
        let mutable y' = y
        while tickleDownPossible field (y', x) do
            printfn "tickling down %d %d" y' x
            y' <- y' + 1
            field.[y'].[x] <- '|'
        (y', x)

    let aboveWaterOrClay (field : char [][]) (y, x) =
        let y' = y + 1
        isLegalyx field (y', x) && (field.[y'].[x] = '~' || field.[y'].[x] = '#')

    let spillSidewaysPossible (field : char [][]) (y, x) =
        let xLeft = x - 1
        let xRight = x + 1
        (isLegaly field y) &&
        ((isLegalx field xLeft && field.[y].[xLeft] = '.') || (isLegalx field xRight && field.[y].[xRight] = '.'))

    let spillLeft (field : char [][]) (y, x) =
        let mutable xLeft = x - 1
        while isLegalx field xLeft && field.[y].[xLeft] = '.' && aboveWaterOrClay field (y, xLeft) do
            field.[y].[xLeft] <- '|'
            xLeft <- xLeft - 1
        if isLegalyx field (y, xLeft) && field.[y].[xLeft] <> '#' then (y, xLeft) else (y, xLeft + 1)

    let spillRight (field : char [][]) (y, x) =
        let mutable xRight = x + 1
        while isLegalx field xRight && field.[y].[xRight] = '.' && aboveWaterOrClay field (y, xRight) do
            field.[y].[xRight] <- '|'
            xRight <- xRight + 1
        if isLegalyx field (y, xRight) && field.[y].[xRight] <> '#'  then (y, xRight) else (y, xRight - 1)

    let rec leftSideBlocked (field : char [][]) (y, x) =
        let x' = x - 1
        if not (isLegalx field x') || not (aboveWaterOrClay field (y, x'))
        then false
        else if field.[y].[x'] = '#'
        then true
        else leftSideBlocked field (y, x')

    let rec rightSideBlocked (field : char [][]) (y, x) =
        let x' = x + 1
        if not (isLegalx field x') || not (aboveWaterOrClay field (y, x'))
        then false
        else if field.[y].[x'] = '#'
        then true
        else rightSideBlocked field (y, x')

    let comeToRestPossible (field : char [][]) (y, x) =
        aboveWaterOrClay field (y, x) &&
        leftSideBlocked field (y, x) &&
        rightSideBlocked field (y, x)

    let comeToRest (field : char [][]) (y, x) =
        let mutable x' = x
        while (isLegalyx field (y, x') && field.[y].[x'] <> '#') do
            field.[y].[x'] <- '~'
            x' <- x' - 1
        x' <- x + 1
        while (isLegalyx field (y, x') && field.[y].[x'] <> '#') do
            field.[y].[x'] <- '~'
            x' <- x' + 1

    let findLeftBlocker (field : char [][]) (y, x) =
        let mutable x' = x
        while isLegalx field x' && field.[y].[x'] <> '#' do
            x' <- x' - 1
        if not (isLegalx field x')
        then System.Exception("no legal leftBlocker found") |> raise
        else x'

    let findRightBlocker (field : char [][]) (y, x) =
        let mutable x' = x
        while isLegalx field x' && field.[y].[x'] <> '#' do
            x' <- x' + 1
        if not (isLegalx field x')
        then System.Exception("no legal rightBlocker found") |> raise
        else x'

    let findTickleFlow (field : char [][]) (y, x) =
        let leftX = findLeftBlocker field (y, x)
        let rightX = findRightBlocker field (y, x)
        let y' = y - 1
        let mutable x' = leftX + 1
        while (isLegalx field x' && x' < rightX && field.[y'].[x'] <> '|') do
            x' <- x' + 1
        if isLegalyx field (y', x') && field.[y'].[x'] = '|' then Some (y', x') else None

    let rec moveWater (field : char [][]) (currentPos : int * int) =
        if not (isLegalyx field currentPos) || fst currentPos = field.Length - 1
        then ()
        else
            if tickleDownPossible field currentPos
            then
                printfn "tickleDownPossible"
                let currentPos' = tickleDown field currentPos
                moveWater field currentPos'
            else if spillSidewaysPossible field currentPos
            then
                printfn "spillSideWaysPossible"
                let posAfterLeftSpill = spillLeft field currentPos
                let posAfterRightSpill = spillRight field currentPos
                moveWater field posAfterLeftSpill
                moveWater field posAfterRightSpill
            else if comeToRestPossible field currentPos
            then
                printfn "comeToRestPossible"
                comeToRest field currentPos
                let ticklePos = findTickleFlow field currentPos
                if Option.isSome ticklePos
                then moveWater field ticklePos.Value
                else printfn "no tickleFlow found"
            else
                printfn "no move possible"
                ()

    let countWater minY maxY (field : char [][]) =
        let mutable sum = 0
        for i in minY .. maxY do
            let w = Array.sumBy (fun c -> if c = '~' || c = '|' then 1 else 0) field.[i]
            sum <- sum + w
        sum

    let day17 () =
        let scan = getScan InputFile
        let field = constructField scan
        moveWater field Source
        let minY = scan.Min(fun s -> fst s)
        let maxY = scan.Max(fun s -> fst s)
        let waterCount = countWater minY maxY field
        //displayField field
        waterCount