namespace Adventofcode2018

module Day15 =

    open System.Linq

    [<Literal>]
    let InputFile = "Day15Input.txt"

    [<Literal>]
    let InitialHitPoints = 200

    [<Literal>]
    let AttackPower = 3

    type Species =
    | Elve
    | Goblin

    type Position = { Y : int; X : int }

    type Unit = {
        Pos : Position
        HitPoints : int
        Species : Species
        AttackPower : int
    }

    type CaveState = {
        Field : char [][]
        mutable Units : Unit []
    }

    let symbolForSpecies = function
        | Goblin -> 'G'
        | Elve   -> 'E'

    let constructUnit species attackPower pos =
        { Pos = pos; HitPoints = InitialHitPoints; Species = species; AttackPower = attackPower }

    let getInitialCave path elveAttackPower =
        let lines = System.IO.File.ReadAllLines path
        let cave = Array.create lines.Length Array.empty
        let mutable units = Array.empty
        for i in [0 .. lines.Length - 1] do
            for j in [0 .. lines.[i].Length - 1] do
                let speciesAndAttackPower = if lines.[i].[j] = 'E' then Some (Elve, elveAttackPower)
                                            else if lines.[i].[j] = 'G' then Some (Goblin, AttackPower)
                                            else None
                if Option.isSome speciesAndAttackPower
                then let (species, attackPower) = speciesAndAttackPower.Value
                     let newUnit = constructUnit species attackPower {Y = i; X = j}
                     units <- Array.append units [|newUnit|]
            let a = lines.[i].ToCharArray()
            cave.[i] <- a
        { Field = cave; Units = units }
        
    // reading order
    let posCompare pos1 pos2 =
        if pos1 = pos2
        then 0
        else if pos1.Y = pos2.Y
             then
                if pos1.X < pos2.X
                then -1
                else 1
        else if pos1.Y < pos2.Y
        then -1
        else 1

    let isLegalPos (field : 'a [][]) (pos : Position) =
        pos.Y >= 0 && pos.X >= 0 && pos.Y < field.Length && pos.X < field.[0].Length

    let isOpenPos (field : char [][]) (pos : Position) =
        field.[pos.Y].[pos.X] = '.'

    let filterLegalOpenSpaces (field : char [][]) positions =
        Array.filter (fun p -> isLegalPos field p && field.[p.Y].[p.X] = '.' ) positions

    let getLegalAdjecentPositions field pos =
        let top = { Y = pos.Y - 1; X = pos.X }
        let left = { Y = pos.Y; X = pos.X - 1 }
        let right = { Y = pos.Y; X = pos.X + 1 }
        let bottom = { Y = pos.Y + 1; X = pos.X }
        let candidates = [| top; left; right; bottom; |]
        let predicate = isLegalPos field
        let legalAdjecentPositions = Array.filter predicate candidates
        legalAdjecentPositions

    let getOpenAdjecentSpaces field pos =
        let candidates = getLegalAdjecentPositions field pos
        let predicate = isOpenPos field
        let openSpaces = Array.filter predicate candidates
        openSpaces

    let findMovementTargets cave unit =
        let targets = cave.Units |> Array.filter (fun u -> u.Species <> unit.Species && u.HitPoints > 0)
        let spaces = Array.map (fun t -> getOpenAdjecentSpaces cave.Field t.Pos) targets
        Array.concat spaces

    let sortShortestWaysInReadingOrder ways =
        let sorted = Array.sortBy Array.length ways
        if Array.length sorted = 1
        then sorted.[0]
        else
            let length = sorted.[0].Length
            let equallyLong = Array.takeWhile (fun w -> Array.length w = length) sorted
            let sortedByReadingOrder = equallyLong.OrderBy(fun w -> w.[0].Y).ThenBy(fun w -> w.[0].X)
            Enumerable.ElementAt(sortedByReadingOrder, 0)

    let createField rows columns =
        Array.init rows (fun _ -> Array.init columns (fun _ -> None : Option<int>))

    let markPositions (distField : Option<int> [][]) positions n =
        let mutable marked = Array.empty
        for p in positions do
            if Option.isNone distField.[p.Y].[p.X]
            then
                distField.[p.Y].[p.X] <- Some n
                marked <- Array.append marked (Array.singleton p)
        marked

    let rec helper (field : char [][]) (distField : Option<int> [][]) (lastMarked : Position []) n (fromPos : Position) =
            let neighbours = Array.map (fun p -> getOpenAdjecentSpaces field p) lastMarked
                             |> Array.fold Array.append Array.empty
            let neighboursToMark = Array.filter (fun p -> Option.isNone distField.[p.Y].[p.X] ) neighbours
            if (not (Array.contains fromPos neighboursToMark)) && not (Array.isEmpty neighboursToMark)
            then
                let n' = n + 1
                let lastMarked' = markPositions distField neighboursToMark n'
                helper field distField lastMarked' n' fromPos

    let rec buildPath (soFar : Position []) (currentPos : Position) (distField : Option<int> [][]) =
        let neighbours = getLegalAdjecentPositions distField currentPos
                         |> Array.filter (fun n -> not(Array.contains n soFar))
        let possibleSteps = Array.filter (fun p -> Option.isSome distField.[p.Y].[p.X] ) neighbours
                            |> Array.sortBy (fun p -> distField.[p.Y].[p.X].Value)
        let minLength = distField.[possibleSteps.[0].Y].[possibleSteps.[0].X].Value
        let possibleMinSteps = Array.takeWhile (fun p -> distField.[p.Y].[p.X].Value = minLength ) possibleSteps
        let nextStep = Array.sortWith posCompare possibleMinSteps |> Array.head
        let path = Array.append soFar (Array.singleton nextStep)
        if distField.[nextStep.Y].[nextStep.X].Value = 0
        then path
        else buildPath path nextStep distField

    let findShortestWay (field : char [][]) fromPos toPos =
        let distField = createField field.Length field.[0].Length
        distField.[toPos.Y].[toPos.X] <- Some 0
        helper field distField (Array.singleton toPos) 0 fromPos
        let fromPosNeighbours = getLegalAdjecentPositions distField fromPos
        let starts = Array.filter (fun p -> Option.isSome distField.[p.Y].[p.X] ) fromPosNeighbours
        if (Array.isEmpty starts)
        then None
        else
            buildPath Array.empty fromPos distField |> Some

    let selectShortestWay field fromPos targets =
        if Array.isEmpty targets
        then None
        else
            let ways = Array.map (fun t -> findShortestWay field fromPos t) targets
                       |> Array.filter Option.isSome
                       |> Array.map (fun w -> w.Value)
            if ways.Length = 0
            then None            
            else
                let sortedWays = Array.sortBy Array.length ways
                let minLength = sortedWays.[0].Length
                let minWays = Array.takeWhile (fun w -> Array.length w = minLength) sortedWays
                if minWays.Length > 1
                then
                    let s = Array.sortWith (fun (w1 : Position []) w2 -> posCompare w1.[0] w2.[0] ) minWays
                    Some s.[0]
                else
                    Some minWays.[0]

    let getWeakestTargetNearby cave unit =
        let targetSpeciesSymbol = if unit.Species = Elve then 'G' else 'E'
        let candidates = getLegalAdjecentPositions cave.Field unit.Pos
        let targetPositions = Array.filter (fun p -> cave.Field.[p.Y].[p.X] = targetSpeciesSymbol) candidates
        let targetUnits = Array.filter (fun u -> Array.contains u.Pos targetPositions && u.HitPoints > 0) cave.Units
                          |> Array.sortBy (fun u -> u.HitPoints)
        if (Array.isEmpty targetUnits)
        then None
        else
            let minHitPoints = targetUnits.[0].HitPoints
            let withMinHitPoints = Array.takeWhile (fun u -> u.HitPoints = minHitPoints) targetUnits
            let readingOrdered = Array.sortWith (fun u1 u2 -> posCompare u1.Pos u2.Pos) withMinHitPoints
            readingOrdered.[0] |> Some

    let attack cave target attackPower =
        let attackedTarget = { target with HitPoints = target.HitPoints - attackPower }
        let idx = Array.findIndex (fun u -> u = target) cave.Units
        cave.Units.[idx] <- attackedTarget
        if attackedTarget.HitPoints <= 0
        then
            printfn "%A Y=%d X=%d killed" target.Species target.Pos.Y target.Pos.X
            cave.Field.[target.Pos.Y].[target.Pos.X] <- '.'

    let moveUnit cave unit newPos =
        let movedUnit = { unit with Pos = newPos }
        let idx = Array.findIndex (fun u -> u = unit) cave.Units
        cave.Units.[idx] <- movedUnit
        cave.Field.[unit.Pos.Y].[unit.Pos.X] <- '.'
        cave.Field.[newPos.Y].[newPos.X] <- (symbolForSpecies unit.Species)
        movedUnit

    let doTurn cave unit =
        let targetNearby = getWeakestTargetNearby cave unit
        if Option.isSome targetNearby then
            attack cave targetNearby.Value unit.AttackPower
        else
            let openAdjacentSpaces = findMovementTargets cave unit
            let way = selectShortestWay cave.Field unit.Pos openAdjacentSpaces
            if Option.isSome way
            then
                 let newPos = way.Value.[0]
                 let movedUnit = moveUnit cave unit newPos
                 let targetNearby' = getWeakestTargetNearby cave movedUnit
                 if Option.isSome targetNearby' then
                    attack cave targetNearby'.Value unit.AttackPower
        ()

    let areTargetsLeft cave =
        let livingElves = Array.filter (fun u -> u.Species = Elve && u.HitPoints > 0) cave.Units |> Array.length
        let livingGoblins = Array.filter (fun u -> u.Species = Goblin && u.HitPoints > 0) cave.Units |> Array.length
        livingElves > 0 && livingGoblins > 0

    let calcRound cave =
        let mutable fullRoundPlayed = true
        for i in [0 .. cave.Units.Length - 1] do
            let u = cave.Units.[i]
            if u.HitPoints > 0
            then
                if areTargetsLeft cave
                then doTurn cave u
                else fullRoundPlayed <- false
        let readingOrderUnits = Array.sortBy (fun u -> (u.Pos.Y, u.Pos.X)) cave.Units
        cave.Units <- readingOrderUnits
        fullRoundPlayed

    let displayField cave =
        for line in cave.Field do
            let s = Array.fold (fun sta c -> sta + string c) "" line
            printfn "%s" s
        for u in cave.Units do
            printfn "%A Y %d X %d: %d A: %d" u.Species u.Pos.Y u.Pos.X u.HitPoints u.AttackPower

    let play cave =
        let mutable r = 0
        while areTargetsLeft cave do
            if calcRound cave
            then
                r <- r + 1
        let sumOfHitPoints = cave.Units
                             |> Array.filter (fun u -> u.HitPoints > 0)
                             |> Array.sumBy (fun u -> u.HitPoints)
        sumOfHitPoints, r

    let day15 () =
        let cave = getInitialCave InputFile 3
        let s, r = play cave
        let o = s * r
        printfn "%d * %d = %d" s r (s * r)
        o

    let rec findMinElveAttackPower elveAttackPower =
        let cave = getInitialCave InputFile elveAttackPower
        let elvesToSurvive = Array.filter (fun u -> u.Species = Elve) cave.Units
                             |> Array.length
        let (s, r) = play cave
        let survivingElves = Array.filter (fun u -> u.Species = Elve && u.HitPoints > 0) cave.Units
                             |> Array.length
        if (survivingElves = elvesToSurvive)
        then
            (s, r, elveAttackPower)
        else findMinElveAttackPower (elveAttackPower + 1)
        
    let day15Part2 () =
        let s, r, a = findMinElveAttackPower 4
        let o = s * r
        printfn "elve attackpower %d" a
        printfn "%d * %d = %d" s r (s * r)
        o