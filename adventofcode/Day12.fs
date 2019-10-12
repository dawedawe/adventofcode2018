namespace Adventofcode2018

module Day12 =

    open System.Linq

    [<Literal>]
    let InputFile = "Day12Input.txt"

    type Rule =
        {
            L2 : char
            L1 : char
            C  : char
            R1 : char
            R2 : char
            Y  : char
        }

    let parseRuleLine (line : string) : Rule =
        let l2 = line.[0]
        let l1 = line.[1]
        let c = line.[2]
        let r1 = line.[3]
        let r2 = line.[4]
        let y = line.[9]
        { L2 = l2; L1 = l1; C = c; R1 = r1; R2 = r2; Y = y }

    let parseStateLine (stateLine : string) =
        let startOfState = stateLine.IndexOfAny [|'.'; '#'|]
        let initialState = stateLine.[startOfState..].ToCharArray()
        let leftGrowingSpace = Array.init 10 (fun _ -> '.')
        let rightGrowingSpace = Array.init 10 (fun _ -> '.')
        let state = Array.concat [leftGrowingSpace; initialState; rightGrowingSpace]
        let indexes = [-10L .. -1L] @ [0L .. int64(initialState.Length - 1 + 10)] |> Array.ofList
        (state, indexes)

    let isMatchingRule l2 l1 c r1 r2 rule =
        rule.L2 = l2 &&
        rule.L1 = l1 &&
        rule.C = c &&
        rule.R1 = r1 &&
        rule.R2 = r2

    let findRule rules (state : char []) index =
        let l2 = state.[index - 2]
        let l1 = state.[index - 1]
        let c = state.[index]
        let r1 = state.[index + 1]
        let r2 = state.[index + 2]
        let predicate = isMatchingRule l2 l1 c r1 r2
        Array.find predicate rules

    let generation rules (state : char []) =
        let state' = Array.copy state
        for index in [2 .. (state.Length - 3)] do
            let matchingRule = findRule rules state index
            state'.[index] <- matchingRule.Y
        state'.CopyTo(state, 0)

    let calcSumOfPlants (indexes : int64 []) state =
        let sum = Array.zip indexes state
                  |> Array.filter (fun (index, c) -> c = '#')
                  |> Array.sumBy (fun (index, c) -> index)
        sum

    let resizeIfNeeded (state : char []) (indexes : int64 []) =
        let predicate = fun c -> c = '#'
        let firstPlantIndex = Array.findIndex predicate state
        let state', indexes' = if firstPlantIndex < 10
                               then
                                //    printfn "bigger left needed"
                                   let leftBuffer = Array.init 10 (fun _ -> '.')
                                   let leftEnlargedState = Array.append leftBuffer state
                                   let leftIndexesBuffer = [(indexes.[0] - 10L) .. (indexes.[0] - 1L)] |> Array.ofList
                                   let leftEnlargedIndexes = Array.append leftIndexesBuffer indexes
                                   (leftEnlargedState, leftEnlargedIndexes)
                                else if firstPlantIndex > 20
                                then
                                    // printfn "smaller left needed"
                                    let leftCuttedState = state.[10..]
                                    let leftCuttedIndexes = indexes.[10..]
                                    (leftCuttedState, leftCuttedIndexes)
                                else
                                    (state, indexes)
        let lastPlantIndex = Array.findIndexBack predicate state'
        let distanceOfLastPlantToEnd = state'.Length - lastPlantIndex
        let state'', indexes'' = if distanceOfLastPlantToEnd < 10
                                 then
                                    //  printfn "bigger right needed"
                                     let rightBuffer = Array.init 10 (fun _ -> '.')
                                     let rightEnlargedState = Array.append state rightBuffer
                                     let rightIndexesBuffer = [(indexes.Last() + 1L) .. (indexes.Last() + 10L)] |> Array.ofList
                                     let rightEnlargedIndexes = Array.append indexes' rightIndexesBuffer
                                     (rightEnlargedState, rightEnlargedIndexes)
                                 else if distanceOfLastPlantToEnd > 20
                                 then
                                    //  printfn "smaller right needed"
                                     let rightCuttedState = state'.[0 .. (state'.Length - 11)]
                                     let rightCuttedIndexes = indexes'.[0 .. (indexes'.Length - 11)]
                                     (rightCuttedState, rightCuttedIndexes)
                                 else
                                     (state', indexes')
        state'', indexes''

    let rec growGenerations (patterns : Map<int64, char []>) currentGen maxGen rules (state : char []) (indexes : int64 []) =
        if currentGen < maxGen
        then
            generation rules state
            let state', indexes' = resizeIfNeeded state indexes
            let currentGen' = currentGen + 1L
            let firstPlantIndex = Array.findIndex (fun c -> c = '#') state'
            let lastPlantIndex = Array.findIndexBack (fun c -> c = '#') state'
            let pattern = state'.[firstPlantIndex .. lastPlantIndex]
            if Map.exists (fun k v -> v = pattern) patterns
            then
                state', indexes', currentGen'
            else
                let patterns' = Map.add currentGen' pattern patterns
                growGenerations patterns' currentGen' maxGen rules state' indexes'
        else
            state, indexes, currentGen

    let day12 () =
        let lines = System.IO.File.ReadAllLines InputFile
        let initialStateLine = lines.[0]
        let ruleLines = lines.[2..]
        let state, indexes = parseStateLine initialStateLine
        let rules = Array.map parseRuleLine ruleLines
        let limit = 20L
        let state', indexes', lastGen = growGenerations Map.empty 0L limit rules state indexes
        let indexesOffset = limit - lastGen
        let movedIndexes = Array.map (fun i -> i + indexesOffset) indexes'
        calcSumOfPlants movedIndexes state'

    let day12Part2 () =
        let lines = System.IO.File.ReadAllLines InputFile
        let initialStateLine = lines.[0]
        let ruleLines = lines.[2..]
        let state, indexes = parseStateLine initialStateLine
        let rules = Array.map parseRuleLine ruleLines
        let limit = 50000000000L
        let state', indexes', lastGen = growGenerations Map.empty 0L limit rules state indexes
        let indexesOffset = limit - lastGen
        let movedIndexes = Array.map (fun i -> i + indexesOffset) indexes'
        calcSumOfPlants movedIndexes state'
