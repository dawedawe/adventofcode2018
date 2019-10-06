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
        let leftGrowingSpace = Array.init 50 (fun _ -> '.')
        let rightGrowingSpace = Array.init 50 (fun _ -> '.')
        Array.concat [leftGrowingSpace; initialState; rightGrowingSpace]

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

    let day12 () =
        let lines = System.IO.File.ReadAllLines InputFile
        let initialStateLine = lines.[0]
        let ruleLines = lines.[2..]
        let state = parseStateLine initialStateLine
        let rules = Array.map parseRuleLine ruleLines
        let indexes = [-50 .. -1] @ [0 .. 149] |> Array.ofList
        for _ in [1 .. 20] do
            generation rules state
        let sum = Array.zip indexes state
                  |> Array.filter (fun (index, c) -> c = '#')
                  |> Array.sumBy (fun (index, c) -> index)
        sum
