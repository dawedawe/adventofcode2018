namespace Adventofcode2018

module Day14 =

    open System.Collections.Generic
    open System.Linq

    [<Literal>]
    let Input = 47801

    let InputList = List<int>([0; 4; 7; 8; 0; 1])

    let initialScoreboard () = List<int>([3; 7])

    type State = {
        Board : List<int>
        Elve1Pos : int
        Elve2Pos : int
    }

    let makeRecipe state =
        let score1 = state.Board.[state.Elve1Pos]
        let score2 = state.Board.[state.Elve2Pos]
        let sum = score1 + score2
        (if sum >= 10 then [sum / 10] else []) |> state.Board.AddRange 
        [sum % 10] |> state.Board.AddRange
        let elve1steps = score1 + 1
        let elve2steps = score2 + 1
        let elve1pos' = (state.Elve1Pos + elve1steps) % state.Board.Count
        let elve2pos' = (state.Elve2Pos + elve2steps) % state.Board.Count
        { state with Elve1Pos = elve1pos'; Elve2Pos = elve2pos'; }

    let day14 () =
        let mutable state = { Board = initialScoreboard(); Elve1Pos = 0; Elve2Pos = 1 }
        while state.Board.Count - 10 <> Input do
            state <- makeRecipe state
        let last10 = state.Board.GetRange(Input, 10)
        let l : int list = List.ofArray (last10.ToArray())
        List.fold (fun s i -> s + i.ToString()) "" l

    let quitPredicate (board : List<int>) =
        if board.Count >= 7
        then
            let start1 = board.Count - 6
            let start2 = board.Count - 7
            if board.GetRange(start1, 6).SequenceEqual(InputList)
                then Some start1
            else if board.GetRange(start2, 6).SequenceEqual(InputList)
                then Some start2
            else
                None
        else
            None
    
    let rec foo state =
        let len = quitPredicate state.Board
        if Option.isSome len
        then len
        else
            makeRecipe state |> foo

    let day14Part2 () =
        let state = { Board = initialScoreboard(); Elve1Pos = 0; Elve2Pos = 1 }
        let len = foo state
        len.Value