namespace Adventofcode2018

module Day14 =

    [<Literal>]
    let Input = 47801

    let initialScoreboard () = [3; 7]

    type State = {
        Board : int list
        Elve1Pos : int
        Elve2Pos : int
    }

    let makeRecipe state =
        let score1 = state.Board.[state.Elve1Pos]
        let score2 = state.Board.[state.Elve2Pos]
        let sum = score1 + score2
        let firstAppend = if sum >= 10 then [sum / 10] else []
        let secAppend = [sum % 10]
        let board' = List.concat [state.Board; firstAppend; secAppend]
        let elve1steps = score1 + 1
        let elve2steps = score2 + 1
        let elve1pos' = (state.Elve1Pos + elve1steps) % board'.Length
        let elve2pos' = (state.Elve2Pos + elve2steps) % board'.Length
        { Board = board'; Elve1Pos = elve1pos'; Elve2Pos = elve2pos'; }

    let day14 () =
        let mutable state = { Board = initialScoreboard(); Elve1Pos = 0; Elve2Pos = 1 }
        while state.Board.Length - 10 <> Input do
            state <- makeRecipe state
        let last10 = state.Board.GetSlice(Some (Input), Some (Input + 9))
        last10