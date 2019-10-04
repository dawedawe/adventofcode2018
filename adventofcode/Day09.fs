namespace Adventofcode2018

module Day09 =

    open System.Collections.Generic
    open System.Linq

    let playerCount = 411

    let lastMarbleValue = 71058

    type Player =
        {
            Id : int
            Scores : ResizeArray<int>
        }

    type GameState =
        {
            CurrentMarbleIndex : int
            LastInsertedMarble : int
            Marbles : ResizeArray<int>
            Players : Player []
        }

    let initGame () =
        let gameState =
            {
                CurrentMarbleIndex = 0;
                LastInsertedMarble = 0;
                Marbles = ResizeArray();
                Players = Array.init playerCount (fun i -> { Id = i + 1; Scores = List<int>() })
            }
        gameState.Marbles.Add(0)
        gameState

    let insertMarbleRight gameState nextMarbleToInsert =
        let nextCurrentMarbleIndex =
            if (gameState.Marbles.Count >= 2)
            then (gameState.CurrentMarbleIndex + 2) % gameState.Marbles.Count
            else 1
        gameState.Marbles.Insert(nextCurrentMarbleIndex, nextMarbleToInsert)
        { gameState with CurrentMarbleIndex = nextCurrentMarbleIndex;
                         LastInsertedMarble = nextMarbleToInsert }

    let scoreCase gameState nextMarbleToInsert playerIndex =
        gameState.Players.[playerIndex].Scores.Add(nextMarbleToInsert)
        let toRemoveIndex = (gameState.CurrentMarbleIndex - 7) % gameState.Marbles.Count
        let toRemoveIndexPos =
            if (toRemoveIndex < 0)
            then toRemoveIndex + gameState.Marbles.Count
            else toRemoveIndex
        gameState.Players.[playerIndex].Scores.Add(gameState.Marbles.[toRemoveIndexPos])
        gameState.Marbles.RemoveAt(toRemoveIndexPos)
        let currentMarbleIndex' = toRemoveIndexPos % gameState.Marbles.Count
        { gameState with CurrentMarbleIndex = currentMarbleIndex'
                         LastInsertedMarble = nextMarbleToInsert }

    let insertMarble gameState playerIndex =
        let nextMarbleToInsert = gameState.LastInsertedMarble + 1
        if (nextMarbleToInsert % 23) <> 0
        then
            insertMarbleRight gameState nextMarbleToInsert
        else
            scoreCase gameState nextMarbleToInsert playerIndex

    let rec play gameState nextPlayerIndex =
        if gameState.LastInsertedMarble = lastMarbleValue
        then gameState
        else
            let gameState' = insertMarble gameState nextPlayerIndex
            let nextPlayerIndex' = (nextPlayerIndex + 1) % playerCount
            play gameState' nextPlayerIndex'

    let day09 () =
        let startState = initGame()
        let gameState = play startState 0
        let winner = Array.maxBy (fun p -> p.Scores.Sum()) gameState.Players
        winner.Scores.Sum()
