namespace Adventofcode2018

module Day09 =

    open System
    open System.Collections.Generic
    open System.Linq

    let playerCount = 411

    let lastMarbleValue = 71058L * 100L

    type Player =
        {
            Id : int
            mutable Score : int64 // ResizeArray<int>
        }

    type GameState =
        {
            CurrentMarbleIndex : int
            LastInsertedMarble : int64
            Marbles : ResizeArray<int64>
            Players : Player []
        }

    let initGame () =
        let gameState =
            {
                CurrentMarbleIndex = 0;
                LastInsertedMarble = 0L;
                Marbles = ResizeArray();
                Players = Array.init playerCount (fun i -> { Id = i + 1; Score = 0L })
            }
        gameState.Marbles.Add(0L)
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
        gameState.Players.[playerIndex].Score <- gameState.Players.[playerIndex].Score + nextMarbleToInsert
        let toRemoveIndex = (gameState.CurrentMarbleIndex - 7) % gameState.Marbles.Count
        let toRemoveIndexPos =
            if (toRemoveIndex < 0)
            then toRemoveIndex + gameState.Marbles.Count
            else toRemoveIndex
        gameState.Players.[playerIndex].Score <- gameState.Players.[playerIndex].Score + gameState.Marbles.[toRemoveIndexPos]
        gameState.Marbles.RemoveAt(toRemoveIndexPos)
        let currentMarbleIndex' = toRemoveIndexPos % gameState.Marbles.Count
        { gameState with CurrentMarbleIndex = currentMarbleIndex'
                         LastInsertedMarble = nextMarbleToInsert }

    let insertMarble gameState playerIndex =
        let nextMarbleToInsert = gameState.LastInsertedMarble + 1L
        if (nextMarbleToInsert % 23L) <> 0L
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
        let winner = Array.maxBy (fun p -> p.Score) gameState.Players
        winner.Score
