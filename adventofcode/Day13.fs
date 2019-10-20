namespace Adventofcode2018

module Day13 =

    open System

    [<Literal>]
    let InputFile = "Day13Input.txt"

    type Turn =
    | Left
    | Straight
    | Right

    type Direction =
    | Up
    | Down
    | Left
    | Right

    type Position = {
        X : int
        Y : int
    }

    type Cart = {
        Direction : Direction
        Position : Position
        NextTurn : Turn
    }

    let cartSymbolToDirection =
        function
        | '<' -> Left
        | '^' -> Up
        | '>' -> Right
        | 'v' -> Down
        | _   -> Exception("bad state in cartSymbolToDirection") |> raise

    let nextTurn = 
        function
        | Turn.Left -> Straight
        | Straight -> Turn.Right
        | Turn.Right -> Turn.Left

    let parseInput path =
        let lines = System.IO.File.ReadAllLines path
        let xLength = lines.[0].Length
        let yLength = lines.Length
        let field = Array2D.create xLength yLength ' '
        let mutable carts = Array.empty
        let cartSymboles = ['<'; '^'; '>'; 'v']
        for x in [0 .. xLength - 1] do
            for y in [0 .. yLength - 1] do
                let symbol = lines.[y].[x]
                if List.contains symbol cartSymboles
                then
                    let direction = cartSymbolToDirection symbol
                    let cart = { Direction = direction; Position = { X = x; Y = y }; NextTurn = Turn.Left }
                    carts <- Array.append carts [|cart|]
                    field.[x, y] <- if direction = Direction.Up || direction = Direction.Down then '|' else '-'
                else
                    field.[x, y] <- symbol
        let orderedCarts = carts |> Array.sortBy (fun c -> c.Position.Y)
        (field, orderedCarts)

    let calcJunctionDirecion cart =
        match (cart.Direction, cart.NextTurn) with
        | (_, Turn.Straight) -> cart.Direction
        | (Direction.Up, Turn.Left) -> Direction.Left
        | (Direction.Up, Turn.Right) -> Direction.Right
        | (Direction.Down, Turn.Left) -> Direction.Right
        | (Direction.Down, Turn.Right) -> Direction.Left
        | (Direction.Left, Turn.Right) -> Direction.Up
        | (Direction.Left, Turn.Left) -> Direction.Down
        | (Direction.Right, Turn.Right) -> Direction.Down
        | (Direction.Right, Turn.Left) -> Direction.Up

    let calcNewDirectionAndTurn cart newTrackSymbol =
        match (newTrackSymbol, cart.Direction) with
        | ('-', _)      -> (cart.Direction, cart.NextTurn)
        | ('|', _)      -> (cart.Direction, cart.NextTurn)
        | ('/', Right)  -> (Direction.Up, cart.NextTurn)
        | ('/', Down)   -> (Direction.Left, cart.NextTurn)
        | ('/', Left)   -> (Direction.Down, cart.NextTurn)
        | ('/', Up)     -> (Direction.Right, cart.NextTurn)
        | ('\\', Right) -> (Direction.Down, cart.NextTurn)
        | ('\\', Down)  -> (Direction.Right, cart.NextTurn)
        | ('\\', Left)  -> (Direction.Up, cart.NextTurn)
        | ('\\', Up)    -> (Direction.Left, cart.NextTurn)
        | ('+', _)      -> let newDirection = calcJunctionDirecion cart
                           let newNextTurn = nextTurn cart.NextTurn
                           (newDirection, newNextTurn)
        | _             -> let message = String.Format("symbol {0} direction {1} position {2}", newTrackSymbol, cart.Direction, cart.Position)
                           Exception("bad state: " + message) |> raise

    let moveCart (field : char [,]) cart =
        let newPos = match cart.Direction with
                     | Up    -> { cart.Position with Y = cart.Position.Y - 1 }
                     | Down  -> { cart.Position with Y = cart.Position.Y + 1 }
                     | Left  -> { cart.Position with X = cart.Position.X - 1 }
                     | Right -> { cart.Position with X = cart.Position.X + 1 }
        let newTrackSymbol = field.[newPos.X, newPos.Y]
        let newDirection, newNextTurn = calcNewDirectionAndTurn cart newTrackSymbol
        { cart with Direction = newDirection; Position = newPos; NextTurn = newNextTurn }

    let detectCrash carts =
        let groups = Array.groupBy (fun c -> c.Position) carts
        let clashes = Array.filter (fun (k, g : Cart []) -> g.Length > 1) groups
        if (clashes.Length > 0)
        then Some (fst clashes.[0])
        else None

    let detectCrashes carts =
        let groups = Array.groupBy (fun c -> c.Position) carts
        let clashes = Array.filter (fun (k, g : Cart []) -> g.Length > 1) groups
        if (clashes.Length > 0)
        then Array.map fst clashes
        else Array.empty
        
    let tick field (carts : Cart []) =
        for i in [0 .. carts.Length - 1] do
            let movedCart = moveCart field carts.[i]
            carts.[i] <- movedCart
        ()

    let tickPart2 field (carts : Cart []) =
        for i in [0 .. carts.Length - 1] do
            let movedCart = if carts.[i].Position.X <> -1 then moveCart field carts.[i] else carts.[i]
            let crashVictim = Array.tryFindIndex (fun c -> c.Position = movedCart.Position) carts
            if Option.isSome crashVictim
            then
                carts.[crashVictim.Value] <- { carts.[crashVictim.Value] with Position = {X = -1; Y = -1;} }
                carts.[i] <- { carts.[i] with Position = {X = -1; Y = -1} }
            else
                carts.[i] <- movedCart
        Array.filter (fun c -> c.Position.X <> -1) carts

    let rec tickTillFirstCrash field carts =
        tick field carts
        let crashPos = detectCrash carts
        if Option.isSome crashPos
        then crashPos.Value
        else tickTillFirstCrash field carts

    let rec tickTillOneCartLeft field carts =
        if (Array.length carts > 1)
        then
            let orderedCarts = Array.sortBy (fun c -> c.Position.Y) carts
            let orderedCarts' = tickPart2 field orderedCarts
            tickTillOneCartLeft field orderedCarts'
        else
            carts.[0]

    let day13 () =
        let (field, carts) = parseInput InputFile
        let firstCrash = tickTillFirstCrash field carts
        firstCrash

    let day13Part2 () =
        let (field, carts) = parseInput InputFile
        let lastCartStanding = tickTillOneCartLeft field carts
        lastCartStanding
