namespace Adventofcode2018

module Day18 =

    [<Literal>]
    let InputFile = "Day18Input.txt"

    [<Literal>]
    let OpenGround = '.'
    [<Literal>]
    let Trees = '|'
    [<Literal>]
    let LumberYard = '#'
    
    type Area = char [][]

    let getArea path : Area =
        System.IO.File.ReadAllLines path
        |> Array.map (fun s -> s.ToCharArray())

    let isLegalPos (area : Area) (y, x) =
        y >= 0 && y < area.Length &&
        x >= 0 && x < area.[0].Length

    let getNeighbourPositions area (y, x) =
        let n = (y - 1, x)
        let ne = (y - 1, x + 1)
        let e = (y, x + 1)
        let se = (y + 1, x + 1)
        let s = (y + 1, x)
        let sw = (y + 1, x - 1)
        let w = (y, x - 1)
        let nw = (y - 1, x - 1)
        [| n; ne; e; se; s; sw; w; nw |]
        |> Array.filter (fun p -> isLegalPos area p)

    let getNeighbours area pos =
        getNeighbourPositions area pos
        |> Array.map (fun (y, x) -> area.[y].[x])

    let calcAcreTransition (area : Area) (y, x) =
        let neighbours = getNeighbours area (y, x)
        let acre = area.[y].[x]
        match acre with
        | OpenGround -> Array.sumBy (fun s -> if s = Trees then 1 else 0) neighbours
                        |> fun count -> if count >= 3 then Trees else OpenGround
        | Trees      -> Array.sumBy (fun s -> if s = LumberYard then 1 else 0) neighbours
                        |> fun count -> if count >= 3 then LumberYard else Trees
        | LumberYard -> let lumberCount = Array.sumBy (fun s -> if s = LumberYard then 1 else 0) neighbours
                        let treesCount = Array.sumBy (fun s -> if s = Trees then 1 else 0) neighbours
                        if lumberCount >= 1 && treesCount >= 1 then LumberYard else OpenGround
        | _ -> System.ArgumentException("bad area given") |> raise

    let calcAreaTransition (area : Area) =
        let area' = Array.copy area
        for i in 0 .. area.Length - 1 do
            area'.[i] <- Array.copy area.[i]

        for y in 0 .. area.Length - 1 do
            for x in 0 .. area.[0].Length - 1 do
                area'.[y].[x] <- calcAcreTransition area (y, x)
        area'
        
    let calcResources (area : Area) =
        let mutable lumberYardsCount = 0
        let mutable treesCount = 0
        for l in area do
            for c in l do
                if c = LumberYard then lumberYardsCount <- lumberYardsCount + 1
                else if c = Trees then treesCount <- treesCount + 1
        lumberYardsCount * treesCount

    let day18 () =
        let mutable area = getArea InputFile
        for _ in 1 .. 10 do
            area <- calcAreaTransition area
        calcResources area