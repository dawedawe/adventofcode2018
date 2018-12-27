namespace Adventofcode2018
open System

module Day03 =

    open System.Text.RegularExpressions
    open System
    open System.Linq

    type Claim =
        {
            Id : int
            X : int
            Y : int
            Width : int
            Height : int
        }

    [<Literal>]
    let InputFile = "Day03Input.txt"

    [<Literal>]
    let FreeSquare = '.'

    [<Literal>]
    let TakenSquare = '#'

    [<Literal>]
    let MultiTakenSquare = 'X'


    let getInput () =
        System.IO.File.ReadAllLines InputFile
        |> List.ofArray

    let parse (s : string) =
        let regex = @"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)"
        let r = new System.Text.RegularExpressions.Regex(regex)
        let m = r.Match(s)
        {
            Id = int(m.Groups.[1].Value)
            X = int(m.Groups.[2].Value)
            Y = int(m.Groups.[3].Value)
            Width = int(m.Groups.[4].Value)
            Height = int(m.Groups.[5].Value)
        }

    let putOnFabric (fabric : char[,]) (claim : Claim) =
        for x in [0 .. (claim.Width - 1)] do
            for y in [0 .. (claim.Height - 1)] do
                let posX = claim.X + x
                let posY = claim.Y + y
                match fabric.[posX, posY] with
                | FreeSquare       -> fabric.[posX, posY] <- TakenSquare
                | TakenSquare      -> fabric.[posX, posY] <- MultiTakenSquare
                | MultiTakenSquare -> ()
                | _                -> failwith("unknown state")
        ()

    let initFabric () =
        Array2D.init 1000 1000 (fun _ _ -> FreeSquare)

    let countMultiTaken (fabric : char[,]) =
        let mutable s = 0
        Array2D.iter (fun c -> if c = MultiTakenSquare then s <- s + 1) fabric
        s

    let checkOverlap (fabric : char[,]) (claim : Claim) =
        let positions = System.Collections.Generic.List<Tuple<int,int>>()
        for x in [0 .. (claim.Width - 1)] do
            for y in [0 .. (claim.Height - 1)] do
                positions.Add (claim.X + x, claim.Y + y)
        let mutable overlapping = false
        positions.ForEach (fun p -> if (fabric.[fst p, snd p] = MultiTakenSquare) then overlapping <- true)
        not overlapping

    let day03 () =
        let lines = getInput()
        let claims = List.map parse lines
        let fabric = initFabric()
        List.map (putOnFabric fabric) claims |> ignore
        countMultiTaken fabric

    let day03Part2 () =
        let lines = getInput()
        let claims = List.map parse lines
        let fabric = initFabric()
        List.map (putOnFabric fabric) claims |> ignore
        List.filter (checkOverlap fabric) claims
        |> fun l -> l.[0].Id
        