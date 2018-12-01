namespace Adventofcode2018

module Day01 =

    [<Literal>]
    let InputFile = "Day01Input.txt"

    let getInput () =
        System.IO.File.ReadAllLines InputFile
        |> List.ofArray

    let parseInput (lines : string list) =
        let f (line : string) =
            let op = match line.[0] with
                        | '+' -> (+)
                        | '-' -> (-)
                        | _   -> failwith "unknown input"
            let arg = line.[1..] |> int
            fun x -> op x arg
        List.map f lines

    let day01 () =
        getInput()
        |> parseInput
        |> List.fold (fun acc e -> acc |> e) 0

    let rec part2 (ops : (int -> int) list) (opIndex : int) (currentFrequency : int) (seenFrequencies : Set<int>) =
        if (opIndex >= ops.Length)
        then part2 ops 0 currentFrequency seenFrequencies
        else
            let currentFrequency' = currentFrequency |> ops.[opIndex]
            if (seenFrequencies.Contains currentFrequency')
            then currentFrequency'
            else
                let seenFrequencies' = seenFrequencies.Add currentFrequency'
                part2 ops (opIndex + 1) currentFrequency' seenFrequencies'


    let day01Part2 () =
        let ops = getInput() |> parseInput
        let frequencies = Set.empty<int>
        let firstRepeat = part2 ops 0 0 frequencies
        firstRepeat

