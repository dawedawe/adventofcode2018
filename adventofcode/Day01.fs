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

