namespace Adventofcode2018

module Day02 =

    open System.Collections.Generic
    open System.Linq

    [<Literal>]
    let InputFile = "Day02Input.txt"

    let getInput () =
        System.IO.File.ReadAllLines InputFile
        |> List.ofArray

    //prtkqyluibmtcwqaezjmhgfndx
    let countOccurences (s : string) =
        let dic = Dictionary<char, int>()
        for c in s do
            if (dic.ContainsKey(c))
            then dic.[c] <- dic.[c] + 1
            else dic.Add(c, 1)
        let hasDouble = dic.Values.Any(fun v -> v = 2)
        let hasTripple = dic.Values.Any(fun v -> v = 3)
        (hasDouble, hasTripple)

    let boolToInt =
        function
        | true -> 1
        | false -> 0

    let checksum lines =
        List.map countOccurences lines
        |> List.fold (fun (dAcc, tAcc) (d, t) -> (dAcc + boolToInt d, tAcc + boolToInt t)) (0, 0)
        |> fun (d, t) -> d * t
    
    let day02 () =
        getInput()
        |> checksum