namespace Adventofcode2018

module Day02 =

    open System.Collections.Generic
    open System.Linq

    [<Literal>]
    let InputFile = "Day02Input.txt"

    let getInput () =
        System.IO.File.ReadAllLines InputFile
        |> List.ofArray

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

    let distance (s1 : string) (s2 : string) =
        Array.zip (s1.ToCharArray()) (s2.ToCharArray())
        |> Array.fold (fun acc (c1, c2) -> if c1 = c2 then acc else acc + 1) 0
    
    let rec getTwoIds lines =
        match lines with
        | l :: rest -> let candidates = List.filter (fun l2 -> distance l l2 = 1) rest
                       if (candidates.IsEmpty)
                       then getTwoIds rest
                       else (l, candidates.Head)
        | _         -> failwith "no two ids found"

    let getCommonChars ((s1, s2) : string * string) =
        Array.zip (s1.ToCharArray()) (s2.ToCharArray())
        |> Array.fold (fun acc (c1, c2) -> if c1 = c2 then acc + string(c1) else acc + "") ""

    let day02Part2 () =
        getInput()
        |> getTwoIds
        |> getCommonChars