namespace Adventofcode2018

open System
open System.Linq

module Day05 =

    [<Literal>]
    let InputFile = "Day05Input.txt"

    let getInput fileName =
        System.IO.File.ReadAllText fileName

    let sameType u1 u2 = Char.ToLower u1 = Char.ToLower u2

    let oppositePolarity u1 u2 = Char.IsLower u1 <> Char.IsLower u2

    let doReact u1 u2 = sameType u1 u2 && oppositePolarity u1 u2

    let processPair c1 c2 =
        if doReact c1 c2
        then [], []
        else [c1], [c2]

    let adjustAfterReaction (p1 : char list) (p2 : char list) u1 u2 =
        match u1, u2 with
        | [], [] -> if (not p1.IsEmpty)
                    then
                        let p1' = List.take (p1.Length - 1) p1
                        let p2' = (List.last p1) :: p2
                        p1', p2'
                    else
                        p1, p2
        | a, b -> let p1' = List.append p1 a
                  let p2' = List.append b p2
                  p1', p2'


    let executeReactions (polymer : string) =
        let rec helper (p1 : char list) (p2 : char list) =
            match p2 with
            | c1 :: c2 :: rest -> let r1, r2 = processPair c1 c2
                                  let p1', p2' = adjustAfterReaction p1 rest r1 r2
                                  helper p1' p2'
            | [c]              -> List.append p1 [c]
            | _ -> p1

        let p2 = polymer.ToCharArray() |> Array.toList
        helper [] p2

    let day05 () =
        let result = getInput InputFile |> executeReactions
        result.Length

    let findShortest (polymer : string) =
        let rec helper unitsToRemove (dic : Map<char, int>) =
            match unitsToRemove with
            | u :: rest -> let polymer' = polymer.Replace(string(u), "", true, Globalization.CultureInfo.InvariantCulture)
                           let r = executeReactions polymer'
                           let dic' = dic.Add(u , r.Length)
                           helper rest dic'
            | []        -> dic
        
        let unitsToRemove = ['a' .. 'z']
        let d = helper unitsToRemove (Map([]))
        d.OrderBy(fun kv -> kv.Value).First().Value

    let day05Part2 () =
        getInput InputFile
        |> findShortest