namespace Adventofcode2018

open System

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

    let rec executeReactions (p1 : char list) (p2 : char list) =
        match p2 with
        | c1 :: c2 :: rest -> let r1, r2 = processPair c1 c2
                              let p1', p2' = adjustAfterReaction p1 rest r1 r2
                              executeReactions p1' p2'
        | [c]              -> List.append p1 [c]
        | _ -> p1

    let day05 () =
        let input = getInput InputFile
        let p2 = input.ToCharArray() |> Array.toList
        let result = executeReactions [] p2
        result.Length