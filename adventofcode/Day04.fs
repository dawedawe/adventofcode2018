namespace Adventofcode2018

open System
open System.Collections.Generic
open System.Linq

module Day04 =

    [<Literal>]
    let InputFile = "Day04Input.txt"

    type Guard =
        {
            Id : int
            Slept : Dictionary<int, int>
        }

    let getInput fileName =
        System.IO.File.ReadAllLines fileName
        |> List.ofArray

    let parseTimestamp (s : string) =
        let pattern = @"\[(\d\d\d\d)-(\d\d)-(\d\d) (\d\d):(\d\d)\]"
        let regex = new System.Text.RegularExpressions.Regex(pattern)
        let m = regex.Match(s)
        let year = int(m.Groups.[1].Value)
        let month = int(m.Groups.[2].Value)
        let day = int(m.Groups.[3].Value)
        let hour = int(m.Groups.[4].Value)
        let minute = int(m.Groups.[5].Value)
        DateTime(year, month, day, hour, minute, 0)

    let parseGuardId s =
        let pattern = @"Guard #(\d+) begins shift"
        let regex = new System.Text.RegularExpressions.Regex(pattern)
        let m = regex.Match(s)
        int(m.Groups.[1].Value)

    let update (guardsDic : Dictionary<int, Guard>) guardId l1 l2 =
        let asleep = parseTimestamp l1
        let wake = (parseTimestamp l2)
        let range = [asleep.Minute .. wake.Minute - 1]
        if (not (guardsDic.ContainsKey(guardId)))
        then guardsDic.Add(guardId, { Id = guardId; Slept = Dictionary<int, int>()} )
        let sleptDic = guardsDic.[guardId].Slept
        for min in range do
            if (sleptDic.ContainsKey(min))
            then sleptDic.[min] <- sleptDic.[min] + 1
            else sleptDic.[min] <- 1

    let rec processLines (lines : string list) guardsDic currentGuardId =
        match lines with
        | l :: rest when l.Contains("Guard #")              -> let currentGuard' = parseGuardId l
                                                               processLines rest guardsDic currentGuard'
        | l1 :: l2 :: rest when l1.Contains("falls asleep") -> update guardsDic currentGuardId l1 l2
                                                               processLines rest guardsDic currentGuardId
        | _ -> guardsDic

    let day04 () =
        let lines = getInput InputFile |> List.sortBy parseTimestamp 
        let guardsDic = processLines lines (Dictionary<int, Guard>()) 0
        let biggestSleeper = guardsDic.Values.OrderByDescending(fun g -> g.Slept.Values.Sum()).First()
        let minute = biggestSleeper.Slept.OrderByDescending(fun p -> p.Value).First().Key
        biggestSleeper.Id * minute

    let day04Part2 () =
        let lines = getInput InputFile |> List.sortBy parseTimestamp 
        let guardsDic = processLines lines (Dictionary<int, Guard>()) 0
        let biggestSleeper = guardsDic.Values.OrderByDescending(fun g -> g.Slept.Values.Max()).First()
        let minute = biggestSleeper.Slept.OrderByDescending(fun p -> p.Value).First().Key
        biggestSleeper.Id * minute