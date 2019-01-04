namespace Adventofcode2018

open System.Linq
open System.Collections.Generic

module Day07 =

    [<Literal>]
    let InputFile = "Day07Input.txt"

    type Dependency =
        {
            Name : char
            Prereq : char
        }

    type Step =
        {
            Name : char
            Prereqs : char list
        }

    let parseLine (s : string) =
        {
            Prereq = s.[5]
            Name = s.[36]
        }

    let getSteps deps =
        let allSteps = Array.fold (fun s (d : Dependency) -> Set.union s (Set.ofList [d.Name; d.Prereq])) Set.empty deps
        let lookup = deps.ToLookup(fun d -> d.Name)
        let stepsWithDeps = lookup.Select(fun g -> { Name =  g.Key;
                                                     Prereqs = List.ofSeq(lookup.[g.Key].Select(fun v -> v.Prereq)) })
                                  .ToList()
        let startSteps = Set.difference allSteps (Set.ofSeq(stepsWithDeps.Select(fun s -> s.Name)))
        let steps = Set.map (fun s -> { Name = s; Prereqs = List.empty }) startSteps 
                    |> Set.union (Set.ofSeq stepsWithDeps)
        steps

    let prereqsDone (stepsDone : Set<Step>) (candidate : Step) =
        let prereqs = candidate.Prereqs |> Set.ofList
        let d = Set.map (fun s -> s.Name) stepsDone
        Set.isSubset prereqs d

    let getNextReady (stepsToDo : Set<Step>) (stepsDone : Set<Step>) =
        Set.filter (prereqsDone stepsDone) stepsToDo
        |> Set.toList
        |> List.min

    let rec findSequence sequence stepsToDo stepsDone =
        if (not (Set.isEmpty stepsToDo))
        then
            let n = getNextReady stepsToDo stepsDone
            let sequence' = List.append sequence [n] 
            let stepsDone' = stepsDone.Add n
            let stepsToDo' = stepsToDo.Remove n
            findSequence sequence' stepsToDo' stepsDone'
        else
            sequence

    let day07 () =
        let steps = System.IO.File.ReadAllLines InputFile
                    |> Array.map parseLine
                    |> getSteps
        findSequence [] steps Set.empty
        |> List.map (fun s -> s.Name)
        |> List.fold (fun s c -> s + string(c)) ""