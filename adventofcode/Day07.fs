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
            TimeRequired : int
        }

    type WorkerState =
        {
            Id : int
            WorksOn : char
        }

    let parseLine (s : string) =
        {
            Prereq = s.[5]
            Name = s.[36]
        }

    let charToDuration (c : char) =
        int(c) - 4

    let getSteps deps =
        let allSteps = Array.fold (fun s (d : Dependency) -> Set.union s (Set.ofList [d.Name; d.Prereq])) Set.empty deps
        let lookup = deps.ToLookup(fun d -> d.Name)
        let stepsWithDeps = lookup.Select(fun g -> { Name =  g.Key
                                                     Prereqs = List.ofSeq(lookup.[g.Key].Select(fun v -> v.Prereq))
                                                     TimeRequired = charToDuration g.Key
                                                    })
                                  .ToList()
        let startSteps = Set.difference allSteps (Set.ofSeq(stepsWithDeps.Select(fun s -> s.Name)))
        let steps = Set.map (fun s -> { Name = s; Prereqs = List.empty; TimeRequired = charToDuration s }) startSteps 
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

    let getNextReady2 (stepsToDo : Set<Step>) (stepsDone : Set<Step>) =
        Set.filter (prereqsDone stepsDone) stepsToDo
        |> Set.toList
        |> List.sortBy (fun s -> s.Name)

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

    let freeWorker (workerToFree : WorkerState) (workerStates : WorkerState list) =
        let otherWorkers = List.filter (fun w -> w.Id <> workerToFree.Id) workerStates
        let freedWorker = { workerToFree with WorksOn = '.' }
        otherWorkers @ [freedWorker]

    let workOnStep stepToWorkOn =
        { stepToWorkOn with TimeRequired = stepToWorkOn.TimeRequired - 1 }

    let doWork (unfinishedSteps: Step list) (workerStates : WorkerState list) =
        let stepsBeingWorkedOn = List.map (fun w -> w.WorksOn) workerStates
        let mutable unfinishedSteps' = List.filter (fun s -> not (List.contains s.Name stepsBeingWorkedOn)) unfinishedSteps
        let mutable workerStates' = workerStates
        let mutable finishedSteps = []
        for worker in workerStates do
            if worker.WorksOn <> '.'
            then
                let stepToWorkOn = List.find (fun s -> s.Name = worker.WorksOn) unfinishedSteps
                let stepWorkedOn = workOnStep stepToWorkOn
                if (stepWorkedOn.TimeRequired = 0)
                then
                    workerStates' <- freeWorker worker workerStates'
                    finishedSteps <- finishedSteps @ [stepWorkedOn]
                else
                    unfinishedSteps' <- unfinishedSteps' @ [stepWorkedOn]
        (workerStates', finishedSteps, unfinishedSteps')

    let scheduleFreeWorkers unfinishedSteps stepsDone (workerStates : WorkerState list) =
        let mutable freeWorkers = List.filter (fun w -> w.WorksOn = '.') workerStates
        let mutable workerStates' = workerStates
        let nextReady = getNextReady2 unfinishedSteps (Set.ofList stepsDone)
        for n in nextReady do
            if not (List.isEmpty freeWorkers)
            then
                let workerAlreadyWorkingOnStep = List.exists (fun w -> w.WorksOn = n.Name) workerStates'
                if not workerAlreadyWorkingOnStep
                then
                    let workerToSchedule = { freeWorkers.[0] with WorksOn = n.Name }
                    freeWorkers <- List.filter (fun w -> w.Id <> workerToSchedule.Id) freeWorkers
                    workerStates' <- List.filter (fun w -> w.Id <> workerToSchedule.Id) workerStates'
                    workerStates' <- workerStates' @ [workerToSchedule]
        workerStates'

    let rec findSequence2 (sequenceOfFinishedSteps : Step list) unfinishedSteps workerStates secondsPassed =
        if List.length sequenceOfFinishedSteps = 26
        then
            (secondsPassed, sequenceOfFinishedSteps)
        else
            let workerStates' = scheduleFreeWorkers unfinishedSteps sequenceOfFinishedSteps workerStates
            let (workerStates'', finishedSteps, unfinishedSteps') = doWork (Set.toList unfinishedSteps) workerStates'
            let sequenceOfSteps' = List.append sequenceOfFinishedSteps finishedSteps
            let secondsPassed' = secondsPassed + 1
            findSequence2 sequenceOfSteps' (Set.ofList unfinishedSteps') workerStates'' secondsPassed'

    let day07Part2 () =
        let steps = System.IO.File.ReadAllLines InputFile
                    |> Array.map parseLine
                    |> getSteps
        let initialWorkerStates = List.init 5 (fun i -> { Id = i + 1; WorksOn = '.' })
        let (seconds, _) = findSequence2 [] steps initialWorkerStates 0
        seconds
        