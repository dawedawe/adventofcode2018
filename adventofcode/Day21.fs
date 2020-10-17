namespace Adventofcode2018

module Day21 =

    open System

    type RegisterState = int64 []

    type ProgramState =
        { RegState: RegisterState
          InstPointerReg: int64
          InstPointer: int64 }

    type Input =
        | Register of int64
        | Value of int64

    type InstructionParams =
        { InputA: int64
          InputB: int64
          OutReg: int64 }

    type Instruction =
        { Opcode: InstructionParams -> ProgramState -> ProgramState
          Params: InstructionParams }

    type Program =
        { State: ProgramState
          Instructions: Instruction [] }

    let copyProgram (program: Program) =
        let progState = {
            RegState = Array.copy program.State.RegState
            InstPointerReg = program.State.InstPointerReg
            InstPointer = program.State.InstPointer
        }
        let instructions = Array.copy program.Instructions
        {
            State = progState
            Instructions = instructions
        }

    [<Literal>]
    let InputFile = "Day21Input.txt"

    let checkABForReg0 caller inst =
        if (inst.InputA = 0L || inst.InputB = 0L)
        then printfn "%s AB using reg 0 A: %d B: %d" caller inst.InputA inst.InputB

    let checkAForReg0 inst =
        if (inst.InputA = 0L)
        then printfn "A using reg 0"

    let checkBForReg0 inst =
        if (inst.InputB = 0L)
        then printfn "B using reg 0"

    let addr (inst: InstructionParams) (state: ProgramState) =
        checkABForReg0 "addr" inst
        let regsAfter = Array.copy state.RegState
        regsAfter.[int state.InstPointerReg] <- state.InstPointer
        regsAfter.[int inst.OutReg] <- regsAfter.[int inst.InputA] + regsAfter.[int inst.InputB]
        let instPointer = regsAfter.[int state.InstPointerReg] + 1L
        { state with
              RegState = regsAfter
              InstPointer = instPointer }

    let addi (inst: InstructionParams) (state: ProgramState) =
        checkAForReg0 inst
        let regsAfter = Array.copy state.RegState
        regsAfter.[int state.InstPointerReg] <- state.InstPointer
        regsAfter.[int inst.OutReg] <- regsAfter.[int inst.InputA] + inst.InputB
        let instPointer = regsAfter.[int state.InstPointerReg] + 1L
        { state with
              RegState = regsAfter
              InstPointer = instPointer }

    let mulr (inst: InstructionParams) (state: ProgramState) =
        checkABForReg0 "mulr" inst
        let regsAfter = Array.copy state.RegState
        regsAfter.[int state.InstPointerReg] <- state.InstPointer
        regsAfter.[int inst.OutReg] <- regsAfter.[int inst.InputA] * regsAfter.[int inst.InputB]
        let instPointer = regsAfter.[int state.InstPointerReg] + 1L
        { state with
              RegState = regsAfter
              InstPointer = instPointer }

    let muli (inst: InstructionParams) (state: ProgramState) =
        checkAForReg0 inst
        let regsAfter = Array.copy state.RegState
        regsAfter.[int state.InstPointerReg] <- state.InstPointer
        regsAfter.[int inst.OutReg] <- regsAfter.[int inst.InputA] * inst.InputB
        let instPointer = regsAfter.[int state.InstPointerReg] + 1L
        { state with
              RegState = regsAfter
              InstPointer = instPointer }

    let banr (inst: InstructionParams) (state: ProgramState) =
        checkABForReg0 "banr" inst
        let regsAfter = Array.copy state.RegState
        regsAfter.[int state.InstPointerReg] <- state.InstPointer
        regsAfter.[int inst.OutReg] <- regsAfter.[int inst.InputA] &&& regsAfter.[int inst.InputB]
        let instPointer = regsAfter.[int state.InstPointerReg] + 1L
        { state with
              RegState = regsAfter
              InstPointer = instPointer }

    let bani (inst: InstructionParams) (state: ProgramState) =
        checkAForReg0 inst
        let regsAfter = Array.copy state.RegState
        regsAfter.[int state.InstPointerReg] <- state.InstPointer
        regsAfter.[int inst.OutReg] <- regsAfter.[int inst.InputA] &&& inst.InputB
        let instPointer = regsAfter.[int state.InstPointerReg] + 1L
        { state with
              RegState = regsAfter
              InstPointer = instPointer }

    let borr (inst: InstructionParams) (state: ProgramState) =
        checkABForReg0 "borr" inst
        let regsAfter = Array.copy state.RegState
        regsAfter.[int state.InstPointerReg] <- state.InstPointer
        regsAfter.[int inst.OutReg] <- regsAfter.[int inst.InputA] ||| regsAfter.[int inst.InputB]
        let instPointer = regsAfter.[int state.InstPointerReg] + 1L
        { state with
              RegState = regsAfter
              InstPointer = instPointer }

    let bori (inst: InstructionParams) (state: ProgramState) =
        checkAForReg0 inst
        let regsAfter = Array.copy state.RegState
        regsAfter.[int state.InstPointerReg] <- state.InstPointer
        regsAfter.[int inst.OutReg] <- regsAfter.[int inst.InputA] ||| inst.InputB
        let instPointer = regsAfter.[int state.InstPointerReg] + 1L
        { state with
              RegState = regsAfter
              InstPointer = instPointer }

    let setr (inst: InstructionParams) (state: ProgramState) =
        checkAForReg0 inst
        let regsAfter = Array.copy state.RegState
        regsAfter.[int state.InstPointerReg] <- state.InstPointer
        regsAfter.[int inst.OutReg] <- regsAfter.[int inst.InputA]
        let instPointer = regsAfter.[int state.InstPointerReg] + 1L
        { state with
              RegState = regsAfter
              InstPointer = instPointer }

    let seti (inst: InstructionParams) (state: ProgramState) =
        let regsAfter = Array.copy state.RegState
        regsAfter.[int state.InstPointerReg] <- state.InstPointer
        regsAfter.[int inst.OutReg] <- inst.InputA
        let instPointer = regsAfter.[int state.InstPointerReg] + 1L
        { state with
              RegState = regsAfter
              InstPointer = instPointer }

    let gtir (inst: InstructionParams) (state: ProgramState) =
        checkBForReg0 inst
        let regsAfter = Array.copy state.RegState
        regsAfter.[int state.InstPointerReg] <- state.InstPointer
        regsAfter.[int inst.OutReg] <- if (inst.InputA > regsAfter.[int inst.InputB]) then 1L else 0L
        let instPointer = regsAfter.[int state.InstPointerReg] + 1L
        { state with
              RegState = regsAfter
              InstPointer = instPointer }

    let gtri (inst: InstructionParams) (state: ProgramState) =
        checkAForReg0 inst
        let regsAfter = Array.copy state.RegState
        regsAfter.[int state.InstPointerReg] <- state.InstPointer
        regsAfter.[int inst.OutReg] <- if (regsAfter.[int inst.InputA] > inst.InputB) then 1L else 0L
        let instPointer = regsAfter.[int state.InstPointerReg] + 1L
        { state with
              RegState = regsAfter
              InstPointer = instPointer }

    let gtrr (inst: InstructionParams) (state: ProgramState) =
        checkABForReg0 "gtrr" inst
        let regsAfter = Array.copy state.RegState
        regsAfter.[int state.InstPointerReg] <- state.InstPointer
        regsAfter.[int inst.OutReg] <- if (regsAfter.[int inst.InputA] > regsAfter.[int inst.InputB]) then 1L else 0L
        let instPointer = regsAfter.[int state.InstPointerReg] + 1L
        { state with
              RegState = regsAfter
              InstPointer = instPointer }

    let eqir (inst: InstructionParams) (state: ProgramState) =
        checkBForReg0 inst
        let regsAfter = Array.copy state.RegState
        regsAfter.[int state.InstPointerReg] <- state.InstPointer
        regsAfter.[int inst.OutReg] <- if (inst.InputA = regsAfter.[int inst.InputB]) then 1L else 0L
        let instPointer = regsAfter.[int state.InstPointerReg] + 1L
        { state with
              RegState = regsAfter
              InstPointer = instPointer }

    let eqri (inst: InstructionParams) (state: ProgramState) =
        checkAForReg0 inst
        let regsAfter = Array.copy state.RegState
        regsAfter.[int state.InstPointerReg] <- state.InstPointer
        regsAfter.[int inst.OutReg] <- if (regsAfter.[int inst.InputA] = inst.InputB) then 1L else 0L
        let instPointer = regsAfter.[int state.InstPointerReg] + 1L
        { state with
              RegState = regsAfter
              InstPointer = instPointer }

    // let eqrr (inst: InstructionParams) (state: ProgramState) =
    //     checkABForReg0 "eqrr" inst
    //     let regsAfter = Array.copy state.RegState
    //     regsAfter.[int state.InstPointerReg] <- state.InstPointer
    //     regsAfter.[int inst.OutReg] <- if (regsAfter.[int inst.InputA] = regsAfter.[int inst.InputB]) then 1L else 0L
    //     let instPointer = regsAfter.[int state.InstPointerReg] + 1L
    //     { state with
    //           RegState = regsAfter
    //           InstPointer = instPointer }

    let eqrr (inst: InstructionParams) (state: ProgramState) =
        checkABForReg0 "eqrr" inst
        let regsAfter = Array.copy state.RegState
        regsAfter.[int state.InstPointerReg] <- state.InstPointer
        if inst.InputB = 0L
        then
            printfn "faking 1"
            printfn "regsAfter.[int inst.InputA] = %d" regsAfter.[int inst.InputA]
            regsAfter.[int inst.OutReg] <- 1L
        else
            regsAfter.[int inst.OutReg] <- if (regsAfter.[int inst.InputA] = regsAfter.[int inst.InputB]) then 1L else 0L
        let instPointer = regsAfter.[int state.InstPointerReg] + 1L
        { state with
              RegState = regsAfter
              InstPointer = instPointer }

    let opcodesMap =
        [| ("addr", addr)
           ("addi", addi)
           ("mulr", mulr)
           ("muli", muli)
           ("banr", banr)
           ("bani", bani)
           ("borr", borr)
           ("bori", bori)
           ("setr", setr)
           ("seti", seti)
           ("gtir", gtir)
           ("gtri", gtri)
           ("gtrr", gtrr)
           ("eqir", eqir)
           ("eqri", eqri)
           ("eqrr", eqrr) |]
        |> Map.ofArray

    let parseInstruction (s: string) =
        let parts = s.Split [| ' ' |]
        let opcodeName = parts.[0]
        let opcode = opcodesMap.[opcodeName]

        let p =
            { InputA = Int64.Parse(parts.[1])
              InputB = Int64.Parse(parts.[2])
              OutReg = Int64.Parse(parts.[3]) }

        { Opcode = opcode
          Params = p }

    let parseInstPointerLine (s: string) =
        let parts = s.Split [| ' ' |]
        Int64.Parse(parts.[1])

    let getProgram path =
        let lines = System.IO.File.ReadAllLines path
        let ipReg = parseInstPointerLine lines.[0]

        let instructions = lines.[1..] |> Array.map parseInstruction

        let progState =
            { RegState = Array.create 6 0L
              InstPointerReg = ipReg
              InstPointer = 0L }

        { State = progState
          Instructions = instructions }

    let rec runProgram maxIteratons program =
        let rec helper i program =
            //printfn "%d" program.State.InstPointer
            if (i > maxIteratons) then
                printfn "max iters reached"
                None
            else if (program.State.InstPointer < 0L || program.State.InstPointer >= int64 program.Instructions.Length) then
                printfn "InstPointer out of range"
                Some i
            else
                let op = program.Instructions.[int program.State.InstPointer]
                let programState' =
                    op.Opcode program.Instructions.[int program.State.InstPointer].Params program.State
                let program' = { program with State = programState' }
                helper (i + 1) program'
        helper 0 program

    // What is the lowest non-negative integer value for register 0 that causes the program to halt
    // after executing the fewest instructions? (Executing the same instruction multiple times counts
    // as multiple instructions executed.)
    let day21() =
        let program = getProgram InputFile
        let mutable minInstructions = Int32.MaxValue
        for i in [0L .. 0L] do
            let p = copyProgram program
            p.State.RegState.[0] <- i
            let instructions = runProgram 500000 p
            match instructions with
            | Some j when j < minInstructions -> minInstructions <- j
            | _ -> ()
            
        minInstructions

