namespace Adventofcode2018

module Day19 =

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

    [<Literal>]
    let InputFile = "Day19Input.txt"

    let addr (inst: InstructionParams) (state: ProgramState) =
        let regsAfter = Array.copy state.RegState
        regsAfter.[int state.InstPointerReg] <- state.InstPointer
        regsAfter.[int inst.OutReg] <- regsAfter.[int inst.InputA] + regsAfter.[int inst.InputB]
        let instPointer = regsAfter.[int state.InstPointerReg] + 1L
        { state with
              RegState = regsAfter
              InstPointer = instPointer }

    let addi (inst: InstructionParams) (state: ProgramState) =
        let regsAfter = Array.copy state.RegState
        regsAfter.[int state.InstPointerReg] <- state.InstPointer
        regsAfter.[int inst.OutReg] <- regsAfter.[int inst.InputA] + inst.InputB
        let instPointer = regsAfter.[int state.InstPointerReg] + 1L
        { state with
              RegState = regsAfter
              InstPointer = instPointer }

    let mulr (inst: InstructionParams) (state: ProgramState) =
        let regsAfter = Array.copy state.RegState
        regsAfter.[int state.InstPointerReg] <- state.InstPointer
        regsAfter.[int inst.OutReg] <- regsAfter.[int inst.InputA] * regsAfter.[int inst.InputB]
        let instPointer = regsAfter.[int state.InstPointerReg] + 1L
        { state with
              RegState = regsAfter
              InstPointer = instPointer }

    let muli (inst: InstructionParams) (state: ProgramState) =
        let regsAfter = Array.copy state.RegState
        regsAfter.[int state.InstPointerReg] <- state.InstPointer
        regsAfter.[int inst.OutReg] <- regsAfter.[int inst.InputA] * inst.InputB
        let instPointer = regsAfter.[int state.InstPointerReg] + 1L
        { state with
              RegState = regsAfter
              InstPointer = instPointer }

    let banr (inst: InstructionParams) (state: ProgramState) =
        let regsAfter = Array.copy state.RegState
        regsAfter.[int state.InstPointerReg] <- state.InstPointer
        regsAfter.[int inst.OutReg] <- regsAfter.[int inst.InputA] &&& regsAfter.[int inst.InputB]
        let instPointer = regsAfter.[int state.InstPointerReg] + 1L
        { state with
              RegState = regsAfter
              InstPointer = instPointer }

    let bani (inst: InstructionParams) (state: ProgramState) =
        let regsAfter = Array.copy state.RegState
        regsAfter.[int state.InstPointerReg] <- state.InstPointer
        regsAfter.[int inst.OutReg] <- regsAfter.[int inst.InputA] &&& inst.InputB
        let instPointer = regsAfter.[int state.InstPointerReg] + 1L
        { state with
              RegState = regsAfter
              InstPointer = instPointer }

    let borr (inst: InstructionParams) (state: ProgramState) =
        let regsAfter = Array.copy state.RegState
        regsAfter.[int state.InstPointerReg] <- state.InstPointer
        regsAfter.[int inst.OutReg] <- regsAfter.[int inst.InputA] ||| regsAfter.[int inst.InputB]
        let instPointer = regsAfter.[int state.InstPointerReg] + 1L
        { state with
              RegState = regsAfter
              InstPointer = instPointer }

    let bori (inst: InstructionParams) (state: ProgramState) =
        let regsAfter = Array.copy state.RegState
        regsAfter.[int state.InstPointerReg] <- state.InstPointer
        regsAfter.[int inst.OutReg] <- regsAfter.[int inst.InputA] ||| inst.InputB
        let instPointer = regsAfter.[int state.InstPointerReg] + 1L
        { state with
              RegState = regsAfter
              InstPointer = instPointer }

    let setr (inst: InstructionParams) (state: ProgramState) =
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
        let regsAfter = Array.copy state.RegState
        regsAfter.[int state.InstPointerReg] <- state.InstPointer
        regsAfter.[int inst.OutReg] <- if (inst.InputA > regsAfter.[int inst.InputB])
                                       then 1L
                                       else 0L
        let instPointer = regsAfter.[int state.InstPointerReg] + 1L
        { state with
              RegState = regsAfter
              InstPointer = instPointer }

    let gtri (inst: InstructionParams) (state: ProgramState) =
        let regsAfter = Array.copy state.RegState
        regsAfter.[int state.InstPointerReg] <- state.InstPointer
        regsAfter.[int inst.OutReg] <- if (regsAfter.[int inst.InputA] > inst.InputB)
                                       then 1L
                                       else 0L
        let instPointer = regsAfter.[int state.InstPointerReg] + 1L
        { state with
              RegState = regsAfter
              InstPointer = instPointer }

    let gtrr (inst: InstructionParams) (state: ProgramState) =
        let regsAfter = Array.copy state.RegState
        regsAfter.[int state.InstPointerReg] <- state.InstPointer
        regsAfter.[int inst.OutReg] <- if (regsAfter.[int inst.InputA] > regsAfter.[int inst.InputB])
                                       then 1L
                                       else 0L
        let instPointer = regsAfter.[int state.InstPointerReg] + 1L
        { state with
              RegState = regsAfter
              InstPointer = instPointer }

    let eqir (inst: InstructionParams) (state: ProgramState) =
        let regsAfter = Array.copy state.RegState
        regsAfter.[int state.InstPointerReg] <- state.InstPointer
        regsAfter.[int inst.OutReg] <- if (inst.InputA = regsAfter.[int inst.InputB])
                                       then 1L
                                       else 0L
        let instPointer = regsAfter.[int state.InstPointerReg] + 1L
        { state with
              RegState = regsAfter
              InstPointer = instPointer }

    let eqri (inst: InstructionParams) (state: ProgramState) =
        let regsAfter = Array.copy state.RegState
        regsAfter.[int state.InstPointerReg] <- state.InstPointer
        regsAfter.[int inst.OutReg] <- if (regsAfter.[int inst.InputA] = inst.InputB)
                                       then 1L
                                       else 0L
        let instPointer = regsAfter.[int state.InstPointerReg] + 1L
        { state with
              RegState = regsAfter
              InstPointer = instPointer }

    let eqrr (inst: InstructionParams) (state: ProgramState) =
        let regsAfter = Array.copy state.RegState
        regsAfter.[int state.InstPointerReg] <- state.InstPointer
        regsAfter.[int inst.OutReg] <- if (regsAfter.[int inst.InputA] = regsAfter.[int inst.InputB])
                                       then 1L
                                       else 0L
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

        { Opcode = opcode; Params = p }

    let parseInstPointerLine (s: string) =
        let parts = s.Split [| ' ' |]
        Int64.Parse(parts.[1])

    let getProgram path =
        let lines = System.IO.File.ReadAllLines path
        let ipReg = parseInstPointerLine lines.[0]

        let instructions =
            lines.[1..] |> Array.map parseInstruction

        let progState =
            { RegState = Array.create 6 0L
              InstPointerReg = ipReg
              InstPointer = 0L }

        { State = progState
          Instructions = instructions }

    let rec runProgram program =
        if (program.State.InstPointer < 0L || program.State.InstPointer >= int64 program.Instructions.Length)
        then program
        else
            let op = program.Instructions.[int program.State.InstPointer]
            let programState' = op.Opcode program.Instructions.[int program.State.InstPointer].Params program.State
            let program' = { program with State = programState' }
            runProgram program'

    let day19 () =
        let program = getProgram InputFile
                      |> runProgram
        program.State.RegState.[0]
    
    let shortcut (program : Program) =
        program.State.RegState.[5] <- 1L
        program.State.RegState.[3] <- 1L
        while int program.State.RegState.[2] < program.Instructions.Length do

            program.State.RegState.[3] <- program.State.RegState.[1] / program.State.RegState.[5]
            program.State.RegState.[4] <- program.State.RegState.[5] * program.State.RegState.[3]
            
            if program.State.RegState.[4] = program.State.RegState.[1]
            then program.State.RegState.[0] <- program.State.RegState.[5] + program.State.RegState.[0]

            program.State.RegState.[3] <- program.State.RegState.[1] + 1L

            if program.State.RegState.[3] > program.State.RegState.[1]
            then
                program.State.RegState.[5] <- program.State.RegState.[5] + 1L
                if program.State.RegState.[5] > program.State.RegState.[1]
                then program.State.RegState.[2] <- int64 program.Instructions.Length
                else program.State.RegState.[3] <- 1L

        { program with State = { program.State with InstPointer = program.State.RegState.[int program.State.InstPointerReg]} }

    let rec runProgramPart2 program =
        if (program.State.InstPointer < 0L || program.State.InstPointer >= int64 program.Instructions.Length)
        then program
        else
            if (program.State.InstPointer = 1L)
            then shortcut program
            else
                let op = program.Instructions.[int program.State.InstPointer]
                let programState' = op.Opcode program.Instructions.[int program.State.InstPointer].Params program.State
                let program' = { program with State = programState' }
                runProgramPart2 program'

    let day19Part2 () =
        let program = getProgram InputFile
        program.State.RegState.[0] <- 1L
        let program' = runProgramPart2 program
        program'.State.RegState.[0]
        
