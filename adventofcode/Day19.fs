namespace Adventofcode2018

module Day19 =

    open System

    type RegisterState = int []

    type ProgramState =
        { RegState: RegisterState
          InstPointerReg: int
          InstPointer: int }

    type Input =
        | Register of int
        | Value of int

    type InstructionParams =
        { InputA: int
          InputB: int
          OutReg: int }

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
        regsAfter.[state.InstPointerReg] <- state.InstPointer
        regsAfter.[inst.OutReg] <- regsAfter.[inst.InputA] + regsAfter.[inst.InputB]
        let instPointer = regsAfter.[state.InstPointerReg] + 1
        { state with
              RegState = regsAfter
              InstPointer = instPointer }

    let addi (inst: InstructionParams) (state: ProgramState) =
        let regsAfter = Array.copy state.RegState
        regsAfter.[state.InstPointerReg] <- state.InstPointer
        regsAfter.[inst.OutReg] <- regsAfter.[inst.InputA] + inst.InputB
        let instPointer = regsAfter.[state.InstPointerReg] + 1
        { state with
              RegState = regsAfter
              InstPointer = instPointer }

    let mulr (inst: InstructionParams) (state: ProgramState) =
        let regsAfter = Array.copy state.RegState
        regsAfter.[state.InstPointerReg] <- state.InstPointer
        regsAfter.[inst.OutReg] <- regsAfter.[inst.InputA] * regsAfter.[inst.InputB]
        let instPointer = regsAfter.[state.InstPointerReg] + 1
        { state with
              RegState = regsAfter
              InstPointer = instPointer }

    let muli (inst: InstructionParams) (state: ProgramState) =
        let regsAfter = Array.copy state.RegState
        regsAfter.[state.InstPointerReg] <- state.InstPointer
        regsAfter.[inst.OutReg] <- regsAfter.[inst.InputA] * inst.InputB
        let instPointer = regsAfter.[state.InstPointerReg] + 1
        { state with
              RegState = regsAfter
              InstPointer = instPointer }

    let banr (inst: InstructionParams) (state: ProgramState) =
        let regsAfter = Array.copy state.RegState
        regsAfter.[state.InstPointerReg] <- state.InstPointer
        regsAfter.[inst.OutReg] <- regsAfter.[inst.InputA] &&& regsAfter.[inst.InputB]
        let instPointer = regsAfter.[state.InstPointerReg] + 1
        { state with
              RegState = regsAfter
              InstPointer = instPointer }

    let bani (inst: InstructionParams) (state: ProgramState) =
        let regsAfter = Array.copy state.RegState
        regsAfter.[state.InstPointerReg] <- state.InstPointer
        regsAfter.[inst.OutReg] <- regsAfter.[inst.InputA] &&& inst.InputB
        let instPointer = regsAfter.[state.InstPointerReg] + 1
        { state with
              RegState = regsAfter
              InstPointer = instPointer }

    let borr (inst: InstructionParams) (state: ProgramState) =
        let regsAfter = Array.copy state.RegState
        regsAfter.[state.InstPointerReg] <- state.InstPointer
        regsAfter.[inst.OutReg] <- regsAfter.[inst.InputA] ||| regsAfter.[inst.InputB]
        let instPointer = regsAfter.[state.InstPointerReg] + 1
        { state with
              RegState = regsAfter
              InstPointer = instPointer }

    let bori (inst: InstructionParams) (state: ProgramState) =
        let regsAfter = Array.copy state.RegState
        regsAfter.[state.InstPointerReg] <- state.InstPointer
        regsAfter.[inst.OutReg] <- regsAfter.[inst.InputA] ||| inst.InputB
        let instPointer = regsAfter.[state.InstPointerReg] + 1
        { state with
              RegState = regsAfter
              InstPointer = instPointer }

    let setr (inst: InstructionParams) (state: ProgramState) =
        let regsAfter = Array.copy state.RegState
        regsAfter.[state.InstPointerReg] <- state.InstPointer
        regsAfter.[inst.OutReg] <- regsAfter.[inst.InputA]
        let instPointer = regsAfter.[state.InstPointerReg] + 1
        { state with
              RegState = regsAfter
              InstPointer = instPointer }

    let seti (inst: InstructionParams) (state: ProgramState) =
        let regsAfter = Array.copy state.RegState
        regsAfter.[state.InstPointerReg] <- state.InstPointer
        regsAfter.[inst.OutReg] <- inst.InputA
        let instPointer = regsAfter.[state.InstPointerReg] + 1
        { state with
              RegState = regsAfter
              InstPointer = instPointer }

    let gtir (inst: InstructionParams) (state: ProgramState) =
        let regsAfter = Array.copy state.RegState
        regsAfter.[state.InstPointerReg] <- state.InstPointer
        regsAfter.[inst.OutReg] <- if (inst.InputA > regsAfter.[inst.InputB])
                                   then 1
                                   else 0
        let instPointer = regsAfter.[state.InstPointerReg] + 1
        { state with
              RegState = regsAfter
              InstPointer = instPointer }

    let gtri (inst: InstructionParams) (state: ProgramState) =
        let regsAfter = Array.copy state.RegState
        regsAfter.[state.InstPointerReg] <- state.InstPointer
        regsAfter.[inst.OutReg] <- if (regsAfter.[inst.InputA] > inst.InputB)
                                   then 1
                                   else 0
        let instPointer = regsAfter.[state.InstPointerReg] + 1
        { state with
              RegState = regsAfter
              InstPointer = instPointer }

    let gtrr (inst: InstructionParams) (state: ProgramState) =
        let regsAfter = Array.copy state.RegState
        regsAfter.[state.InstPointerReg] <- state.InstPointer
        regsAfter.[inst.OutReg] <- if (regsAfter.[inst.InputA] > regsAfter.[inst.InputB])
                                   then 1
                                   else 0
        let instPointer = regsAfter.[state.InstPointerReg] + 1
        { state with
              RegState = regsAfter
              InstPointer = instPointer }

    let eqir (inst: InstructionParams) (state: ProgramState) =
        let regsAfter = Array.copy state.RegState
        regsAfter.[state.InstPointerReg] <- state.InstPointer
        regsAfter.[inst.OutReg] <- if (inst.InputA = regsAfter.[inst.InputB])
                                   then 1
                                   else 0
        let instPointer = regsAfter.[state.InstPointerReg] + 1
        { state with
              RegState = regsAfter
              InstPointer = instPointer }

    let eqri (inst: InstructionParams) (state: ProgramState) =
        let regsAfter = Array.copy state.RegState
        regsAfter.[state.InstPointerReg] <- state.InstPointer
        regsAfter.[inst.OutReg] <- if (regsAfter.[inst.InputA] = inst.InputB)
                                   then 1
                                   else 0
        let instPointer = regsAfter.[state.InstPointerReg] + 1
        { state with
              RegState = regsAfter
              InstPointer = instPointer }

    let eqrr (inst: InstructionParams) (state: ProgramState) =
        let regsAfter = Array.copy state.RegState
        regsAfter.[state.InstPointerReg] <- state.InstPointer
        regsAfter.[inst.OutReg] <- if (regsAfter.[inst.InputA] = regsAfter.[inst.InputB])
                                   then 1
                                   else 0
        let instPointer = regsAfter.[state.InstPointerReg] + 1
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
            { InputA = Int32.Parse(parts.[1])
              InputB = Int32.Parse(parts.[2])
              OutReg = Int32.Parse(parts.[3]) }

        { Opcode = opcode; Params = p }

    let parseInstPointerLine (s: string) =
        let parts = s.Split [| ' ' |]
        Int32.Parse(parts.[1])

    let getProgram path =
        let lines = System.IO.File.ReadAllLines path
        let ipReg = parseInstPointerLine lines.[0]

        let instructions =
            lines.[1..] |> Array.map parseInstruction

        let progState =
            { RegState = Array.create 6 0
              InstPointerReg = ipReg
              InstPointer = 0 }

        { State = progState
          Instructions = instructions }

    let rec runProgram program =
        if (program.State.InstPointer < 0 || program.State.InstPointer >= program.Instructions.Length)
        then program
        else
            let op = program.Instructions.[program.State.InstPointer]
            let programState' = op.Opcode program.Instructions.[program.State.InstPointer].Params program.State
            let program' = { program with State = programState' }
            runProgram program'

    let day19 () =
        let program = getProgram InputFile
                      |> runProgram
        program.State.RegState.[0]
