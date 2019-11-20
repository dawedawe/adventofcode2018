namespace Adventofcode2018

module Day16 =

    open System

    type RegisterState = int []

    type Input =
    | Register of int
    | Value of int

    type Instruction = {
        Number : int
        InputA : int
        InputB : int
        OutReg : int
    }

    type Sample = {
        Before : RegisterState
        Inst : Instruction
        After : RegisterState
    }

    [<Literal>]
    let InputFile = "Day16Input.txt"

    let parseRegisterState (s : string) =
        [|
            Int32.Parse(string s.[9]);
            Int32.Parse(string s.[12]);
            Int32.Parse(string s.[15]);
            Int32.Parse(string s.[18]);
        |]
        
    let parseInstruction (s : string) =
        let parts = s.Split [| ' ' |]
        {
            Number = Int32.Parse(parts.[0])
            InputA = Int32.Parse(parts.[1])
            InputB = Int32.Parse(parts.[2])
            OutReg = Int32.Parse(parts.[3])
        }

    let getSamples path =
        let mutable samples = List.empty
        let input = System.IO.File.ReadAllLines path
        for i in 0 .. 4 .. (input.Length - 1) do
            if input.[i].StartsWith "Before"
            then
                let beforeLine = input.[i]
                let instructionLine = input.[i + 1]
                let afterLine = input.[i + 2]
                let before = parseRegisterState beforeLine
                let instruction = parseInstruction instructionLine
                let after = parseRegisterState afterLine
                let sample = { Before = before; Inst = instruction; After = after }
                samples <- samples @ [sample]
        samples

    let addr (inst : Instruction) (regs : RegisterState) =
        let regsAfter = Array.copy regs
        regsAfter.[inst.OutReg] <- regs.[inst.InputA] + regs.[inst.InputB]
        regsAfter

    let addi (inst : Instruction) (regs : RegisterState) =
        let regAfter = Array.copy regs
        regAfter.[inst.OutReg] <- regs.[inst.InputA] + inst.InputB
        regAfter

    let mulr (inst : Instruction) (regs : RegisterState) =
        let regAfter = Array.copy regs
        regAfter.[inst.OutReg] <- regs.[inst.InputA] * regs.[inst.InputB]
        regAfter

    let muli (inst : Instruction) (regs : RegisterState) =
        let regAfter = Array.copy regs
        regAfter.[inst.OutReg] <- regs.[inst.InputA] * inst.InputB
        regAfter

    let banr (inst : Instruction) (regs : RegisterState) =
        let regAfter = Array.copy regs
        regAfter.[inst.OutReg] <- regs.[inst.InputA] &&& regs.[inst.InputB]
        regAfter

    let bani (inst : Instruction) (regs : RegisterState) =
        let regAfter = Array.copy regs
        regAfter.[inst.OutReg] <- regs.[inst.InputA] &&& inst.InputB
        regAfter

    let borr (inst : Instruction) (regs : RegisterState) =
        let regAfter = Array.copy regs
        regAfter.[inst.OutReg] <- regs.[inst.InputA] ||| regs.[inst.InputB]
        regAfter

    let bori (inst : Instruction) (regs : RegisterState) =
        let regAfter = Array.copy regs
        regAfter.[inst.OutReg] <- regs.[inst.InputA] ||| inst.InputB
        regAfter

    let setr (inst : Instruction) (regs : RegisterState) =
        let regAfter = Array.copy regs
        regAfter.[inst.OutReg] <- regs.[inst.InputA]
        regAfter

    let seti (inst : Instruction) (regs : RegisterState) =
        let regAfter = Array.copy regs
        regAfter.[inst.OutReg] <- inst.InputA
        regAfter

    let gtir (inst : Instruction) (regs : RegisterState) =
        let regAfter = Array.copy regs
        regAfter.[inst.OutReg] <- if (inst.InputA > regs.[inst.InputB]) then 1 else 0
        regAfter

    let gtri (inst : Instruction) (regs : RegisterState) =
        let regAfter = Array.copy regs
        regAfter.[inst.OutReg] <- if (regs.[inst.InputA] > inst.InputB) then 1 else 0
        regAfter

    let gtrr (inst : Instruction) (regs : RegisterState) =
        let regAfter = Array.copy regs
        regAfter.[inst.OutReg] <- if (regs.[inst.InputA] > regs.[inst.InputB]) then 1 else 0
        regAfter

    let eqir (inst : Instruction) (regs : RegisterState) =
        let regAfter = Array.copy regs
        regAfter.[inst.OutReg] <- if (inst.InputA = regs.[inst.InputB]) then 1 else 0
        regAfter

    let eqri (inst : Instruction) (regs : RegisterState) =
        let regAfter = Array.copy regs
        regAfter.[inst.OutReg] <- if (regs.[inst.InputA] = inst.InputB) then 1 else 0
        regAfter

    let eqrr (inst : Instruction) (regs : RegisterState) =
        let regAfter = Array.copy regs
        regAfter.[inst.OutReg] <- if (regs.[inst.InputA] = regs.[inst.InputB]) then 1 else 0
        regAfter

    let opcodes =
        [|
            addr; addi; mulr; muli; banr; bani; borr; bori;
            setr; seti; gtir; gtri; gtrr; eqir; eqri; eqrr
        |]

    let testOpcode opcode (sample : Sample) =
        let result = opcode sample.Inst sample.Before
        result = sample.After

    let behaveLike3OrMore sample =
        Array.map (fun o -> testOpcode o sample) opcodes
        |> Array.sumBy (fun t -> if t then 1 else 0) >= 3

    let day16 () =
        let samples = getSamples InputFile
        List.map behaveLike3OrMore samples
        |> List.sumBy (fun t -> if t then 1 else 0)