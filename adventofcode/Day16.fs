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

    let allOpcodes =
        [
            addr; addi; mulr; muli; banr; bani; borr; bori;
            setr; seti; gtir; gtri; gtrr; eqir; eqri; eqrr
        ]

    let allOpcodesNamed : (string * (Instruction -> RegisterState -> int []) * int Option) [] =
        [|
            ("addr", addr, None);
            ("addi", addi, None);
            ("mulr", mulr, None);
            ("muli", muli, None);
            ("banr", banr, None);
            ("bani", bani, None);
            ("borr", borr, None);
            ("bori", bori, None);
            ("setr", setr, None);
            ("seti", seti, None);
            ("gtir", gtir, None);
            ("gtri", gtri, None);
            ("gtrr", gtrr, None);
            ("eqir", eqir, None);
            ("eqri", eqri, None);
            ("eqrr", eqrr, None)
        |]

    let testOpcode opcode (sample : Sample) =
        let result = opcode sample.Inst sample.Before
        result = sample.After

    let behaveLike3OrMore sample =
        List.map (fun o -> testOpcode o sample) allOpcodes
        |> List.sumBy (fun t -> if t then 1 else 0) >= 3

    let getMatchingOpcodes (namedOpcodes : (string * (Instruction -> RegisterState -> int []) * int Option) []) sample =
        let r = Array.filter (fun (_, o, _) -> testOpcode o sample) namedOpcodes
        r

    let day16 () =
        let samples = getSamples InputFile
        List.map behaveLike3OrMore samples
        |> List.sumBy (fun t -> if t then 1 else 0)


    let rec mapOpcodesToNumber opcodesNamedNumbered samples =
        if (Option.isNone (Array.tryFind (fun (_, _, n) -> Option.isNone n) opcodesNamedNumbered))
        then opcodesNamedNumbered
        else
             let opcodesToTry = Array.filter (fun (_, _, n) -> Option.isNone n) opcodesNamedNumbered
             let r = List.map (fun s -> (s.Inst.Number, getMatchingOpcodes opcodesToTry s)) samples
             let m = List.filter (fun (_, matches) -> Array.length matches = 1) r
                     |> Map.ofList
             if Map.isEmpty m
             then
                 printfn "no unambiguous found"
                 opcodesNamedNumbered
             else
                 for e in m do
                    let (name, op, _) = e.Value.[0]
                    let index = Array.findIndex (fun (x1, _, _) -> x1 = name) opcodesNamedNumbered
                    opcodesNamedNumbered.[index] <- (name, op, Some e.Key)
                 mapOpcodesToNumber opcodesNamedNumbered samples

    let getTestProgram path =
        let mutable instructions = List.empty
        let input = System.IO.File.ReadAllLines path
        let lastAfter = Array.FindLastIndex(input, (fun l -> l.StartsWith("After:")))
        let indexToStart = lastAfter + 4
        for i in [indexToStart .. input.Length - 1] do
            let instruction = parseInstruction input.[i]
            instructions <- instructions @ List.singleton instruction
        instructions            

    let computeProgram (instructions : Instruction list) (opcodeMap : (int * (Instruction -> RegisterState -> int []) * string) []) =
        let mutable state = [| 0; 0; 0; 0; |]
        for i in instructions do
            let opToLookUp = i.Number
            let (opNumber, op, opName) = opcodeMap.[opToLookUp]
            let r = op i state
            state <- r
        state

    let day16Part2 () =
        let samples = getSamples InputFile
        let m = mapOpcodesToNumber allOpcodesNamed samples
                |> Array.map (fun (name, op, number : int Option) -> (number.Value, op, name))
                |> Array.sortBy (fun (n, _, _) -> n)
        let testProgram = getTestProgram InputFile
        let r = computeProgram testProgram m
        r