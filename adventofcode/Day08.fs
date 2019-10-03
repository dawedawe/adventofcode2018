namespace Adventofcode2018

module Day08 =

    [<Literal>]
    let InputFile = "Day08Input.txt"

    type Node =
        {
            ChildCount : int
            MetadataCount : int
            ChildNodes : Node []
            Metadata : int []
        }

    let emptyChild = { ChildCount = 0; MetadataCount = 0; ChildNodes = [||]; Metadata = [||] }

    let lineOfNumbersToInts (line : string) =
        line.Split [|' '|]
        |> Array.map int

    let buildNode (numbers : int []) =
        let rec buildNode' (numbers : int []) =
            let childCount = numbers.[0]
            let metadataCount = numbers.[1]
            let childNodes = Array.create childCount emptyChild
            let mutable selfShoudSkip = 2
            for c in [0 .. (childCount - 1)] do
                let childNode, toSkip = buildNode' numbers.[selfShoudSkip..]
                childNodes.[c] <- childNode
                selfShoudSkip <- selfShoudSkip + toSkip
            let node = {
                             ChildCount = childCount
                             MetadataCount = metadataCount
                             ChildNodes = childNodes
                             Metadata = numbers.[selfShoudSkip..(selfShoudSkip + metadataCount - 1)]
                       }
            let parentShouldSkip = selfShoudSkip + metadataCount
            node, parentShouldSkip
        let n, _ = buildNode' numbers
        n

    let sumMetaData node =
        let rec sumMetaData' (node : Node) (acc : int) : int =
            let mutable acc' = acc + (Array.sum node.Metadata)
            for c in node.ChildNodes do
                acc' <- sumMetaData' c acc'
            acc'
        sumMetaData' node 0

    let day08 () =
        let line = System.IO.File.ReadAllText InputFile
        let numbers = lineOfNumbersToInts line
        let node = buildNode numbers
        sumMetaData node
    
    let sumMetaDataPart2 node =
        let rec sumMetaDataPart2' (node : Node) (acc : int) : int =
            match node with
            | n when n.ChildCount = 0 -> let mutable acc' = acc + (Array.sum n.Metadata)
                                         for c in node.ChildNodes do
                                            acc' <- sumMetaDataPart2' c acc'
                                         acc'
            | n                       -> let mutable acc' = acc
                                         for metadataValue in n.Metadata do
                                            let indexedChild = Array.tryItem (metadataValue - 1) n.ChildNodes
                                            match indexedChild with
                                            | Some child -> acc' <- sumMetaDataPart2' child acc'
                                            | None       -> ()
                                         acc'
    
        sumMetaDataPart2' node 0

    let day08Part2 () =
        let line = System.IO.File.ReadAllText InputFile
        let numbers = lineOfNumbersToInts line
        let node = buildNode numbers
        sumMetaDataPart2 node
        