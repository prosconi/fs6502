open System
open System.IO
open System.Xml

type AddressMode =
    {
        Addressing: string
        Assembler: string
        OpCode: string
        Bytes: string
        Cycles: byte
        CycleMode: string
    }

type OpCode =
    {
        Name: string
        Summary: string
        Synopsis: string
        AddressingModes: AddressMode[]
    }

let inputFile = Path.Combine(__SOURCE_DIRECTORY__, "OpCodes.html")
let xml =
    File.ReadAllText(inputFile)
        .Replace("<br>", "<br />")

let xmlDoc = XmlDocument()
xmlDoc.LoadXml(xml)

let dlNode = xmlDoc.ChildNodes.Item(0)

let mapCycles (str: string) =
    let cycles = str.Trim().Trim('*') |> byte
    let mode = 
        if   str.Contains "**" then "Option2"
        elif str.Contains "*"  then "Option1"
        else "Standard"

    cycles, mode

let mapAddressingString (str: string) =
    match str.ToLower().Trim() with
    | "accumulator"  -> "Accumulator"
    | "implied"      -> "Implied"
    | "immediate"    -> "Immediate"
    | "zeropage"     -> "ZeroPage"
    | "zeropage,x"   -> "ZeroPageX"
    | "zeropage,y"   -> "ZeroPageY"
    | "relative"     -> "Relative"
    | "absolute"     -> "Absolute"
    | "absolute,x"   -> "AbsoluteX"
    | "absolute,y"   -> "AbsoluteY"
    | "indirect"     -> "Indirect"
    | "(indirect,x)" -> "IndirectX"
    | "(indirect),y" -> "IndirectY"
    | _ -> failwithf "Unrecognized addressing mode: %s" str

let opCodes =
    seq { 0.. 2.. dlNode.ChildNodes.Count - 1 }
    |> Seq.map (fun i ->
        let name = dlNode.ChildNodes[i].InnerText
        printfn "parsing opcode mode: %s [%d] " name i
        let definitionNode = dlNode.ChildNodes[i + 1]
        let secondNodeAria = definitionNode.ChildNodes[1].Attributes["aria-label"]

        // sometimes there is "notes on implementation" node, handle that situation
        let summaryIndex, synopsisIndex, flagsIndex, detailsIndex =
            match secondNodeAria.InnerText.ToLower().Trim() with
            | "notes on the implementation" -> 0, 2, 3, 4
            | _ -> 0, 1, 2, 3

        let summary = definitionNode.ChildNodes[summaryIndex].InnerText
        let synopsis = definitionNode.ChildNodes[synopsisIndex].InnerText
        let flags = definitionNode.ChildNodes[flagsIndex]
        let details = definitionNode.ChildNodes[detailsIndex] // table
        let tbodyNode = details.ChildNodes[0] // tbody
        let addressingModes =
            seq { 1..tbodyNode.ChildNodes.Count - 1 }
            |> Seq.map (fun index ->
                let row = tbodyNode.ChildNodes[index]
                let addressing = row.ChildNodes[0].InnerText
                let assembler = row.ChildNodes[1].InnerText
                let opc = row.ChildNodes[2].InnerText
                let bytes = row.ChildNodes[3].InnerText
                let cycles = row.ChildNodes[4].InnerText
                let cycles, mode = mapCycles cycles
                { Addressing = mapAddressingString addressing
                  Assembler = assembler
                  OpCode = opc
                  Bytes = bytes
                  Cycles = cycles
                  CycleMode = mode }
            )
            |> Seq.toArray

        { Name = name
          Summary = summary
          Synopsis = synopsis
          AddressingModes = addressingModes }
    )
    |> Seq.toArray

opCodes
|> Array.collect (fun x ->
    x.AddressingModes
    |> Array.map (fun y -> sprintf """            { Action = instruction%s; Name = "%s"; OpCode = 0x%suy; Cycles = %duy; CycleMode = %s; AddressMode = %s }""" x.Name x.Name y.OpCode y.Cycles y.CycleMode y.Addressing)
)
|> Array.iter (printfn "%s")


opCodes
|> Array.map (fun x -> $"let instruction{x.Name}() = ()")
|> Array.iter (printfn "%s")
