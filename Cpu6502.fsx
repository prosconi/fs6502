#load "OpCodes.fsx"

open System
open OpCodes

type Memory(ram: byte[]) =
    member _.Get(index: uint16)    = ram[int index]
    member _.Get(index: byte)      = ram[int index]
    member _.Set(index: uint16, v) = ram[int index] <- v
    member _.Set(index: byte, v)   = ram[int index] <- v
    member _.Length = ram.Length

type Cpu6502(memory: Memory) =

    // Registers
    // PC    program counter    (16 bit)
    // AC    accumulator    (8 bit)
    // X    X register    (8 bit)
    // Y    Y register    (8 bit)
    // SR    status register [NV-BDIZC]    (8 bit)
    // SP    stack pointer    (8 bit)
    // Note: The status register (SR) is also known as the P register.

    // SR Flags (bit 7 to bit 0)
    // N    Negative
    // V    Overflow
    // -    ignored
    // B    Break
    // D    Decimal (use BCD for arithmetics)
    // I    Interrupt (IRQ disable)
    // Z    Zero
    // C    Carry
    let mutable programCounter = 0us
    let mutable accumulator    = 0uy
    let mutable xRegister      = 0uy
    let mutable yRegister      = 0uy
    let mutable stackPointer   = 0uy
    let mutable statusNegative = false
    let mutable statusOverflow = false
    let mutable statusIgnore   = false
    let mutable statusBreak    = false
    let mutable statusDecimal  = false
    let mutable statusIrq      = false
    let mutable statusZero     = false
    let mutable statusCarry    = false

    let incrementProgramCounter() =
        programCounter <- programCounter + 1us

    let fetch() =
        let value = memory.Get(programCounter)
        incrementProgramCounter()
        value

    let fetchUInt16() =
        BitConverter.ToUInt16([| fetch(); fetch() |], 0)

    let execute() =
        let opCode = fetch()

        match opCode with
        | LDA.LDA_IMMEDIATE -> accumulator <- fetch()
        | LDA.LDA_ZEROPAGE  -> accumulator <- memory.Get(fetch())
        | LDA.LDA_ZEROPAGEX -> failwithf "What does LDA_ZEROPAGEX mean?"
        | LDA.LDA_ABSOLUTE  -> accumulator <- memory.Get(fetchUInt16())
        | LDA.LDA_ABSOLUTEX -> accumulator <- memory.Get(fetchUInt16() + uint16 xRegister)
        | LDA.LDA_ABSOLUTEY -> accumulator <- memory.Get(fetchUInt16() + uint16 yRegister)
        | LDA.LDA_INDIRECTX -> accumulator <- memory.Get(fetch() + xRegister)
        | LDA.LDA_INDIRECTY -> accumulator <- memory.Get(fetch() + yRegister)

        | LDX.LDX_IMMEDIATE -> xRegister <- fetch()
        | LDX.LDX_ZEROPAGE  -> xRegister <- memory.Get(fetch())
        | LDX.LDX_ZEROPAGEY -> failwithf "What does LDX_ZEROPAGEY mean?"
        | LDX.LDX_ABSOLUTE  -> xRegister <- memory.Get(fetchUInt16())
        | LDX.LDX_ABSOLUTEY -> xRegister <- memory.Get(fetchUInt16() + uint16 yRegister)

        | LDY.LDY_IMMEDIATE -> yRegister <- fetch()
        | LDY.LDY_ZEROPAGE  -> yRegister <- memory.Get(fetch())
        | LDY.LDY_ZEROPAGEX -> failwithf "What does LDY_ZEROPAGEX mean?"
        | LDY.LDY_ABSOLUTE  -> yRegister <- memory.Get(fetchUInt16())
        | LDY.LDY_ABSOLUTEX -> yRegister <- memory.Get(fetchUInt16() + uint16 xRegister)

        | STA.STA_ZEROPAGE  -> memory.Set(fetch(), accumulator)
        | STA.STA_ZEROPAGEX -> failwithf "What does STA_ZEROPAGEX mean?"
        | STA.STA_ABSOLUTE  -> memory.Set(fetchUInt16(), accumulator)
        | STA.STA_ABSOLUTEX -> memory.Set(fetchUInt16() + uint16 xRegister, accumulator)
        | STA.STA_ABSOLUTEY -> memory.Set(fetchUInt16() + uint16 yRegister, accumulator)
        | STA.STA_INDIRECTX -> memory.Set(fetch() + xRegister, accumulator)
        | STA.STA_INDIRECTY -> memory.Set(fetch() + yRegister, accumulator)

        | STX.STX_ZEROPAGE  -> memory.Set(fetch(), xRegister)
        | STX.STX_ZEROPAGEY -> failwithf "What does STX_ZEROPAGEY mean?"
        | STX.STX_ABSOLUTE  -> memory.Set(fetchUInt16(), xRegister)

        | STY.STY_ZEROPAGE  -> memory.Set(fetch(), yRegister)
        | STY.STY_ZEROPAGEX -> failwithf "What does STX_ZEROPAGEY mean?"
        | STY.STY_ABSOLUTE  -> memory.Set(fetchUInt16(), yRegister)

        | RegisterInstructions.TAX -> xRegister <- accumulator
        | RegisterInstructions.TXA -> accumulator <- xRegister
        | RegisterInstructions.DEX -> xRegister <- xRegister - 1uy
        | RegisterInstructions.INX -> xRegister <- xRegister + 1uy
        | RegisterInstructions.TAY -> yRegister <- accumulator
        | RegisterInstructions.TYA -> accumulator <- yRegister
        | RegisterInstructions.DEY -> yRegister <- yRegister - 1uy
        | RegisterInstructions.INY -> yRegister <- yRegister + 1uy

        | AND.AND_IMMEDIATE -> accumulator <- (fetch() &&& accumulator)
        | AND.AND_ZEROPAGE  -> accumulator <- (memory.Get(fetch()) &&& accumulator)
        | AND.AND_ZEROPAGEX -> failwithf "What does LDA_ZEROPAGEX mean?"
        | AND.AND_ABSOLUTE  -> accumulator <- (memory.Get(fetchUInt16()) &&& accumulator)
        | AND.AND_ABSOLUTEX -> accumulator <- (memory.Get(fetchUInt16() + uint16 xRegister) &&& accumulator)
        | AND.AND_ABSOLUTEY -> accumulator <- (memory.Get(fetchUInt16() + uint16 yRegister) &&& accumulator)
        | AND.AND_INDIRECTX -> accumulator <- (memory.Get(fetch() + xRegister) &&& accumulator)
        | AND.AND_INDIRECTY -> accumulator <- (memory.Get(fetch() + yRegister) &&& accumulator)

        | ADC.ADC_IMMEDIATE -> accumulator <- (fetch() + accumulator)
        | ADC.ADC_ZEROPAGE  -> accumulator <- (memory.Get(fetch()) + accumulator)
        | ADC.ADC_ZEROPAGEX -> failwithf "What does LDA_ZEROPAGEX mean?"
        | ADC.ADC_ABSOLUTE  -> accumulator <- (memory.Get(fetchUInt16()) + accumulator)
        | ADC.ADC_ABSOLUTEX -> accumulator <- (memory.Get(fetchUInt16() + uint16 xRegister) + accumulator)
        | ADC.ADC_ABSOLUTEY -> accumulator <- (memory.Get(fetchUInt16() + uint16 yRegister) + accumulator)
        | ADC.ADC_INDIRECTX -> accumulator <- (memory.Get(fetch() + xRegister) + accumulator)
        | ADC.ADC_INDIRECTY -> accumulator <- (memory.Get(fetch() + yRegister) + accumulator)
        
        | _ -> printfn "Unrecognized opCode: %d" opCode

    let go() =
        while int(programCounter) <= memory.Length do
            execute() |> ignore

    member _.State =
        {| ProgramCounter = programCounter
           Accumulator    = accumulator
           XRegister      = xRegister
           YRegister      = yRegister
           StackPointer   = stackPointer
           StatusNegative = statusNegative
           StatusOverflow = statusOverflow
           StatusIgnore   = statusIgnore
           StatusBreak    = statusBreak
           StatusDecimal  = statusDecimal
           StatusIrq      = statusIrq
           StatusZero     = statusZero
           StatusCarry    = statusCarry |} 

    member this.Execute() = 
        execute()
        this.State

let program = System.IO.File.ReadAllBytes(@"C:\Source\NES\DevEnvironmentDemo\demo.nes")
let cpu = Cpu6502(Memory(program))
cpu.Execute()