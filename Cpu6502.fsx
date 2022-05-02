#load "OpCodes.fsx"

open System
open OpCodes

type Memory(ram: byte[]) =
    member _.Get(index: uint16)    = ram[int index]
    member _.Get(index: byte)      = ram[int index]
    member _.Set(index: uint16, v) = ram[int index] <- v
    member _.Set(index: byte, v)   = ram[int index] <- v
    member _.Length = ram.Length

// Address Modes
// A    Accumulator    OPC A    operand is AC (implied single byte instruction)
// abs    absolute    OPC $LLHH    operand is address $HHLL *
// abs,X    absolute, X-indexed    OPC $LLHH,X    operand is address; effective address is address incremented by X with carry **
// abs,Y    absolute, Y-indexed    OPC $LLHH,Y    operand is address; effective address is address incremented by Y with carry **
// #    immediate    OPC #$BB    operand is byte BB
// impl    implied    OPC    operand implied
// ind    indirect    OPC ($LLHH)    operand is address; effective address is contents of word at address: C.w($HHLL)
// X,ind    X-indexed, indirect    OPC ($LL,X)    operand is zeropage address; effective address is word in (LL + X, LL + X + 1), inc. without carry: C.w($00LL + X)
// ind,Y    indirect, Y-indexed    OPC ($LL),Y    operand is zeropage address; effective address is word in (LL, LL + 1) incremented by Y with carry: C.w($00LL) + Y
// rel    relative    OPC $BB    branch target is PC + signed offset BB ***
// zpg    zeropage    OPC $LL    operand is zeropage address (hi-byte is zero, address = $00LL)
// zpg,X    zeropage, X-indexed    OPC $LL,X    operand is zeropage address; effective address is address incremented by X without carry **
// zpg,Y    zeropage, Y-indexed    OPC $LL,Y    operand is zeropage address; effective address is address incremented by Y without carry **
// *
// 16-bit address words are little endian, lo(w)-byte first, followed by the hi(gh)-byte.
// (An assembler will use a human readable, big-endian notation as in $HHLL.)
// **
// The available 16-bit address space is conceived as consisting of pages of 256 bytes each, with
// address hi-bytes represententing the page index. An increment with carry may affect the hi-byte
// and may thus result in a crossing of page boundaries, adding an extra cycle to the execution.
// Increments without carry do not affect the hi-byte of an address and no page transitions do occur.
// Generally, increments of 16-bit addresses include a carry, increments of zeropage addresses don't.
// Notably this is not related in any way to the state of the carry bit of the accumulator.
// ***
// Branch offsets are signed 8-bit values, -128 ... +127, negative offsets in two's complement.
// Page transitions may occur and add an extra cycle to the exucution.
type AddressMode =
    | Accumulator
    | Implied
    | Immediate
    | ZeroPage
    | ZeroPageX
    | ZeroPageY
    | Relative
    | Absolute
    | AbsoluteX
    | AbsoluteY
    | Indirect
    | IndirectX
    | IndirectY

type CycleMode =
    | Standard
    | Option1
    | Option2

type Instruction =
    {
        Name: string
        OpCode: byte
        AddressMode: AddressMode
        Cycles: byte
        CycleMode: CycleMode
        Action: uint16 -> unit
    }

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
        let b = memory.Get(programCounter)
        incrementProgramCounter()
        b

    let fetchUInt16() =
        BitConverter.ToUInt16([| fetch(); fetch() |], 0)

    let readZeroPage() =
        memory.Get(fetch())

    let readAbsolute() =
        memory.Get(fetchUInt16())

    let readAbsoluteOffset(offset) =
        memory.Get(fetchUInt16() + offset)

    let addressModeAccumulator() = failwithf "Not implmented"
    let addressModeImplied() = 0us
    let addressModeImmediate() = uint16(fetch())
    let addressModeZeroPage() = uint16(memory.Get(fetch()))
    let addressModeZeroPageX() = uint16(memory.Get(fetch() + xRegister))
    let addressModeZeroPageY() = uint16(memory.Get(fetch() + yRegister))
    let addressModeRelative() = failwithf "Not implmented"
    let addressModeAbsolute() = uint16(memory.Get(fetchUInt16()))
    let addressModeAbsoluteX() = uint16(memory.Get(fetchUInt16() + uint16 xRegister))
    let addressModeAbsoluteY() = uint16(memory.Get(fetchUInt16() + uint16 yRegister))
    let addressModeIndirect() = failwithf "Not implmented"
    let addressModeIndirectX() = failwithf "Not implmented"
    let addressModeIndirectY() = failwithf "Not implmented"

    let instructionADC(b: uint16) = accumulator <- accumulator + byte b
    let instructionAND(b: uint16) = accumulator <- accumulator &&& byte b
    let instructionASL(b: uint16) = ()
    let instructionBCC(b: uint16) = ()
    let instructionBCS(b: uint16) = ()
    let instructionBEQ(b: uint16) = ()
    let instructionBIT(b: uint16) = ()
    let instructionBMI(b: uint16) = ()
    let instructionBNE(b: uint16) = ()
    let instructionBPL(b: uint16) = ()
    let instructionBRK(b: uint16) = ()
    let instructionBVC(b: uint16) = ()
    let instructionBVS(b: uint16) = ()
    let instructionCLC(b: uint16) = ()
    let instructionCLD(b: uint16) = ()
    let instructionCLI(b: uint16) = ()
    let instructionCLV(b: uint16) = ()
    let instructionCMP(b: uint16) = ()
    let instructionCPX(b: uint16) = ()
    let instructionCPY(b: uint16) = ()
    let instructionDEC(b: uint16) = ()
    let instructionDEX(b: uint16) = ()
    let instructionDEY(b: uint16) = ()
    let instructionEOR(b: uint16) = ()
    let instructionINC(b: uint16) = ()
    let instructionINX(b: uint16) = ()
    let instructionINY(b: uint16) = ()
    let instructionJMP(b: uint16) = ()
    let instructionJSR(b: uint16) = ()
    let instructionLDA(b: uint16) = accumulator <- byte b
    let instructionLDX(b: uint16) = xRegister <- byte b
    let instructionLDY(b: uint16) = yRegister <- byte b
    let instructionLSR(b: uint16) = ()
    let instructionNOP(b: uint16) = ()
    let instructionORA(b: uint16) = ()
    let instructionPHA(b: uint16) = ()
    let instructionPHP(b: uint16) = ()
    let instructionPLA(b: uint16) = ()
    let instructionPLP(b: uint16) = ()
    let instructionROL(b: uint16) = ()
    let instructionROR(b: uint16) = ()
    let instructionRTI(b: uint16) = ()
    let instructionRTS(b: uint16) = ()
    let instructionSBC(b: uint16) = ()
    let instructionSEC(b: uint16) = ()
    let instructionSED(b: uint16) = ()
    let instructionSEI(b: uint16) = ()
    let instructionSTA(b: uint16) = memory.Set(b, accumulator)
    let instructionSTX(b: uint16) = memory.Set(b, xRegister)
    let instructionSTY(b: uint16) = memory.Set(b, yRegister)
    let instructionTAX(b: uint16) = ()
    let instructionTAY(b: uint16) = ()
    let instructionTSX(b: uint16) = ()
    let instructionTXA(b: uint16) = ()
    let instructionTXS(b: uint16) = ()
    let instructionTYA(b: uint16) = ()

    let instructions =
        [|
            { Action = instructionADC; Name = "ADC"; OpCode = 0x69uy; Cycles = 2uy; CycleMode = Standard; AddressMode = Immediate }
            { Action = instructionADC; Name = "ADC"; OpCode = 0x65uy; Cycles = 3uy; CycleMode = Standard; AddressMode = ZeroPage }
            { Action = instructionADC; Name = "ADC"; OpCode = 0x75uy; Cycles = 4uy; CycleMode = Standard; AddressMode = ZeroPageX }
            { Action = instructionADC; Name = "ADC"; OpCode = 0x6Duy; Cycles = 4uy; CycleMode = Standard; AddressMode = Absolute }
            { Action = instructionADC; Name = "ADC"; OpCode = 0x7Duy; Cycles = 4uy; CycleMode = Option1; AddressMode = AbsoluteX }
            { Action = instructionADC; Name = "ADC"; OpCode = 0x79uy; Cycles = 4uy; CycleMode = Option1; AddressMode = AbsoluteY }
            { Action = instructionADC; Name = "ADC"; OpCode = 0x61uy; Cycles = 6uy; CycleMode = Standard; AddressMode = IndirectX }
            { Action = instructionADC; Name = "ADC"; OpCode = 0x71uy; Cycles = 5uy; CycleMode = Option1; AddressMode = IndirectY }
            { Action = instructionAND; Name = "AND"; OpCode = 0x29uy; Cycles = 2uy; CycleMode = Standard; AddressMode = Immediate }
            { Action = instructionAND; Name = "AND"; OpCode = 0x25uy; Cycles = 3uy; CycleMode = Standard; AddressMode = ZeroPage }
            { Action = instructionAND; Name = "AND"; OpCode = 0x35uy; Cycles = 4uy; CycleMode = Standard; AddressMode = ZeroPageX }
            { Action = instructionAND; Name = "AND"; OpCode = 0x2Duy; Cycles = 4uy; CycleMode = Standard; AddressMode = Absolute }
            { Action = instructionAND; Name = "AND"; OpCode = 0x3Duy; Cycles = 4uy; CycleMode = Option1; AddressMode = AbsoluteX }
            { Action = instructionAND; Name = "AND"; OpCode = 0x39uy; Cycles = 4uy; CycleMode = Option1; AddressMode = AbsoluteY }
            { Action = instructionAND; Name = "AND"; OpCode = 0x21uy; Cycles = 6uy; CycleMode = Standard; AddressMode = IndirectX }
            { Action = instructionAND; Name = "AND"; OpCode = 0x31uy; Cycles = 5uy; CycleMode = Option1; AddressMode = IndirectY }
            { Action = instructionASL; Name = "ASL"; OpCode = 0x0Auy; Cycles = 2uy; CycleMode = Standard; AddressMode = Accumulator }
            { Action = instructionASL; Name = "ASL"; OpCode = 0x06uy; Cycles = 5uy; CycleMode = Standard; AddressMode = ZeroPage }
            { Action = instructionASL; Name = "ASL"; OpCode = 0x16uy; Cycles = 6uy; CycleMode = Standard; AddressMode = ZeroPageX }
            { Action = instructionASL; Name = "ASL"; OpCode = 0x0Euy; Cycles = 6uy; CycleMode = Standard; AddressMode = Absolute }
            { Action = instructionASL; Name = "ASL"; OpCode = 0x1Euy; Cycles = 7uy; CycleMode = Standard; AddressMode = AbsoluteX }
            { Action = instructionBCC; Name = "BCC"; OpCode = 0x90uy; Cycles = 2uy; CycleMode = Option2; AddressMode = Relative }
            { Action = instructionBCS; Name = "BCS"; OpCode = 0xB0uy; Cycles = 2uy; CycleMode = Option2; AddressMode = Relative }
            { Action = instructionBEQ; Name = "BEQ"; OpCode = 0xF0uy; Cycles = 2uy; CycleMode = Option2; AddressMode = Relative }
            { Action = instructionBIT; Name = "BIT"; OpCode = 0x24uy; Cycles = 3uy; CycleMode = Standard; AddressMode = ZeroPage }
            { Action = instructionBIT; Name = "BIT"; OpCode = 0x2Cuy; Cycles = 4uy; CycleMode = Standard; AddressMode = Absolute }
            { Action = instructionBMI; Name = "BMI"; OpCode = 0x30uy; Cycles = 2uy; CycleMode = Option2; AddressMode = Relative }
            { Action = instructionBNE; Name = "BNE"; OpCode = 0xD0uy; Cycles = 2uy; CycleMode = Option2; AddressMode = Relative }
            { Action = instructionBPL; Name = "BPL"; OpCode = 0x10uy; Cycles = 2uy; CycleMode = Option2; AddressMode = Relative }
            { Action = instructionBRK; Name = "BRK"; OpCode = 0x00uy; Cycles = 7uy; CycleMode = Standard; AddressMode = Implied }
            { Action = instructionBVC; Name = "BVC"; OpCode = 0x50uy; Cycles = 2uy; CycleMode = Option2; AddressMode = Relative }
            { Action = instructionBVS; Name = "BVS"; OpCode = 0x70uy; Cycles = 2uy; CycleMode = Option2; AddressMode = Relative }
            { Action = instructionCLC; Name = "CLC"; OpCode = 0x18uy; Cycles = 2uy; CycleMode = Standard; AddressMode = Implied }
            { Action = instructionCLD; Name = "CLD"; OpCode = 0xD8uy; Cycles = 2uy; CycleMode = Standard; AddressMode = Implied }
            { Action = instructionCLI; Name = "CLI"; OpCode = 0x58uy; Cycles = 2uy; CycleMode = Standard; AddressMode = Implied }
            { Action = instructionCLV; Name = "CLV"; OpCode = 0xB8uy; Cycles = 2uy; CycleMode = Standard; AddressMode = Implied }
            { Action = instructionCMP; Name = "CMP"; OpCode = 0xC9uy; Cycles = 2uy; CycleMode = Standard; AddressMode = Immediate }
            { Action = instructionCMP; Name = "CMP"; OpCode = 0xC5uy; Cycles = 3uy; CycleMode = Standard; AddressMode = ZeroPage }
            { Action = instructionCMP; Name = "CMP"; OpCode = 0xD5uy; Cycles = 4uy; CycleMode = Standard; AddressMode = ZeroPageX }
            { Action = instructionCMP; Name = "CMP"; OpCode = 0xCDuy; Cycles = 4uy; CycleMode = Standard; AddressMode = Absolute }
            { Action = instructionCMP; Name = "CMP"; OpCode = 0xDDuy; Cycles = 4uy; CycleMode = Option1; AddressMode = AbsoluteX }
            { Action = instructionCMP; Name = "CMP"; OpCode = 0xD9uy; Cycles = 4uy; CycleMode = Option1; AddressMode = AbsoluteY }
            { Action = instructionCMP; Name = "CMP"; OpCode = 0xC1uy; Cycles = 6uy; CycleMode = Standard; AddressMode = IndirectX }
            { Action = instructionCMP; Name = "CMP"; OpCode = 0xD1uy; Cycles = 5uy; CycleMode = Option1; AddressMode = IndirectY }
            { Action = instructionCPX; Name = "CPX"; OpCode = 0xE0uy; Cycles = 2uy; CycleMode = Standard; AddressMode = Immediate }
            { Action = instructionCPX; Name = "CPX"; OpCode = 0xE4uy; Cycles = 3uy; CycleMode = Standard; AddressMode = ZeroPage }
            { Action = instructionCPX; Name = "CPX"; OpCode = 0xECuy; Cycles = 4uy; CycleMode = Standard; AddressMode = Absolute }
            { Action = instructionCPY; Name = "CPY"; OpCode = 0xC0uy; Cycles = 2uy; CycleMode = Standard; AddressMode = Immediate }
            { Action = instructionCPY; Name = "CPY"; OpCode = 0xC4uy; Cycles = 3uy; CycleMode = Standard; AddressMode = ZeroPage }
            { Action = instructionCPY; Name = "CPY"; OpCode = 0xCCuy; Cycles = 4uy; CycleMode = Standard; AddressMode = Absolute }
            { Action = instructionDEC; Name = "DEC"; OpCode = 0xC6uy; Cycles = 5uy; CycleMode = Standard; AddressMode = ZeroPage }
            { Action = instructionDEC; Name = "DEC"; OpCode = 0xD6uy; Cycles = 6uy; CycleMode = Standard; AddressMode = ZeroPageX }
            { Action = instructionDEC; Name = "DEC"; OpCode = 0xCEuy; Cycles = 6uy; CycleMode = Standard; AddressMode = Absolute }
            { Action = instructionDEC; Name = "DEC"; OpCode = 0xDEuy; Cycles = 7uy; CycleMode = Standard; AddressMode = AbsoluteX }
            { Action = instructionDEX; Name = "DEX"; OpCode = 0xCAuy; Cycles = 2uy; CycleMode = Standard; AddressMode = Implied }
            { Action = instructionDEY; Name = "DEY"; OpCode = 0x88uy; Cycles = 2uy; CycleMode = Standard; AddressMode = Implied }
            { Action = instructionEOR; Name = "EOR"; OpCode = 0x49uy; Cycles = 2uy; CycleMode = Standard; AddressMode = Immediate }
            { Action = instructionEOR; Name = "EOR"; OpCode = 0x45uy; Cycles = 3uy; CycleMode = Standard; AddressMode = ZeroPage }
            { Action = instructionEOR; Name = "EOR"; OpCode = 0x55uy; Cycles = 4uy; CycleMode = Standard; AddressMode = ZeroPageX }
            { Action = instructionEOR; Name = "EOR"; OpCode = 0x4Duy; Cycles = 4uy; CycleMode = Standard; AddressMode = Absolute }
            { Action = instructionEOR; Name = "EOR"; OpCode = 0x5Duy; Cycles = 4uy; CycleMode = Option1; AddressMode = AbsoluteX }
            { Action = instructionEOR; Name = "EOR"; OpCode = 0x59uy; Cycles = 4uy; CycleMode = Option1; AddressMode = AbsoluteY }
            { Action = instructionEOR; Name = "EOR"; OpCode = 0x41uy; Cycles = 6uy; CycleMode = Standard; AddressMode = IndirectX }
            { Action = instructionEOR; Name = "EOR"; OpCode = 0x51uy; Cycles = 5uy; CycleMode = Option1; AddressMode = IndirectY }
            { Action = instructionINC; Name = "INC"; OpCode = 0xE6uy; Cycles = 5uy; CycleMode = Standard; AddressMode = ZeroPage }
            { Action = instructionINC; Name = "INC"; OpCode = 0xF6uy; Cycles = 6uy; CycleMode = Standard; AddressMode = ZeroPageX }
            { Action = instructionINC; Name = "INC"; OpCode = 0xEEuy; Cycles = 6uy; CycleMode = Standard; AddressMode = Absolute }
            { Action = instructionINC; Name = "INC"; OpCode = 0xFEuy; Cycles = 7uy; CycleMode = Standard; AddressMode = AbsoluteX }
            { Action = instructionINX; Name = "INX"; OpCode = 0xE8uy; Cycles = 2uy; CycleMode = Standard; AddressMode = Implied }
            { Action = instructionINY; Name = "INY"; OpCode = 0xC8uy; Cycles = 2uy; CycleMode = Standard; AddressMode = Implied }
            { Action = instructionJMP; Name = "JMP"; OpCode = 0x4Cuy; Cycles = 3uy; CycleMode = Standard; AddressMode = Absolute }
            { Action = instructionJMP; Name = "JMP"; OpCode = 0x6Cuy; Cycles = 5uy; CycleMode = Standard; AddressMode = Indirect }
            { Action = instructionJSR; Name = "JSR"; OpCode = 0x20uy; Cycles = 6uy; CycleMode = Standard; AddressMode = Absolute }
            { Action = instructionLDA; Name = "LDA"; OpCode = 0xA9uy; Cycles = 2uy; CycleMode = Standard; AddressMode = Immediate }
            { Action = instructionLDA; Name = "LDA"; OpCode = 0xA5uy; Cycles = 3uy; CycleMode = Standard; AddressMode = ZeroPage }
            { Action = instructionLDA; Name = "LDA"; OpCode = 0xB5uy; Cycles = 4uy; CycleMode = Standard; AddressMode = ZeroPageX }
            { Action = instructionLDA; Name = "LDA"; OpCode = 0xADuy; Cycles = 4uy; CycleMode = Standard; AddressMode = Absolute }
            { Action = instructionLDA; Name = "LDA"; OpCode = 0xBDuy; Cycles = 4uy; CycleMode = Option1; AddressMode = AbsoluteX }
            { Action = instructionLDA; Name = "LDA"; OpCode = 0xB9uy; Cycles = 4uy; CycleMode = Option1; AddressMode = AbsoluteY }
            { Action = instructionLDA; Name = "LDA"; OpCode = 0xA1uy; Cycles = 6uy; CycleMode = Standard; AddressMode = IndirectX }
            { Action = instructionLDA; Name = "LDA"; OpCode = 0xB1uy; Cycles = 5uy; CycleMode = Option1; AddressMode = IndirectY }
            { Action = instructionLDX; Name = "LDX"; OpCode = 0xA2uy; Cycles = 2uy; CycleMode = Standard; AddressMode = Immediate }
            { Action = instructionLDX; Name = "LDX"; OpCode = 0xA6uy; Cycles = 3uy; CycleMode = Standard; AddressMode = ZeroPage }
            { Action = instructionLDX; Name = "LDX"; OpCode = 0xB6uy; Cycles = 4uy; CycleMode = Standard; AddressMode = ZeroPageY }
            { Action = instructionLDX; Name = "LDX"; OpCode = 0xAEuy; Cycles = 4uy; CycleMode = Standard; AddressMode = Absolute }
            { Action = instructionLDX; Name = "LDX"; OpCode = 0xBEuy; Cycles = 4uy; CycleMode = Option1; AddressMode = AbsoluteY }
            { Action = instructionLDY; Name = "LDY"; OpCode = 0xA0uy; Cycles = 2uy; CycleMode = Standard; AddressMode = Immediate }
            { Action = instructionLDY; Name = "LDY"; OpCode = 0xA4uy; Cycles = 3uy; CycleMode = Standard; AddressMode = ZeroPage }
            { Action = instructionLDY; Name = "LDY"; OpCode = 0xB4uy; Cycles = 4uy; CycleMode = Standard; AddressMode = ZeroPageX }
            { Action = instructionLDY; Name = "LDY"; OpCode = 0xACuy; Cycles = 4uy; CycleMode = Standard; AddressMode = Absolute }
            { Action = instructionLDY; Name = "LDY"; OpCode = 0xBCuy; Cycles = 4uy; CycleMode = Option1; AddressMode = AbsoluteX }
            { Action = instructionLSR; Name = "LSR"; OpCode = 0x4Auy; Cycles = 2uy; CycleMode = Standard; AddressMode = Accumulator }
            { Action = instructionLSR; Name = "LSR"; OpCode = 0x46uy; Cycles = 5uy; CycleMode = Standard; AddressMode = ZeroPage }
            { Action = instructionLSR; Name = "LSR"; OpCode = 0x56uy; Cycles = 6uy; CycleMode = Standard; AddressMode = ZeroPageX }
            { Action = instructionLSR; Name = "LSR"; OpCode = 0x4Euy; Cycles = 6uy; CycleMode = Standard; AddressMode = Absolute }
            { Action = instructionLSR; Name = "LSR"; OpCode = 0x5Euy; Cycles = 7uy; CycleMode = Standard; AddressMode = AbsoluteX }
            { Action = instructionNOP; Name = "NOP"; OpCode = 0xEAuy; Cycles = 2uy; CycleMode = Standard; AddressMode = Implied }
            { Action = instructionORA; Name = "ORA"; OpCode = 0x09uy; Cycles = 2uy; CycleMode = Standard; AddressMode = Immediate }
            { Action = instructionORA; Name = "ORA"; OpCode = 0x05uy; Cycles = 3uy; CycleMode = Standard; AddressMode = ZeroPage }
            { Action = instructionORA; Name = "ORA"; OpCode = 0x15uy; Cycles = 4uy; CycleMode = Standard; AddressMode = ZeroPageX }
            { Action = instructionORA; Name = "ORA"; OpCode = 0x0Duy; Cycles = 4uy; CycleMode = Standard; AddressMode = Absolute }
            { Action = instructionORA; Name = "ORA"; OpCode = 0x1Duy; Cycles = 4uy; CycleMode = Option1; AddressMode = AbsoluteX }
            { Action = instructionORA; Name = "ORA"; OpCode = 0x19uy; Cycles = 4uy; CycleMode = Option1; AddressMode = AbsoluteY }
            { Action = instructionORA; Name = "ORA"; OpCode = 0x01uy; Cycles = 6uy; CycleMode = Standard; AddressMode = IndirectX }
            { Action = instructionORA; Name = "ORA"; OpCode = 0x11uy; Cycles = 5uy; CycleMode = Option1; AddressMode = IndirectY }
            { Action = instructionPHA; Name = "PHA"; OpCode = 0x48uy; Cycles = 3uy; CycleMode = Standard; AddressMode = Implied }
            { Action = instructionPHP; Name = "PHP"; OpCode = 0x08uy; Cycles = 3uy; CycleMode = Standard; AddressMode = Implied }
            { Action = instructionPLA; Name = "PLA"; OpCode = 0x68uy; Cycles = 4uy; CycleMode = Standard; AddressMode = Implied }
            { Action = instructionPLP; Name = "PLP"; OpCode = 0x28uy; Cycles = 4uy; CycleMode = Standard; AddressMode = Implied }
            { Action = instructionROL; Name = "ROL"; OpCode = 0x2Auy; Cycles = 2uy; CycleMode = Standard; AddressMode = Accumulator }
            { Action = instructionROL; Name = "ROL"; OpCode = 0x26uy; Cycles = 5uy; CycleMode = Standard; AddressMode = ZeroPage }
            { Action = instructionROL; Name = "ROL"; OpCode = 0x36uy; Cycles = 6uy; CycleMode = Standard; AddressMode = ZeroPageX }
            { Action = instructionROL; Name = "ROL"; OpCode = 0x2Euy; Cycles = 6uy; CycleMode = Standard; AddressMode = Absolute }
            { Action = instructionROL; Name = "ROL"; OpCode = 0x3Euy; Cycles = 7uy; CycleMode = Standard; AddressMode = AbsoluteX }
            { Action = instructionROR; Name = "ROR"; OpCode = 0x6Auy; Cycles = 2uy; CycleMode = Standard; AddressMode = Accumulator }
            { Action = instructionROR; Name = "ROR"; OpCode = 0x66uy; Cycles = 5uy; CycleMode = Standard; AddressMode = ZeroPage }
            { Action = instructionROR; Name = "ROR"; OpCode = 0x76uy; Cycles = 6uy; CycleMode = Standard; AddressMode = ZeroPageX }
            { Action = instructionROR; Name = "ROR"; OpCode = 0x6Euy; Cycles = 6uy; CycleMode = Standard; AddressMode = Absolute }
            { Action = instructionROR; Name = "ROR"; OpCode = 0x7Euy; Cycles = 7uy; CycleMode = Standard; AddressMode = AbsoluteX }
            { Action = instructionRTI; Name = "RTI"; OpCode = 0x40uy; Cycles = 6uy; CycleMode = Standard; AddressMode = Implied }
            { Action = instructionRTS; Name = "RTS"; OpCode = 0x60uy; Cycles = 6uy; CycleMode = Standard; AddressMode = Implied }
            { Action = instructionSBC; Name = "SBC"; OpCode = 0xE9uy; Cycles = 2uy; CycleMode = Standard; AddressMode = Immediate }
            { Action = instructionSBC; Name = "SBC"; OpCode = 0xE5uy; Cycles = 3uy; CycleMode = Standard; AddressMode = ZeroPage }
            { Action = instructionSBC; Name = "SBC"; OpCode = 0xF5uy; Cycles = 4uy; CycleMode = Standard; AddressMode = ZeroPageX }
            { Action = instructionSBC; Name = "SBC"; OpCode = 0xEDuy; Cycles = 4uy; CycleMode = Standard; AddressMode = Absolute }
            { Action = instructionSBC; Name = "SBC"; OpCode = 0xFDuy; Cycles = 4uy; CycleMode = Option1; AddressMode = AbsoluteX }
            { Action = instructionSBC; Name = "SBC"; OpCode = 0xF9uy; Cycles = 4uy; CycleMode = Option1; AddressMode = AbsoluteY }
            { Action = instructionSBC; Name = "SBC"; OpCode = 0xE1uy; Cycles = 6uy; CycleMode = Standard; AddressMode = IndirectX }
            { Action = instructionSBC; Name = "SBC"; OpCode = 0xF1uy; Cycles = 5uy; CycleMode = Option1; AddressMode = IndirectY }
            { Action = instructionSEC; Name = "SEC"; OpCode = 0x38uy; Cycles = 2uy; CycleMode = Standard; AddressMode = Implied }
            { Action = instructionSED; Name = "SED"; OpCode = 0xF8uy; Cycles = 2uy; CycleMode = Standard; AddressMode = Implied }
            { Action = instructionSEI; Name = "SEI"; OpCode = 0x78uy; Cycles = 2uy; CycleMode = Standard; AddressMode = Implied }
            { Action = instructionSTA; Name = "STA"; OpCode = 0x85uy; Cycles = 3uy; CycleMode = Standard; AddressMode = ZeroPage }
            { Action = instructionSTA; Name = "STA"; OpCode = 0x95uy; Cycles = 4uy; CycleMode = Standard; AddressMode = ZeroPageX }
            { Action = instructionSTA; Name = "STA"; OpCode = 0x8Duy; Cycles = 4uy; CycleMode = Standard; AddressMode = Absolute }
            { Action = instructionSTA; Name = "STA"; OpCode = 0x9Duy; Cycles = 5uy; CycleMode = Standard; AddressMode = AbsoluteX }
            { Action = instructionSTA; Name = "STA"; OpCode = 0x99uy; Cycles = 5uy; CycleMode = Standard; AddressMode = AbsoluteY }
            { Action = instructionSTA; Name = "STA"; OpCode = 0x81uy; Cycles = 6uy; CycleMode = Standard; AddressMode = IndirectX }
            { Action = instructionSTA; Name = "STA"; OpCode = 0x91uy; Cycles = 6uy; CycleMode = Standard; AddressMode = IndirectY }
            { Action = instructionSTX; Name = "STX"; OpCode = 0x86uy; Cycles = 3uy; CycleMode = Standard; AddressMode = ZeroPage }
            { Action = instructionSTX; Name = "STX"; OpCode = 0x96uy; Cycles = 4uy; CycleMode = Standard; AddressMode = ZeroPageY }
            { Action = instructionSTX; Name = "STX"; OpCode = 0x8Euy; Cycles = 4uy; CycleMode = Standard; AddressMode = Absolute }
            { Action = instructionSTY; Name = "STY"; OpCode = 0x84uy; Cycles = 3uy; CycleMode = Standard; AddressMode = ZeroPage }
            { Action = instructionSTY; Name = "STY"; OpCode = 0x94uy; Cycles = 4uy; CycleMode = Standard; AddressMode = ZeroPageX }
            { Action = instructionSTY; Name = "STY"; OpCode = 0x8Cuy; Cycles = 4uy; CycleMode = Standard; AddressMode = Absolute }
            { Action = instructionTAX; Name = "TAX"; OpCode = 0xAAuy; Cycles = 2uy; CycleMode = Standard; AddressMode = Implied }
            { Action = instructionTAY; Name = "TAY"; OpCode = 0xA8uy; Cycles = 2uy; CycleMode = Standard; AddressMode = Implied }
            { Action = instructionTSX; Name = "TSX"; OpCode = 0xBAuy; Cycles = 2uy; CycleMode = Standard; AddressMode = Implied }
            { Action = instructionTXA; Name = "TXA"; OpCode = 0x8Auy; Cycles = 2uy; CycleMode = Standard; AddressMode = Implied }
            { Action = instructionTXS; Name = "TXS"; OpCode = 0x9Auy; Cycles = 2uy; CycleMode = Standard; AddressMode = Implied }
            { Action = instructionTYA; Name = "TYA"; OpCode = 0x98uy; Cycles = 2uy; CycleMode = Standard; AddressMode = Implied }
        |]

    let instructionsByOpCode =
        instructions
        |> Seq.map (fun x -> x.OpCode, x)
        |> dict

    let lda(action) =
        action()
        statusZero <- accumulator = 0uy
        statusNegative <- (accumulator &&& 0b00000010uy) > 0uy

    let newExecute() =
        let opCode = fetch()
        match instructionsByOpCode.TryGetValue opCode with
        | true, instruction ->
            let b =
                match instruction.AddressMode with
                | Accumulator -> addressModeAccumulator()
                | Implied     -> addressModeImplied()
                | Immediate   -> addressModeImmediate()
                | ZeroPage    -> addressModeZeroPage()
                | ZeroPageX   -> addressModeZeroPageX()
                | ZeroPageY   -> addressModeZeroPageY()
                | Relative    -> addressModeRelative()
                | Absolute    -> addressModeAbsolute()
                | AbsoluteX   -> addressModeAbsoluteX()
                | AbsoluteY   -> addressModeAbsoluteY()
                | Indirect    -> addressModeIndirect()
                | IndirectX   -> addressModeIndirectX()
                | IndirectY   -> addressModeIndirectY()

            instruction.Action b
        | _ -> failwithf "Unable to find OP Code: %d" opCode

    let execute() =
        let opCode = fetch()

        match opCode with
        | LDA.LDA_IMMEDIATE -> lda(fun _ -> accumulator <- fetch())
        | LDA.LDA_ZEROPAGE  -> lda(fun _ -> accumulator <- readZeroPage())
        | LDA.LDA_ZEROPAGEX -> failwithf "What does LDA_ZEROPAGEX mean?"
        | LDA.LDA_ABSOLUTE  -> lda(fun _ -> accumulator <- readAbsolute())
        | LDA.LDA_ABSOLUTEX -> lda(fun _ -> accumulator <- readAbsoluteOffset(uint16 xRegister))
        | LDA.LDA_ABSOLUTEY -> lda(fun _ -> accumulator <- readAbsoluteOffset(uint16 yRegister))
        | LDA.LDA_INDIRECTX -> lda(fun _ -> accumulator <- memory.Get(fetch() + xRegister))
        | LDA.LDA_INDIRECTY -> lda(fun _ -> accumulator <- memory.Get(fetch() + yRegister))

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