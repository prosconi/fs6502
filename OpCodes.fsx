module OpCodes

// ADC (ADd with Carry)
// Affects Flags: N V Z C

// MODE           SYNTAX       HEX LEN TIM
// Immediate     ADC #$44      $69  2   2
// Zero Page     ADC $44       $65  2   3
// Zero Page,X   ADC $44,X     $75  2   4
// Absolute      ADC $4400     $6D  3   4
// Absolute,X    ADC $4400,X   $7D  3   4+
// Absolute,Y    ADC $4400,Y   $79  3   4+
// Indirect,X    ADC ($44,X)   $61  2   6
// Indirect,Y    ADC ($44),Y   $71  2   5+

// + add 1 cycle if page boundary crossed

// ADC results are dependant on the setting of the decimal flag. In decimal mode, addition is carried out on the assumption that the values involved are packed BCD (Binary Coded Decimal).
// There is no way to add without carry.
module ADC =
    let [<Literal>] ADC_IMMEDIATE = 0x69uy
    let [<Literal>] ADC_ZEROPAGE  = 0x65uy
    let [<Literal>] ADC_ZEROPAGEX = 0x75uy
    let [<Literal>] ADC_ABSOLUTE  = 0x6Duy
    let [<Literal>] ADC_ABSOLUTEX = 0x7Duy
    let [<Literal>] ADC_ABSOLUTEY = 0x79uy
    let [<Literal>] ADC_INDIRECTX = 0x61uy
    let [<Literal>] ADC_INDIRECTY = 0x71uy

// LDA (LoaD Accumulator)
// Affects Flags: N Z

// MODE           SYNTAX       HEX LEN TIM
// Immediate     LDA #$44      $A9  2   2
// Zero Page     LDA $44       $A5  2   3
// Zero Page,X   LDA $44,X     $B5  2   4
// Absolute      LDA $4400     $AD  3   4
// Absolute,X    LDA $4400,X   $BD  3   4+
// Absolute,Y    LDA $4400,Y   $B9  3   4+
// Indirect,X    LDA ($44,X)   $A1  2   6
// Indirect,Y    LDA ($44),Y   $B1  2   5+

// + add 1 cycle if page boundary crossed
module LDA =
    let [<Literal>] LDA_IMMEDIATE = 0xA9uy
    let [<Literal>] LDA_ZEROPAGE  = 0xA5uy
    let [<Literal>] LDA_ZEROPAGEX = 0xB5uy
    let [<Literal>] LDA_ABSOLUTE  = 0xADuy
    let [<Literal>] LDA_ABSOLUTEX = 0xBDuy
    let [<Literal>] LDA_ABSOLUTEY = 0xB9uy
    let [<Literal>] LDA_INDIRECTX = 0xA1uy
    let [<Literal>] LDA_INDIRECTY = 0xB1uy

// LDX (LoaD X register)
// Affects Flags: N Z

// MODE           SYNTAX       HEX LEN TIM
// Immediate     LDX #$44      $A2  2   2
// Zero Page     LDX $44       $A6  2   3
// Zero Page,Y   LDX $44,Y     $B6  2   4
// Absolute      LDX $4400     $AE  3   4
// Absolute,Y    LDX $4400,Y   $BE  3   4+

// + add 1 cycle if page boundary crossed
module LDX =
    let [<Literal>] LDX_IMMEDIATE = 0xA2uy
    let [<Literal>] LDX_ZEROPAGE  = 0xA6uy
    let [<Literal>] LDX_ZEROPAGEY = 0xB6uy
    let [<Literal>] LDX_ABSOLUTE  = 0xAEuy
    let [<Literal>] LDX_ABSOLUTEY = 0xBEuy

// LDY (LoaD Y register)
// Affects Flags: N Z

// MODE           SYNTAX       HEX LEN TIM
// Immediate     LDY #$44      $A0  2   2
// Zero Page     LDY $44       $A4  2   3
// Zero Page,X   LDY $44,X     $B4  2   4
// Absolute      LDY $4400     $AC  3   4
// Absolute,X    LDY $4400,X   $BC  3   4+

// + add 1 cycle if page boundary crossed
module LDY =
    let [<Literal>] LDY_IMMEDIATE = 0xA0uy
    let [<Literal>] LDY_ZEROPAGE  = 0xA4uy
    let [<Literal>] LDY_ZEROPAGEX = 0xB4uy
    let [<Literal>] LDY_ABSOLUTE  = 0xACuy
    let [<Literal>] LDY_ABSOLUTEX = 0xBCuy

// STA (STore Accumulator)
// Affects Flags: none

// MODE           SYNTAX       HEX LEN TIM
// Zero Page     STA $44       $85  2   3
// Zero Page,X   STA $44,X     $95  2   4
// Absolute      STA $4400     $8D  3   4
// Absolute,X    STA $4400,X   $9D  3   5
// Absolute,Y    STA $4400,Y   $99  3   5
// Indirect,X    STA ($44,X)   $81  2   6
// Indirect,Y    STA ($44),Y   $91  2   6
module STA =
    let [<Literal>] STA_ZEROPAGE  = 0x85uy
    let [<Literal>] STA_ZEROPAGEX = 0x95uy
    let [<Literal>] STA_ABSOLUTE  = 0x8Duy
    let [<Literal>] STA_ABSOLUTEX = 0x9Duy
    let [<Literal>] STA_ABSOLUTEY = 0x99uy
    let [<Literal>] STA_INDIRECTX = 0x81uy
    let [<Literal>] STA_INDIRECTY = 0x91uy

// STX (STore X register)
// Affects Flags: none

// MODE           SYNTAX       HEX LEN TIM
// Zero Page     STX $44       $86  2   3
// Zero Page,Y   STX $44,Y     $96  2   4
// Absolute      STX $4400     $8E  3   4
module STX =
    let [<Literal>] STX_ZEROPAGE  = 0x86uy
    let [<Literal>] STX_ZEROPAGEY = 0x96uy
    let [<Literal>] STX_ABSOLUTE  = 0x8Euy

// STY (STore Y register)
// Affects Flags: none

// MODE           SYNTAX       HEX LEN TIM
// Zero Page     STY $44       $84  2   3
// Zero Page,X   STY $44,X     $94  2   4
// Absolute      STY $4400     $8C  3   4
module STY =
    let [<Literal>] STY_ZEROPAGE  = 0x84uy
    let [<Literal>] STY_ZEROPAGEX = 0x94uy
    let [<Literal>] STY_ABSOLUTE  = 0x8Cuy

// Register Instructions
// Affect Flags: N Z

// These instructions are implied mode, have a length of one byte and require two machine cycles.
module RegisterInstructions =
    let [<Literal>] TAX = 0xAAuy //(Transfer A to X)    $AA
    let [<Literal>] TXA = 0x8Auy //(Transfer X to A)    $8A
    let [<Literal>] DEX = 0xCAuy //(DEcrement X)        $CA
    let [<Literal>] INX = 0xE8uy //(INcrement X)        $E8
    let [<Literal>] TAY = 0xA8uy //(Transfer A to Y)    $A8
    let [<Literal>] TYA = 0x98uy //(Transfer Y to A)    $98
    let [<Literal>] DEY = 0x88uy //(DEcrement Y)        $88
    let [<Literal>] INY = 0xC8uy //(INcrement Y)        $C8

// AND (bitwise AND with accumulator)
// Affects Flags: N Z

// MODE           SYNTAX       HEX LEN TIM
// Immediate     AND #$44      $29  2   2
// Zero Page     AND $44       $25  2   3
// Zero Page,X   AND $44,X     $35  2   4
// Absolute      AND $4400     $2D  3   4
// Absolute,X    AND $4400,X   $3D  3   4+
// Absolute,Y    AND $4400,Y   $39  3   4+
// Indirect,X    AND ($44,X)   $21  2   6
// Indirect,Y    AND ($44),Y   $31  2   5+

// + add 1 cycle if page boundary crossed
module AND =
    let [<Literal>] AND_IMMEDIATE = 0x29uy
    let [<Literal>] AND_ZEROPAGE  = 0x25uy
    let [<Literal>] AND_ZEROPAGEX = 0x35uy
    let [<Literal>] AND_ABSOLUTE  = 0x2Duy
    let [<Literal>] AND_ABSOLUTEX = 0x3Duy
    let [<Literal>] AND_ABSOLUTEY = 0x39uy
    let [<Literal>] AND_INDIRECTX = 0x21uy
    let [<Literal>] AND_INDIRECTY = 0x31uy