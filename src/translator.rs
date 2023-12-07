/*
 * Copyright (c) 2023 Steven Becker
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

//
// Translate a AST to the equivalent machine code after it
// has been parsed, linked and potentially optimized.
use std::fs::File;
use std::io::BufWriter;
use crate::parser::{LabelRecog, Instruction, Operation, Imm};

impl <'a> Operation<'a> {

}
/*
impl Instruction {
    fn imm_stype(imm:i32) -> u32{
       let upper =  imm & 0b11111_10000i32;
       let lower = imm & 0b1111i32;
       let mid = imm & 0b1_11110_00000_00000 <<3;
       upper << 25;
       lower << 7;
       (upper+ mid + lower) as u32
    }

    fn imm_btype(imm:i32) -> u32{
        let u12 =  imm & 0b10_00000_00000i32 >> 1;
        let l11 = imm & 0b1_00000_00000i32 >> 11;
        let upper = imm & 0b11111_00000i32 | u12;
        let lower = imm & 0b11110i32 | l11;
        upper << 25;
        lower << 7;
        (upper + lower) as u32
     }

     fn mask(imm: i32, shift: u8, mask: u8) -> u32 {
        let result = imm & (2_i32.pow((mask + 1) as u32) - 1);
        (result << shift) as u32
     }

    fn translate(self) -> u32{
        match self {
           Instruction::NA =>  todo!("ausfüllen"),

    // 1 Imm
    Instruction::Jmp(Imm) => todo!("impl"),
    Bt(Imm),
    Bf(Imm),

    VJmp(LabelStr),
    VBt(LabelStr),
    VBf(LabelStr),


    // Imm is the address!
    Instruction::Beq(Reg1, Reg2, Imm) => imm_btype(Imm) + (Reg1 as u8) << 20 + (Reg2 as u8) << 15 +  0b1100011, // Branch if equal 
    Instruction::Bne(Reg1, Reg2, Imm) => imm_btype(Imm) + (Reg1 as u8) << 20 + (Reg2 as u8) << 15 +  0b1100011 + 0b10_00000_00000, // Branch if not equal
    Instruction::Blt(Reg1, Reg2, Imm) => imm_btype(Imm) + (Reg1 as u8) << 20 + (Reg2 as u8) << 15 +  0b1100011 + 0b1000_00000_00000, // Branch if less than
    Instruction::Bltu(Reg1, Reg2, Imm) => imm_btype(Imm) + (Reg1 as u8) << 20 + (Reg2 as u8) << 15 +  0b1100011 + 0b1100_00000_00000,
    Instruction::Bge(Reg1, Reg2, Imm) => imm_btype(Imm) + (Reg1 as u8) << 20 + (Reg2 as u8) << 15 +  0b1100011 + 0b1010_00000_00000, // Branch if greater or equal
    Instruction::Bgeu(Reg1, Reg2, Imm)=> imm_btype(Imm) + (Reg1 as u8) << 20 + (Reg2 as u8) << 15 +  0b1100011 + 0b1110_00000_00000,

    // LabelStr is the address
    Instruction::VBeq(Reg, Reg, LabelStr) => -1,
    Instruction::VBne(Reg, Reg, LabelStr) => -1,
    Instruction::VBlt(Reg, Reg, LabelStr) => -1,
    Instruction::VBltu(Reg, Reg, LabelStr) => -1,
    Instruction::VBge(Reg, Reg, LabelStr) => -1,
    Instruction::VBgeu(Reg, Reg, LabelStr) => -1,

    // 1 Reg & 1 Imm
    // Need VLd and VSt as well for variables

    Instruction::Lb(Reg, Imm) => Imm <<15 + (Reg as u8) <<7 + 0b0000011, //Load byte
    Instruction::Lh(Reg, Imm) => Imm <<15 + (Reg as u8) <<7 + 0b0000011 + 0b10_00000_00000, //Load half
    Instruction::Lw(Reg, Imm) => Imm <<15 + (Reg as u8) <<7 + 0b0000011 + 0b100_00000_00000, //Load word
    Instruction::Sb(Reg, Imm) => imm_stype(Imm) + (Reg as u8) << 20 +0b0100011, //Store byte
    Instruction::Sh(Reg, Imm) => imm_stype(Imm) + (Reg as u8) << 20 +0b0100011 + 0b10_00000_00000, //Store half
    Instruction::Sw(Reg, Imm) => imm_stype(Imm) + (Reg as u8) << 20 +0b0100011 + 0b100_00000_00000, //Store word
    Instruction::Lui(Reg, Imm) => Imm <<12 + (Reg as u8) <<7 + 0b0110111,
    Instruction::Auipc(Reg, Imm) => Imm << 12 + (Reg as u8)<<7 + 0b0010111,

    Instruction::Lbu(Reg, Imm) => Imm <<15 + (Reg as u8) <<7 + 0b0000011 + 0b1000_00000_00000,
    Instruction::Lhu(Reg, Imm) => Imm <<15 + (Reg as u8) <<7 + 0b0000011 + 0b1010_00000_00000,

    Movu(Reg, Imm),
    Movl(Reg, Imm),

    // Shift left|right logical|arithmetic|rotate
    Instruction::Sll(Reg1, Reg2, Imm) => mask(Imm, 20, 4) + (Reg1 as u8) <<20 + (Reg2 as u8) <<15 + 0b0110011 + 0b10_00000_00000,
    Instruction::Srl(Reg1, Reg2, Imm) => mask(Imm, 20, 4) + (Reg1 as u8) <<20 + (Reg2 as u8) <<15 + 0b0110011 + 0b1010_00000_00000,
    Instruction::Sra(Reg1, Reg2, Imm) => mask(Imm, 20, 4) + (Reg1 as u8) <<20 + (Reg2 as u8) <<15 + 0b0110011 + 0b1010_00000_00000 + 0x20 << 25,
    Instruction::Sla(Reg, Reg, Imm) => -1,

    Srr(Reg, Imm),
    Slr(Reg, Imm),


    // 2 Regs
    Cmpe(Reg, Reg),
    Cmpg(Reg, Reg),
    Cmpl(Reg, Reg),

    Mov(Reg, Reg),

    // 2 Regs & 1 Imm
    Instruction::Addi(Reg1, Reg2, Imm) => Imm << 20 + (Reg2 as u8)<<15 + (Reg1 as u8) << 7 + 0b0010011,

    Addci(Reg, Reg, Imm),

    Subi(Reg, Reg, Imm),
    Subci(Reg, Reg, Imm),

    Muli(Reg, Reg, Imm),
    Mulci(Reg, Reg, Imm),

    Divi(Reg, Reg, Imm),
    Divci(Reg, Reg, Imm),

    Slti(Reg, Reg, Imm),
    Sltiu(Reg, Reg, Imm),

    Instruction::XorI(Reg, Reg, Imm) => Imm << 20 + (Reg2 as u8)<<15 + (Reg1 as u8) << 7 + 0b0010011 + 0b1000_00000_00000,
    Instruction::OrI(Reg, Reg, Imm) => Imm << 20 + (Reg2 as u8)<<15 + (Reg1 as u8) << 7 + 0b0010011 + 0b1100_00000_00000,
    Instruction::AndI(Reg, Reg, Imm) => Imm << 20 + (Reg2 as u8)<<15 + (Reg1 as u8) << 7 + 0b0010011 + 0b1110_00000_00000,

    // 3 Regs
    Addn(Reg, Reg, Reg),
    Addcn(Reg, Reg, Reg),

    Subn(Reg, Reg, Reg),
    Subcn(Reg, Reg, Reg),

    Muln(Reg, Reg, Reg),
    Mulcn(Reg, Reg, Reg),

    Divn(Reg, Reg, Reg),
    Divcn(Reg, Reg, Reg),

    Xor(Reg, Reg, Reg),
    Or(Reg, Reg, Reg),
    And(Reg, Reg, Reg),
    Xnor(Reg, Reg, Reg),
    Nor(Reg, Reg, Reg),

    Slt(Reg, Reg, Reg),
    Sltu(Reg, Reg, Reg),
        }

    }
}
*/