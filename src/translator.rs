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
use regex::Regex;
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

     fn sign_extend(imm:u32) -> u32{
         mask_length = 31 - imm.length();
         if imm.start() == 0 {
            mask = 0b00_00000_00000_00000_00000_00000_00000;
            let result = (imm + mask) as u32
         }
         else {
            mask = 0b11_11111_11111_11111_11111_11111_11111;
            for imm.length() > 0{
               mask = mask & (0 << (imm.length - 1));
            }
            let result = (imm + mask) as u32
         }
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
    Instruction::Beq(Reg1, Reg2, Imm) => imm_btype(Imm) + (Reg2 as u8) << 20 + (Reg1 as u8) << 15 +  0b1100011, // Branch if equal 
    Instruction::Bne(Reg1, Reg2, Imm) => imm_btype(Imm) + (Reg2 as u8) << 20 + (Reg1 as u8) << 15 +  0b1100011 + 0b10_00000_00000, // Branch if not equal
    Instruction::Blt(Reg1, Reg2, Imm) => imm_btype(Imm) + (Reg2 as u8) << 20 + (Reg1 as u8) << 15 +  0b1100011 + 0b1000_00000_00000, // Branch if less than
    Instruction::Bltu(Reg1, Reg2, Imm) => imm_btype(Imm) + (Reg2 as u8) << 20 + (Reg1 as u8) << 15 +  0b1100011 + 0b1100_00000_00000,
    Instruction::Bge(Reg1, Reg2, Imm) => imm_btype(Imm) + (Reg2 as u8) << 20 + (Reg1 as u8) << 15 +  0b1100011 + 0b1010_00000_00000, // Branch if greater or equal
    Instruction::Bgeu(Reg1, Reg2, Imm)=> imm_btype(Imm) + (Reg2 as u8) << 20 + (Reg1 as u8) << 15 +  0b1100011 + 0b1110_00000_00000,

    // LabelStr is the address
    Instruction::VBeq(Reg1, Reg2, LabelStr) => -1,
    Instruction::VBne(Reg1, Reg2, LabelStr) => -1,
    Instruction::VBlt(Reg1, Reg2, LabelStr) => -1,
    Instruction::VBltu(Reg1, Reg2, LabelStr) => -1,
    Instruction::VBge(Reg1, Reg2, LabelStr) => -1,
    Instruction::VBgeu(Reg1, Reg2, LabelStr) => -1,

    // 1 Reg & 1 Imm
    // Need VLd and VSt as well for variables

    Instruction::Lb(Reg, Imm) => Imm << 15 + (Reg as u8) << 7 + 0b0000011, //Load byte
    Instruction::Lh(Reg, Imm) => Imm << 15 + (Reg as u8) << 7 + 0b0000011 + 0b10_00000_00000, //Load half
    Instruction::Lw(Reg, Imm) => Imm << 15 + (Reg as u8) << 7 + 0b0000011 + 0b100_00000_00000, //Load word
    Instruction::Sb(Reg, Imm) => imm_stype(Imm) + (Reg as u8) << 20 + 0b0100011, //Store byte
    Instruction::Sh(Reg, Imm) => imm_stype(Imm) + (Reg as u8) << 20 + 0b0100011 + 0b10_00000_00000, //Store half
    Instruction::Sw(Reg, Imm) => imm_stype(Imm) + (Reg as u8) << 20 + 0b0100011 + 0b100_00000_00000, //Store word
    Instruction::Lui(Reg, Imm) => Imm << 12 + (Reg as u8) << 7 + 0b0110111,
    Instruction::Auipc(Reg, Imm) => Imm << 12 + (Reg as u8) << 7 + 0b0010111,

    Instruction::Lbu(Reg, Imm) => Imm << 15 + (Reg as u8) << 7 + 0b0000011 + 0b1000_00000_00000,
    Instruction::Lhu(Reg, Imm) => Imm << 15 + (Reg as u8) << 7 + 0b0000011 + 0b1010_00000_00000,

    Movu(Reg, Imm),
    Movl(Reg, Imm),

    // Shift left|right logical|arithmetic|rotate
    Instruction::Slli(Reg1, Reg2, Imm) => mask(Imm, 20, 4) + (Reg2 as u8) << 20 + (Reg1 as u8) << 15 + 0b0110011 + 0b10_00000_00000,
    Instruction::Srli(Reg1, Reg2, Imm) => mask(Imm, 20, 4) + (Reg2 as u8) << 20 + (Reg1 as u8) << 15 + 0b0110011 + 0b1010_00000_00000,
    Instruction::Srai(Reg1, Reg2, Imm) => mask(Imm, 20, 4) + (Reg2 as u8) << 20 + (Reg1 as u8) << 15 + 0b0110011 + 0b1010_00000_00000 + 0x20 << 25,
    Instruction::Slai(Reg, Reg, Imm) => -1,

    Srr(Reg, Imm),
    Slr(Reg, Imm),


    // 2 Regs
    Cmpe(Reg, Reg),
    Instruction::Cmpg(Reg1, Reg2) => (Reg2 as u8) << 20 + (Reg1 as u8) << 15 + 0b100_00000_00000 + (Reg3 as u8) << 7 + 0b0110011,
    Instruction::Cmpl(Reg1, Reg2) => (Reg1 as u8) << 20 + (Reg2 as u8) << 15 + 0b100_00000_00000 + (Reg3 as u8) << 7 + 0b0110011,

    Mov(Reg, Reg),

    // 2 Regs & 1 Imm
    Instruction::Addi(Reg1, Reg2, Imm) => Imm << 20 + (Reg2 as u8) << 15 + (Reg1 as u8) << 7 + 0b0010011,

    Addci(Reg, Reg, Imm),

    Subi(Reg, Reg, Imm),
    Subci(Reg, Reg, Imm),

    Muli(Reg, Reg, Imm),
    Mulci(Reg, Reg, Imm),

    Divi(Reg, Reg, Imm),
    Divci(Reg, Reg, Imm),

    Instruction::Slti(Reg1, Reg2, Imm) => Imm << 20 + (Reg2 as u8) << 15 + (Reg1 as u8) << 7 + 0b0010011 + 0b110_00000_00000,
    Instruction::Sltiu(Reg1, Reg2, Imm) => Imm << 20 + (Reg2 as u8) << 15 + (Reg1 as u8) << 7 + 0b0010011 + 0b110_00000_00000, todo!("Zero extends")

    Instruction::XorI(Reg1, Reg2, Imm) => Imm << 20 + (Reg2 as u8) << 15 + (Reg1 as u8) << 7 + 0b0010011 + 0b1000_00000_00000,
    Instruction::OrI(Reg1, Reg2, Imm) => Imm << 20 + (Reg2 as u8) << 15 + (Reg1 as u8) << 7 + 0b0010011 + 0b1100_00000_00000,
    Instruction::AndI(Reg1, Reg2, Imm) => Imm << 20 + (Reg2 as u8) << 15 + (Reg1 as u8) << 7 + 0b0010011 + 0b1110_00000_00000,

    // 3 Regs
    Instruction::Addn(Reg3, Reg1, Reg2) => (Reg2 as u8) << 20 + (Reg1 as u8) << 15 + (Reg3 as u8) << 7 + 0b0110011,
    Addcn(Reg, Reg, Reg),

    Instruction::Subn(Reg3, Reg1, Reg2) => 0b100000 << 25 + (Reg2 as u8) << 20 + (Reg1 as u8) << 15 + (Reg3 as u8) << 7 + 0b0110011,
    Subcn(Reg, Reg, Reg),

    Muln(Reg, Reg, Reg),
    Mulcn(Reg, Reg, Reg),

    Divn(Reg, Reg, Reg),
    Divcn(Reg, Reg, Reg),

    Instruction::Xor(Reg3, Reg1, Reg2) => (Reg2 as u8) << 20 + (Reg1 as u8) << 15 + 0b1000_00000_00000 + (Reg3 as u8) << 7 + 0b0110011,
    Instruction::Or(Reg3, Reg1, Reg2) => (Reg2 as u8) << 20 + (Reg1 as u8) << 15 + 0b1100_00000_00000 + (Reg3 as u8) << 7 + 0b0110011,
    Instruction::And(Reg3, Reg1, Reg2) => (Reg2 as u8) << 20 + (Reg1 as u8) << 15 + 0b1110_00000_00000 + (Reg3 as u8) << 7 + 0b0110011,
    Instruction::Xnor(Reg3, Reg1, Reg2) => -1,
    Instruction::Nor(Reg3, Reg1, Reg2) => -1,

    Instruction::Slt(Reg3, Reg1, Reg2) => (Reg2 as u8) << 20 + (Reg1 as u8) << 15 + 0b100_00000_00000 + (Reg3 as u8) << 7 + 0b0110011,
    Instruction::Sltu(Reg3, Reg1, Reg2) => (Reg2 as u8) << 20 + (Reg1 as u8) << 15 + 0b110_00000_00000 + (Reg3 as u8) << 7 + 0b0110011,
    
    Instruction::Sll(Reg3, Reg1, Reg2) => (Reg2 as u8) << 20 + (Reg1 as u8) << 15 + 0b0110011 + 0b10_00000_00000,
    Instruction::Srl(Reg3, Reg1, Reg2) => (Reg2 as u8) << 20 + (Reg1 as u8) << 15 + 0b0110011 + 0b1010_00000_00000,
    Instruction::Sra(Reg3, Reg1, Reg2) => 0b100000 << 25 + (Reg1 as u8) << 20 + (Reg2 as u8) << 15 + 0b0110011 + 0b1010_00000_00000,
    Instruction::Sla(Reg3, Reg1, Reg2) => -1,
}
*/