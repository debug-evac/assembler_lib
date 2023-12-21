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

impl Instruction {
    fn imm_stype(imm:i32) -> u32{
       let upper =  imm & 0b11111_10000i32;
       let lower = imm & 0b1111i32;
       let mid = imm & 0b1_11110_00000_00000 <<3;
       upper << 25;
       lower << 7;
       (upper + mid + lower) as u32
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

     /*
     fn sign_extend(imm:u32) -> u32{
         let mask_length = 31 - imm.len();
         if imm.start() == 0 {
            let mask = 0b00_00000_00000_00000_00000_00000_00000;
            let result = (imm + mask) as u32;
         }
         else {
            let mask = 0b11_11111_11111_11111_11111_11111_11111;
            for imm.length() > 0{
               let mask = mask & (0 << (imm.length - 1));
            }
            let result = (imm + mask) as u32;
         }
         result
     }
     */

    fn translate_instruction(self) -> u32{
        match self {
           Instruction::NA => todo!("impl"),
          // Instruction::Noop => todo!("impl"),

    // 1 imm
    Instruction::Jmp(imm) => todo!("impl"),
    Instruction::Bt(imm) => todo!("impl"),
    Instruction::Bf(imm)=> todo!("impl"),

    Instruction::VJmp(_) => 0,
    Instruction::VBt(_) => 0,
    Instruction::VBf(_) => 0,


    // imm is the address!
    Instruction::Beq(reg1, reg2, imm) => Instruction::imm_btype(imm) + (reg2 as u32) << 20 + (reg1 as u8) << 15 +  0b1100011_u32, // Branch if equal 
    Instruction::Bne(reg1, reg2, imm) => Instruction::imm_btype(imm) + (reg2 as u32) << 20 + (reg1 as u8) << 15 +  0b1100011_u32 + 0b10_00000_00000_u32, // Branch if not equal
    Instruction::Blt(reg1, reg2, imm) => Instruction::imm_btype(imm) + (reg2 as u32) << 20 + (reg1 as u8) << 15 +  0b1100011_u32 + 0b1000_00000_00000_u32, // Branch if less than
    Instruction::Bltu(reg1, reg2, imm) => Instruction::imm_btype(imm) + (reg2 as u32) << 20 + (reg1 as u8) << 15 +  0b1100011_u32 + 0b1100_00000_00000_u32,
    Instruction::Bge(reg1, reg2, imm) => Instruction::imm_btype(imm) + (reg2 as u32) << 20 + (reg1 as u8) << 15 +  0b1100011_u32 + 0b1010_00000_00000_u32, // Branch if greater or equal
    Instruction::Bgeu(reg1, reg2, imm)=> Instruction::imm_btype(imm) + (reg2 as u32) << 20 + (reg1 as u8) << 15 +  0b1100011_u32 + 0b1110_00000_00000_u32,

    // LabelStr is the address
    Instruction::VBeq(_, _, _) => 0,
    Instruction::VBne(_, _, _) => 0,
    Instruction::VBlt(_, _, _) => 0,
    Instruction::VBltu(_, _, _) => 0,
    Instruction::VBge(_, _, _) => 0,
    Instruction::VBgeu(_, _, _) => 0,

    // 1 reg & 1 imm
    // Need VLd and VSt as well for variables

    Instruction::Lb(reg, imm) => (imm as u32) << 15 + (reg as u32) << 7 + 0b0000011_u32, //Load byte
    Instruction::Lh(reg, imm) => (imm as u32) << 15 + (reg as u32) << 7 + 0b0000011_u32 + 0b10_00000_00000_u32, //Load half
    Instruction::Lw(reg, imm) => (imm as u32) << 15 + (reg as u32) << 7 + 0b0000011_u32 + 0b100_00000_00000_u32, //Load word
    Instruction::Sb(reg, imm) => Instruction::imm_stype(imm) + (reg as u32) << 20 + 0b0100011_u32, //Store byte
    Instruction::Sh(reg, imm) => Instruction::imm_stype(imm) + (reg as u32) << 20 + 0b0100011_u32 + 0b10_00000_00000_u32, //Store half
    Instruction::Sw(reg, imm) => Instruction::imm_stype(imm) + (reg as u32) << 20 + 0b0100011_u32 + 0b100_00000_00000_u32, //Store word
    Instruction::Lui(reg, imm) => (imm as u32) << 12 + (reg as u8) << 7 + 0b0110111_u32,
    Instruction::Auipc(reg, imm) => (imm as u32) << 12 + (reg as u8) << 7 + 0b0010111_u32,

    Instruction::Lbu(reg, imm) => (imm as u32) << 15 + (reg as u32) << 7 + 0b0000011_u32 + 0b1000_00000_00000_u32,
    Instruction::Lhu(reg, imm) => (imm as u32) << 15 + (reg as u32) << 7 + 0b0000011_u32 + 0b1010_00000_00000_u32,

    Instruction::Movu(reg, imm) => todo!("impl"),
    Instruction::Movl(reg, imm) => todo!("impl"),

    // Shift left|right logical|arithmetic|rotate
    Instruction::Slli(reg1, reg2, imm) => Instruction::mask(imm, 20, 4) + (reg2 as u32) << 20 + (reg1 as u32) << 15 + 0b0110011_u32 + 0b10_00000_00000,
    Instruction::Srli(reg1, reg2, imm) => Instruction::mask(imm, 20, 4) + (reg2 as u32) << 20 + (reg1 as u32) << 15 + 0b0110011_u32 + 0b1010_00000_00000,
    Instruction::Srai(reg1, reg2, imm) => Instruction::mask(imm, 20, 4) + (reg2 as u32) << 20 + (reg1 as u32) << 15 + 0b0110011_u32 + 0b1010_00000_00000 + 0x20 << 25,
    Instruction::Slai(_, _, _) => 0,

    Instruction::Srr(reg1, reg2, imm) => todo!("impl"),
    Instruction::Slr(reg1, reg2, imm) => todo!("impl"),


    // 2 regs
    Instruction::Cmpe(reg2, reg1) => todo!("impl"),
    Instruction::Cmpg(reg3, reg1, reg2) => (reg2 as u32) << 20 + (reg1 as u32) << 15 + 0b100_00000_00000_u32 + (reg3 as u32) << 7 + 0b0110011_u32,
    Instruction::Cmpl(reg3, reg1, reg2) => (reg1 as u32) << 20 + (reg2 as u32) << 15 + 0b100_00000_00000_u32 + (reg3 as u32) << 7 + 0b0110011_u32,

    Instruction::Mov(reg2, reg1) => todo!("impl"),

    // 2 regs & 1 imm
    Instruction::Addi(reg1, reg2, imm) => (imm as u32) << 20 + (reg2 as u32) << 15 + (reg1 as u32) << 7 + 0b0010011_u32,

    Instruction::Addci(_, _, _) => 0,

    Instruction::Subi(_, _, _) => 0,
    Instruction::Subci(_, _, _) => 0,

    Instruction::Muli(_, _, _) => 0,
    Instruction::Mulci(_, _, _) => 0,

    Instruction::Divi(_, _, _) => 0,
    Instruction::Divci(_, _, _) => 0,

    Instruction::Slti(reg1, reg2, imm) => (imm as u32) << 20 + (reg2 as u32) << 15 + (reg1 as u32) << 7 + 0b0010011_u32 + 0b110_00000_00000_u32,
    Instruction::Sltiu(reg1, reg2, imm) => (imm as u32) << 20 + (reg2 as u32) << 15 + (reg1 as u32) << 7 + 0b0010011_u32 + 0b110_00000_00000_u32, 

    Instruction::XorI(reg1, reg2, imm) => (imm as u32) << 20 + (reg2 as u32) << 15 + (reg1 as u32) << 7 + 0b0010011_u32 + 0b1000_00000_00000_u32,
    Instruction::OrI(reg1, reg2, imm) => (imm as u32) << 20 + (reg2 as u32) << 15 + (reg1 as u32) << 7 + 0b0010011_u32 + 0b1100_00000_00000_u32,
    Instruction::AndI(reg1, reg2, imm) => (imm as u32) << 20 + (reg2 as u32) << 15 + (reg1 as u32) << 7 + 0b0010011_u32 + 0b1110_00000_00000_u32,

    // 3 regs
    Instruction::Addn(reg3, reg1, reg2) => (reg2 as u32) << 20 + (reg1 as u32) << 15 + (reg3 as u32) << 7 + 0b0110011_u32,
    Instruction::Addcn(_, _, _) => 0,

    Instruction::Subn(reg3, reg1, reg2) => 0b100000_u32 << 25 + (reg2 as u32) << 20 + (reg1 as u32) << 15 + (reg3 as u32) << 7 + 0b0110011_u32,
    Instruction::Subcn(_, _, _) => 0,

    Instruction::Muln(_, _, _) => 0,
    Instruction::Mulcn(_, _, _) => 0,

    Instruction::Divn(_, _, _) => 0,
    Instruction::Divcn(_, _, _) => 0,

    Instruction::Xor(reg3, reg1, reg2) => (reg2 as u32) << 20 + (reg1 as u32) << 15 + 0b1000_00000_00000_u32 + (reg3 as u32) << 7 + 0b0110011_u32,
    Instruction::Or(reg3, reg1, reg2) => (reg2 as u32) << 20 + (reg1 as u32) << 15 + 0b1100_00000_00000_u32 + (reg3 as u32) << 7 + 0b0110011_u32,
    Instruction::And(reg3, reg1, reg2) => (reg2 as u32) << 20 + (reg1 as u32) << 15 + 0b1110_00000_00000_u32 + (reg3 as u32) << 7 + 0b0110011_u32,
    Instruction::Xnor(_, _, _) => 0,
    Instruction::Nor(_, _, _) => 0,

    Instruction::Slt(reg3, reg1, reg2) => (reg2 as u32) << 20 + (reg1 as u32) << 15 + 0b100_00000_00000_u32 + (reg3 as u32) << 7 + 0b0110011_u32,
    Instruction::Sltu(reg3, reg1, reg2) => (reg2 as u32) << 20 + (reg1 as u32) << 15 + 0b110_00000_00000_u32 + (reg3 as u32) << 7 + 0b0110011_u32,
    
    Instruction::Sll(reg3, reg1, reg2) => (reg2 as u32) << 20 + (reg1 as u32) << 15 + 0b0110011_u32 + (reg3 as u32) << 7 + 0b10_00000_00000_u32,
    Instruction::Srl(reg3, reg1, reg2) => (reg2 as u32) << 20 + (reg1 as u32) << 15 + 0b0110011_u32 + (reg3 as u32) << 7 + 0b1010_00000_00000_u32,
    Instruction::Sra(reg3, reg1, reg2) => 0b100000_u32 << 25 + (reg1 as u32) << 20 + (reg2 as u32) << 15 + 0b0110011_u32 + (reg3 as u32) << 7 + 0b1010_00000_00000_u32,
    Instruction::Sla(_, _, _) => 0,

    
}
}
}


pub fn translate(input: Vec<Instruction>) -> Vec<u32> {
    let mut output: Vec<u32> = vec![];
    for instr in input.iter(){

        output.push(Instruction::translate_instruction(instr.clone()));

    }
    
    output
}

