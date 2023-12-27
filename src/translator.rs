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
use crate::parser::{LabelRecog, Instruction, Operation, Reg, Imm};

fn btype_instr(rs1: Reg, rs2: Reg, imm: Imm) -> u32 {
   let u12 =  imm & 0b10_00000_00000i32 >> 1;
   let l11 = imm & 0b1_00000_00000i32 >> 11;
   let upper = (imm & 0b11111_00000i32 | u12) << 25;
   let lower = (imm & 0b11110i32 | l11) << 7;
   ((upper + lower) as u32) + ((rs2 as u32) << 20) + ((rs1 as u32) << 15) + 0b1100011_u32
}

fn stype_instr(rs1: Reg, rs2: Reg, imm: Imm) -> u32 {
   let upper =  (imm & 0b11111_10000i32) << 25;
   let lower = (imm & 0b1111i32) << 7;
   let mid = imm & 0b1_11110_00000_00000 << 3;
   ((upper + mid + lower) as u32) + ((rs2 as u32) << 20) + ((rs1 as u32) << 15) + 0b100011_u32
}

fn itype_instr(rd: Reg, rs1: Reg, imm: Imm) -> u32 {
   ((imm as u32) << 20) + ((rs1 as u32) << 15) + ((rd as u32) << 7)
}

fn rtype_instr(rd: Reg, rs1: Reg, rs2: Reg) -> u32 {
   ((rs2 as u32) << 20) + ((rs1 as u32) << 15) + ((rd as u32) << 7) + 0b0110011_u32
}

fn utype_instr(rd: Reg, imm: Imm) -> u32 {
   ((imm as u32) << 12) + ((rd as u32) << 7)
}

impl Instruction {
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


    // B-Type instruction ; imm is the address!
    Instruction::Beq(reg1, reg2, imm) => btype_instr(reg1, reg2, imm), // Branch if equal 
    Instruction::Bne(reg1, reg2, imm) => btype_instr(reg1, reg2, imm) + 0b10_00000_00000_u32, // Branch if not equal
    Instruction::Blt(reg1, reg2, imm) => btype_instr(reg1, reg2, imm) + 0b1000_00000_00000_u32, // Branch if less than
    Instruction::Bltu(reg1, reg2, imm) => btype_instr(reg1, reg2, imm) + 0b1100_00000_00000_u32,
    Instruction::Bge(reg1, reg2, imm) => btype_instr(reg1, reg2, imm) + 0b1010_00000_00000_u32, // Branch if greater or equal
    Instruction::Bgeu(reg1, reg2, imm)=> btype_instr(reg1, reg2, imm) + 0b1110_00000_00000_u32,

    // LabelStr is the address
    Instruction::VBeq(_, _, _) => 0,
    Instruction::VBne(_, _, _) => 0,
    Instruction::VBlt(_, _, _) => 0,
    Instruction::VBltu(_, _, _) => 0,
    Instruction::VBge(_, _, _) => 0,
    Instruction::VBgeu(_, _, _) => 0,

    // 1 reg & 1 imm
    // Need VLd and VSt as well for variables
    // U-Type instruction
    Instruction::Lui(reg, imm) => utype_instr(reg, imm) + 0b0110111_u32,
    Instruction::Auipc(reg, imm) => utype_instr(reg, imm) + 0b0010111_u32,


    Instruction::Movu(reg, imm) => todo!("impl"),
    Instruction::Movl(reg, imm) => todo!("impl"),
    
    // Shift left|right logical|arithmetic|rotate
    /*Instruction::Slli(reg1, reg2, imm) => Instruction::mask(imm, 20, 4) + (reg2 as u32) << 20 + (reg1 as u32) << 15 + 0b0110011_u32 + 0b10_00000_00000,
    Instruction::Srli(reg1, reg2, imm) => Instruction::mask(imm, 20, 4) + (reg2 as u32) << 20 + (reg1 as u32) << 15 + 0b0110011_u32 + 0b1010_00000_00000,
    Instruction::Srai(reg1, reg2, imm) => Instruction::mask(imm, 20, 4) + (reg2 as u32) << 20 + (reg1 as u32) << 15 + 0b0110011_u32 + 0b1010_00000_00000 + 0x20 << 25,
    */
    Instruction::Slai(_, _, _) => 0,
    
    Instruction::Srr(reg1, reg2, imm) => todo!("impl"),
    Instruction::Slr(reg1, reg2, imm) => todo!("impl"),
    
    
    // 2 regs
    Instruction::Cmpe(reg2, reg1) => todo!("impl"),
    //Instruction::Cmpg(reg3, reg1, reg2) => (reg2 as u32) << 20 + (reg1 as u32) << 15 + 0b100_00000_00000_u32 + (reg3 as u32) << 7 + 0b0110011_u32,
    //Instruction::Cmpl(reg3, reg1, reg2) => (reg1 as u32) << 20 + (reg2 as u32) << 15 + 0b100_00000_00000_u32 + (reg3 as u32) << 7 + 0b0110011_u32,
    
    Instruction::Mov(reg2, reg1) => todo!("impl"),
    
    // 2 regs & 1 imm
    
    Instruction::Addci(_, _, _) => 0,
    
    Instruction::Subi(_, _, _) => 0,
    Instruction::Subci(_, _, _) => 0,
    
    Instruction::Muli(_, _, _) => 0,
    Instruction::Mulci(_, _, _) => 0,
    
    Instruction::Divi(_, _, _) => 0,
    Instruction::Divci(_, _, _) => 0,
    
    //Instruction::Slti(reg1, reg2, imm) => (imm as u32) << 20 + (reg2 as u32) << 15 + (reg1 as u32) << 7 + 0b0010011_u32 + 0b110_00000_00000_u32,
    //Instruction::Sltiu(reg1, reg2, imm) => (imm as u32) << 20 + (reg2 as u32) << 15 + (reg1 as u32) << 7 + 0b0010011_u32 + 0b110_00000_00000_u32, 
    
    // I-Type instruction; reg1 == rd
    Instruction::Addi(reg1, reg2, imm) => itype_instr(reg1, reg2, imm) + 0b0010011_u32,
    
    Instruction::XorI(reg1, reg2, imm) => itype_instr(reg1, reg2, imm) + 0b0010011_u32 + 0b1000_00000_00000_u32,
    Instruction::OrI(reg1, reg2, imm) => itype_instr(reg1, reg2, imm) + 0b0010011_u32 + 0b1100_00000_00000_u32,
    Instruction::AndI(reg1, reg2, imm) => itype_instr(reg1, reg2, imm) + 0b0010011_u32 + 0b1110_00000_00000_u32,

    Instruction::Lb(reg1, reg2, imm) => itype_instr(reg1, reg2, imm) + 0b0000011_u32, //Load byte
    Instruction::Lh(reg1, reg2, imm) => itype_instr(reg1, reg2, imm) + 0b0000011_u32 + 0b10_00000_00000_u32, //Load half
    Instruction::Lw(reg1, reg2, imm) => itype_instr(reg1, reg2, imm) + 0b0000011_u32 + 0b100_00000_00000_u32, //Load word
    
    Instruction::Lbu(reg1, reg2, imm) => itype_instr(reg1, reg2, imm) + 0b0000011_u32 + 0b1000_00000_00000_u32, // ??
    Instruction::Lhu(reg1, reg2, imm) => itype_instr(reg1, reg2, imm) + 0b0000011_u32 + 0b1010_00000_00000_u32, // ??

    // S-Type instruction
    Instruction::Sb(reg1, reg2, imm) => stype_instr(reg1, reg2, imm), //Store byte
    Instruction::Sh(reg1, reg2, imm) => stype_instr(reg1, reg2, imm) + 0b10_00000_00000_u32, //Store half
    Instruction::Sw(reg1, reg2, imm) => stype_instr(reg1, reg2, imm) + 0b100_00000_00000_u32, //Store word

    // R-Type instruction; 3 regs
    Instruction::Addn(reg3, reg1, reg2) => rtype_instr(reg3, reg1, reg2),
    Instruction::Subn(reg3, reg1, reg2) => rtype_instr(reg3, reg1, reg2) + (0b100000_u32 << 25),
    
    Instruction::Xor(reg3, reg1, reg2) => rtype_instr(reg3, reg1, reg2) + 0b1000_00000_00000_u32,
    Instruction::Or(reg3, reg1, reg2) => rtype_instr(reg3, reg1, reg2) + 0b1100_00000_00000_u32,
    Instruction::And(reg3, reg1, reg2) => rtype_instr(reg3, reg1, reg2) + 0b1110_00000_00000_u32,
    
    Instruction::Slt(reg3, reg1, reg2) => rtype_instr(reg3, reg1, reg2) + 0b100_00000_00000_u32,
    Instruction::Sltu(reg3, reg1, reg2) => rtype_instr(reg3, reg1, reg2) + 0b110_00000_00000_u32,

    Instruction::Muln(_, _, _) => 0,
    
    Instruction::Addcn(_, _, _) => 0,
    Instruction::Subcn(_, _, _) => 0,
    Instruction::Mulcn(_, _, _) => 0,
    Instruction::Divn(_, _, _) => 0,
    Instruction::Divcn(_, _, _) => 0,

    Instruction::Xnor(_, _, _) => 0,
    Instruction::Nor(_, _, _) => 0,
    
    /*Instruction::Sll(reg3, reg1, reg2) => (reg2 as u32) << 20 + (reg1 as u32) << 15 + 0b0110011_u32 + (reg3 as u32) << 7 + 0b10_00000_00000_u32,
    Instruction::Srl(reg3, reg1, reg2) => (reg2 as u32) << 20 + (reg1 as u32) << 15 + 0b0110011_u32 + (reg3 as u32) << 7 + 0b1010_00000_00000_u32,
    Instruction::Sra(reg3, reg1, reg2) => 0b100000_u32 << 25 + (reg1 as u32) << 20 + (reg2 as u32) << 15 + 0b0110011_u32 + (reg3 as u32) << 7 + 0b1010_00000_00000_u32,
    */
    Instruction::Sla(_, _, _) => 0,    

    _ => todo!("Not implemented!"),
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
