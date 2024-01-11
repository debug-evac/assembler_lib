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
use crate::common::{Instruction, Reg, Imm};

fn btype_instr(rs1: Reg, rs2: Reg, imm: Imm) -> u32 {
   let u12 =  (imm & 0b100_00000_00000i32) >> 1;
   let l11 = (imm & 0b10_00000_00000i32) >> 11;
   let upper = ((imm & 0b1_11110_00000i32) | u12) << 25;
   let lower = ((imm & 0b11110i32) | l11) << 7;
   ((upper + lower) as u32) + ((rs2 as u32) << 21) + ((rs1 as u32) << 16) + 0b1100011_u32
}

fn stype_instr(rs1: Reg, rs2: Reg, imm: Imm) -> u32 {
   let upper =  (imm & 0b11111_10000i32) << 25;
   let lower = (imm & 0b1111i32) << 7;
   let mid = (imm & 0b1_11110_00000_00000) << 3;
   ((upper + mid + lower) as u32) + ((rs2 as u32) << 20) + ((rs1 as u32) << 15) + 0b100011_u32
}

fn itype_instr(rd: Reg, rs1: Reg, imm: Imm) -> u32 {
   ((imm as u32) << 17) + ((rs1 as u32) << 12) + ((rd as u32) << 7)
}

fn rtype_instr(rd: Reg, rs1: Reg, rs2: Reg) -> u32 {
   ((rs2 as u32) << 17) + ((rs1 as u32) << 15) + ((rd as u32) << 7) + 0b0110011_u32
}

fn utype_instr(rd: Reg, imm: Imm) -> u32 {
   (((imm as u32) >> 12) << 12) + ((rd as u32) << 7)
}

fn jtype_instr(rd: Reg, imm: Imm) -> u32 {
   let ten_to_one = (imm & 0b01_11111_11110_i32) << 8;
   let nineteen_to_twelve = (imm & 0b0_11111_11100_00000_00000_i32) >> 12;
   let eleven = (imm & 0b010_00000_00000_i32) >> 3;
   let twenty = imm & 0b01_00000_00000_00000_00000_i32 >> 1;

   (((ten_to_one | nineteen_to_twelve | eleven | twenty) as u32) << 12) + ((rd as u32) << 7) + 0b1101111_u32
}

impl Instruction {
   fn mask(imm: i32, mask: u8) -> i32 {
      imm & 2_i32.pow(((mask + 1) as u32) - 1)
   }

   fn translate_instruction(self) -> u32 {
      match self {
         Instruction::NA => panic!("NA Instruction received! Fatal error!"),

         Instruction::Jal(reg, imm) => jtype_instr(reg, imm),
         Instruction::Jalr(reg1, reg2, imm) => itype_instr(reg1, reg2, imm) + 0b1100111_u32,

         // B-Type instruction ; imm is the address!
         Instruction::Beq(reg1, reg2, imm) => btype_instr(reg1, reg2, imm), // Branch if equal 
         Instruction::Bne(reg1, reg2, imm) => btype_instr(reg1, reg2, imm) + 0b10_00000_00000_u32, // Branch if not equal
         Instruction::Blt(reg1, reg2, imm) => btype_instr(reg1, reg2, imm) + 0b1000_00000_00000_u32, // Branch if less than
         Instruction::Bltu(reg1, reg2, imm) => btype_instr(reg1, reg2, imm) + 0b1100_00000_00000_u32,
         Instruction::Bge(reg1, reg2, imm) => btype_instr(reg1, reg2, imm) + 0b1010_00000_00000_u32, // Branch if greater or equal
         Instruction::Bgeu(reg1, reg2, imm)=> btype_instr(reg1, reg2, imm) + 0b1110_00000_00000_u32,

         // 1 reg & 1 imm
         // Need VLd and VSt as well for variables
         // U-Type instruction
         Instruction::Lui(reg, imm) => utype_instr(reg, imm) + 0b0110111_u32,
         Instruction::Auipc(reg, imm) => utype_instr(reg, imm) + 0b0010111_u32,

         // I-Type instruction; reg1 == rd
         Instruction::Addi(reg1, reg2, imm) => itype_instr(reg1, reg2, imm) + 0b0010011_u32,
         
         Instruction::Xori(reg1, reg2, imm) => itype_instr(reg1, reg2, imm) + 0b0010011_u32 + 0b1000_00000_00000_u32,
         Instruction::Ori(reg1, reg2, imm) => itype_instr(reg1, reg2, imm) + 0b0010011_u32 + 0b1100_00000_00000_u32,
         Instruction::Andi(reg1, reg2, imm) => itype_instr(reg1, reg2, imm) + 0b0010011_u32 + 0b1110_00000_00000_u32,

         Instruction::Lb(reg1, reg2, imm) => itype_instr(reg1, reg2, imm) + 0b0000011_u32, //Load byte
         Instruction::Lh(reg1, reg2, imm) => itype_instr(reg1, reg2, imm) + 0b0000011_u32 + 0b10_00000_00000_u32, //Load half
         Instruction::Lw(reg1, reg2, imm) => itype_instr(reg1, reg2, imm) + 0b0000011_u32 + 0b100_00000_00000_u32, //Load word
         
         Instruction::Lbu(reg1, reg2, imm) => itype_instr(reg1, reg2, imm) + 0b0000011_u32 + 0b1000_00000_00000_u32, // ??
         Instruction::Lhu(reg1, reg2, imm) => itype_instr(reg1, reg2, imm) + 0b0000011_u32 + 0b1010_00000_00000_u32, // ??

         // Shift left|right logical|arithmetic|rotate
         Instruction::Slli(reg1, reg2, imm) => itype_instr(reg1, reg2, Instruction::mask(imm, 4)) + 0b0110011_u32 + 0b10_00000_00000,
         Instruction::Srli(reg1, reg2, imm) => itype_instr(reg1, reg2, Instruction::mask(imm, 4)) + 0b0110011_u32 + 0b1010_00000_00000,
         Instruction::Srai(reg1, reg2, imm) => itype_instr(reg1, reg2, Instruction::mask(imm, 4)) + 0b0110011_u32 + 0b1010_00000_00000 + (0x20 << 25),

         Instruction::Slti(reg1, reg2, imm) => itype_instr(reg1, reg2, imm) + 0b0010011_u32 + 0b100_00000_00000_u32,
         Instruction::Sltiu(reg1, reg2, imm) => itype_instr(reg1, reg2, imm) + 0b0010011_u32 + 0b110_00000_00000_u32, 

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
         
         Instruction::Sll(reg3, reg1, reg2) => rtype_instr(reg3, reg1, reg2) + 0b10_00000_00000_u32,
         Instruction::Srl(reg3, reg1, reg2) => rtype_instr(reg3, reg1, reg2) + 0b1010_00000_00000_u32,
         Instruction::Sra(reg3, reg1, reg2) => (0b100000_u32 << 25) + rtype_instr(reg3, reg1, reg2) + 0b1010_00000_00000_u32,

         Instruction::Xnor(reg3, reg1, reg2) => rtype_instr(reg3, reg1, reg2) + 0b1110_00000_00000_u32 + (0b100000_u32 << 25),
         Instruction::Equal(reg3, reg1, reg2) => rtype_instr(reg3, reg1, reg2) + 0b110_00000_00000_u32 + (0b100000_u32 << 25),
      }
   }
}

pub fn translate(input: Vec<Instruction>) -> Vec<u8> {
   let mut output: Vec<u8> = vec![];

   for instr in input.iter(){
      output.extend(Instruction::translate_instruction(instr.clone()).to_be_bytes());
   }

   output

   
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::borrow::Cow;
    use crate::{common::{Instruction, Reg, MacroInstr}, linker::link};

    
    #[test]
    fn translate_instruction_test (){
      let mut _inst: Instruction = Instruction::Jal(Reg::G0, 1864135);
      assert_eq!(0b10011100011011000111000001101111, Instruction::translate_instruction(_inst));
      
      _inst = Instruction::Jalr(Reg::G1, Reg::G2, 1138);
      assert_eq!(0b1000111001000010000011100111, Instruction::translate_instruction(_inst));
      
      
      _inst = Instruction::Beq(Reg::G3, Reg::G4, 3364);
      println!("{:#032b}", Instruction::translate_instruction(_inst.clone()));
      println!("{:#032b}", (0b10100100010000011000010011100011 as u32));
      assert_eq!(0b10100100010000011000010011100011, Instruction::translate_instruction(_inst));
      _inst = Instruction::Bne(Reg::G5, Reg::G6, 3364);
      assert_eq!(0b10100100011000101001010011100011, Instruction::translate_instruction(_inst));
      _inst = Instruction::Bltu(Reg::G7, Reg::G8, 3364);
      assert_eq!(0b1010010010000011111001001,Instruction::translate_instruction(_inst));
      _inst = Instruction::Bge(Reg::G9, Reg::G10, 3364);
      assert_eq!(0b10100100101001001101010011100011, Instruction::translate_instruction(_inst));
      
      _inst = Instruction::Lui(Reg::G11, 757305);
      assert_eq!(0b10111000111000111001010110110111, Instruction::translate_instruction(_inst));
      _inst = Instruction::Auipc(Reg::G12, 757305);
      assert_eq!(0b10111000111000111001011000010111, Instruction::translate_instruction(_inst));

      _inst = Instruction::Addi(Reg::G13, Reg::G14, 2953);
      assert_eq!(0b10111000100101110000011010010011, Instruction::translate_instruction(_inst));
      _inst = Instruction::Andi(Reg::G15, Reg::G16, 2953);
      assert_eq!(0b10111000100110000111011110010011, Instruction::translate_instruction(_inst));
      _inst = Instruction::Srli(Reg::G17, Reg::G18, 2953);
      assert_eq!(0b10111000100110010101100010010011, Instruction::translate_instruction(_inst));
      _inst = Instruction::Sltiu(Reg::G19, Reg::G20, 2953);
      assert_eq!(0b10111000100110100011100110010011, Instruction::translate_instruction(_inst));

      _inst = Instruction::Sb(Reg::G21, Reg::G22, 2953);
      assert_eq!(0b10111001010010011000010010100011, Instruction::translate_instruction(_inst));
      _inst = Instruction::Sh(Reg::G23, Reg::G24, 2953);
      assert_eq!(0b10111001100010111001010010100011, Instruction::translate_instruction(_inst));
      _inst = Instruction::Sw(Reg::G25, Reg::G26, 2953);
      assert_eq!(0b10111001101011001010010010100011, Instruction::translate_instruction(_inst));

      _inst = Instruction::Addn(Reg::G27, Reg::G28, Reg::G29);
      assert_eq!(0b00000001110011011000111010110011, Instruction::translate_instruction(_inst));
      _inst = Instruction::And(Reg::G30, Reg::G31, Reg::G0);
      assert_eq!(0b00000001111111110111000000110011, Instruction::translate_instruction(_inst));
      _inst = Instruction::Sra(Reg::G1, Reg::G2, Reg::G3);
      assert_eq!(0b1000000001000001101000110110011, Instruction::translate_instruction(_inst));
    }
}