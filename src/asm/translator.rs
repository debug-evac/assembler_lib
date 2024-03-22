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

mod pretty_print;

use std::{
   fs::File, io::Write, path::{Path, PathBuf}
};
use log::{debug, info, log_enabled, warn};

use self::pretty_print::emit_debug_translate_instruction;
use crate::common::{ByteData, DWordData, HalfData, Imm, Instruction, MemData, Reg, TranslatableCode, WordData, errors::TranslatorError};

fn btype_instr(rs1: &Reg, rs2: &Reg, imm: &Imm) -> u32 {
   let u12 =  (imm & 0b100_00000_00000i32) >> 1;
   let l11 = (imm & 0b10_00000_00000i32) >> 11;
   let upper = ((imm & 0b1_11111_00000i32) | u12) << 20;
   let lower = ((imm & 0b11110i32) | l11) << 7;
   ((upper + lower) as u32) + ((*rs2 as u32) << 20) + ((*rs1 as u32) << 15) + 0b1100011_u32
}

fn stype_instr(rs1: &Reg, rs2: &Reg, imm: &Imm) -> u32 {
   let upper =  (imm & 0b11_11111_10000i32) << 20;
   let lower = (imm & 0b1111i32) << 7;
   ((upper + lower) as u32) + ((*rs1 as u32) << 20) + ((*rs2 as u32) << 15) + 0b100011_u32
}

fn itype_instr(rd: &Reg, rs1: &Reg, imm: &Imm) -> u32 {
   ((*imm as u32) << 20) + ((*rs1 as u32) << 15) + ((*rd as u32) << 7)
}

fn rtype_instr(rd: &Reg, rs1: &Reg, rs2: &Reg) -> u32 {
   ((*rs2 as u32) << 20) + ((*rs1 as u32) << 15) + ((*rd as u32) << 7) + 0b0110011_u32
}

fn utype_instr(rd: &Reg, imm: &Imm) -> u32 {
   (((*imm as u32) >> 12) << 12) + ((*rd as u32) << 7)
}

fn jtype_instr(rd: &Reg, imm: &Imm) -> u32 {
   let ten_to_one = (imm & 0b01_11111_11110_i32) << 8;
   let nineteen_to_twelve = (imm & 0b0_11111_11100_00000_00000_i32) >> 12;
   let eleven = (imm & 0b010_00000_00000_i32) >> 3;
   let twenty = imm & 0b01_00000_00000_00000_00000_i32 >> 1;

   (((ten_to_one | nineteen_to_twelve | eleven | twenty) as u32) << 12) + ((*rd as u32) << 7) + 0b1101111_u32
}

fn funct3(value: u8) -> u32 {
   (value as u32) << 12
}

trait Translate {
   fn mask(imm: i32, mask: u8) -> i32 {
      imm & (2_i32.pow((mask + 1) as u32) - 1)
   }
   fn translate_instruction(&self) -> u32;
}

impl Translate for Instruction {
   fn translate_instruction(&self) -> u32 {
      let mach_instr = match self {
         Instruction::Jal(reg, imm) => jtype_instr(reg, imm),
         Instruction::Jalr(reg1, reg2, imm) => itype_instr(reg1, reg2, imm) + 0b1100111_u32,

         // B-Type instruction ; imm is the address!
         Instruction::Beq(reg1, reg2, imm) => btype_instr(reg1, reg2, imm),
         Instruction::Bne(reg1, reg2, imm) => btype_instr(reg1, reg2, imm) + funct3(1),
         Instruction::Blt(reg1, reg2, imm) => btype_instr(reg1, reg2, imm) + funct3(4),
         Instruction::Bltu(reg1, reg2, imm) => btype_instr(reg1, reg2, imm) + funct3(6),
         Instruction::Bge(reg1, reg2, imm) => btype_instr(reg1, reg2, imm) + funct3(5),
         Instruction::Bgeu(reg1, reg2, imm)=> btype_instr(reg1, reg2, imm) + funct3(7),

         // 1 reg & 1 imm
         // Need VLd and VSt as well for variables
         // U-Type instruction
         Instruction::Lui(reg, imm) => utype_instr(reg, imm) + 0b0110111_u32,
         Instruction::Auipc(reg, imm) => utype_instr(reg, imm) + 0b0010111_u32,

         // I-Type instruction; reg1 == rd
         Instruction::Addi(reg1, reg2, imm) => itype_instr(reg1, reg2, imm) + 0b0010011_u32,
         
         Instruction::Xori(reg1, reg2, imm) => itype_instr(reg1, reg2, imm) + 0b0010011_u32 + funct3(4),
         Instruction::Ori(reg1, reg2, imm) => itype_instr(reg1, reg2, imm) + 0b0010011_u32 + funct3(6),
         Instruction::Andi(reg1, reg2, imm) => itype_instr(reg1, reg2, imm) + 0b0010011_u32 + funct3(7),

         Instruction::Lb(reg1, reg2, imm) => itype_instr(reg1, reg2, imm) + 0b0000011_u32,
         Instruction::Lh(reg1, reg2, imm) => itype_instr(reg1, reg2, imm) + 0b0000011_u32 + funct3(1),
         Instruction::Lw(reg1, reg2, imm) => itype_instr(reg1, reg2, imm) + 0b0000011_u32 + funct3(2),
         
         Instruction::Lbu(reg1, reg2, imm) => itype_instr(reg1, reg2, imm) + 0b0000011_u32 + funct3(4),
         Instruction::Lhu(reg1, reg2, imm) => itype_instr(reg1, reg2, imm) + 0b0000011_u32 + funct3(5),

         // Shift left|right logical|arithmetic|rotate
         Instruction::Slli(reg1, reg2, imm) => itype_instr(reg1, reg2, &Instruction::mask(*imm, 4)) + 0b0010011_u32 + funct3(1),
         Instruction::Srli(reg1, reg2, imm) => itype_instr(reg1, reg2, &Instruction::mask(*imm, 4)) + 0b0010011_u32 + funct3(5),
         Instruction::Srai(reg1, reg2, imm) => itype_instr(reg1, reg2, &Instruction::mask(*imm, 4)) + 0b0010011_u32 + funct3(5) + (0x20 << 25),

         Instruction::Slti(reg1, reg2, imm) => itype_instr(reg1, reg2, imm) + 0b0010011_u32 + funct3(2),
         Instruction::Sltiu(reg1, reg2, imm) => itype_instr(reg1, reg2, imm) + 0b0010011_u32 + funct3(3), 

         // S-Type instruction
         Instruction::Sb(reg1, reg2, imm) => stype_instr(reg1, reg2, imm) + funct3(4),
         Instruction::Sh(reg1, reg2, imm) => stype_instr(reg1, reg2, imm) + funct3(5),
         Instruction::Sw(reg1, reg2, imm) => stype_instr(reg1, reg2, imm) + funct3(6),

         // R-Type instruction; 3 regs
         Instruction::Add(reg3, reg1, reg2) => rtype_instr(reg3, reg1, reg2),
         Instruction::Sub(reg3, reg1, reg2) => rtype_instr(reg3, reg1, reg2) + (0b100000_u32 << 25),
         
         Instruction::Xor(reg3, reg1, reg2) => rtype_instr(reg3, reg1, reg2) + funct3(4),
         Instruction::Or(reg3, reg1, reg2) => rtype_instr(reg3, reg1, reg2) + funct3(6),
         Instruction::And(reg3, reg1, reg2) => rtype_instr(reg3, reg1, reg2) + funct3(7),
         
         Instruction::Slt(reg3, reg1, reg2) => rtype_instr(reg3, reg1, reg2) + funct3(2),
         Instruction::Sltu(reg3, reg1, reg2) => rtype_instr(reg3, reg1, reg2) + funct3(3),
         
         Instruction::Sll(reg3, reg1, reg2) => rtype_instr(reg3, reg1, reg2) + funct3(1),
         Instruction::Srl(reg3, reg1, reg2) => rtype_instr(reg3, reg1, reg2) + funct3(5),
         Instruction::Sra(reg3, reg1, reg2) => (0b100000_u32 << 25) + rtype_instr(reg3, reg1, reg2) + funct3(5),

         Instruction::Xnor(reg3, reg1, reg2) => rtype_instr(reg3, reg1, reg2) + funct3(7) + (0b100000_u32 << 25),
         Instruction::Equal(reg3, reg1, reg2) => rtype_instr(reg3, reg1, reg2) + funct3(3) + (0b100000_u32 << 25),

         Instruction::Mul(reg3, reg1, reg2) => rtype_instr(reg3, reg1, reg2) + (1_u32 << 25),
         Instruction::Mulh(reg3, reg1, reg2) => rtype_instr(reg3, reg1, reg2) + funct3(1) + (1_u32 << 25),
         Instruction::Mulhsu(reg3, reg1, reg2) => rtype_instr(reg3, reg1, reg2) + funct3(2) + (1_u32 << 25),
         Instruction::Mulhu(reg3, reg1, reg2) => rtype_instr(reg3, reg1, reg2) + funct3(3) + (1_u32 << 25),

         Instruction::Div(reg3, reg1, reg2) => rtype_instr(reg3, reg1, reg2) + funct3(4) + (1_u32 << 25),
         Instruction::Divu(reg3, reg1, reg2) => rtype_instr(reg3, reg1, reg2) + funct3(5) + (1_u32 << 25),
         Instruction::Rem(reg3, reg1, reg2) => rtype_instr(reg3, reg1, reg2) + funct3(6) + (1_u32 << 25),
         Instruction::Remu(reg3, reg1, reg2) => rtype_instr(reg3, reg1, reg2) + funct3(7) + (1_u32 << 25),

         Instruction::Ebreak => 0b1110011_u32,
         Instruction::Ecall => itype_instr(&Reg::G0, &Reg::G0, &1) + 0b1110011_u32,
      };
      if log_enabled!(log::Level::Debug) {
         debug!("{}", emit_debug_translate_instruction(self, mach_instr));
      }
      mach_instr
   }
}

fn set_data_path(output: &Path, extension: &str) -> PathBuf {
   let mut datapath = output.to_path_buf();
   datapath.set_extension(extension);
   info!("Data specified. It will be saved to {:?}", datapath);
   datapath
}

trait Translatable {
   fn translate(&self) -> u32;
}

impl Translatable for Instruction {
   fn translate(&self) -> u32 {
      self.translate_instruction()
   }
}

impl Translatable for u32 {
   fn translate(&self) -> u32 {
      *self
   }
}

trait Writable {
   // about to be phased out, only included here to not have to deal with two proceedings
   fn write_to_mif(&self, output: &Path, depth: u16, width: u8, comment: bool) -> Result<(), TranslatorError>;
   fn write_raw(&self, output: &Path) -> Result<(), std::io::Error>;
}

impl <T: Translatable + std::fmt::Display> Writable for &Vec<T> {
   fn write_raw(&self, output: &Path) -> Result<(), std::io::Error> {
      let mut file = File::create(output)?;
      let mut byte_instrs: Vec<u8> = vec![];

      for instr in self.iter() {
         byte_instrs.extend(instr.translate().to_le_bytes());
      }
      file.write_all(&byte_instrs)?;
      Ok(())
   }

   fn write_to_mif(&self, output: &Path, depth: u16, width: u8, comment: bool) -> Result<(), TranslatorError> {
      let (sep, added) = if width == 32 {
         if (depth as usize) < self.len() {
            return Err(TranslatorError::DepthNotFit(depth as usize, self.len()))
         }
         ("", 1)
      } else if width == 8 {
         let addr_count = (depth as usize) / 4;
         if addr_count < self.len() {
            return Err(TranslatorError::DepthNotFit(addr_count, self.len()))
         }
         (" ", 4)
      } else {
         return Err(TranslatorError::UndefinedWidth(width));
      };

      let mut file = File::create(output)?;

      writeln!(file, "DEPTH = {depth};\nWIDTH = {width};\nADDRESS_RADIX = DEC;\nDATA_RADIX = BIN;\nCONTENT\nBEGIN")?;

      let mut counter: usize = 0;
      if comment {
         for elem in self.iter() {
            let mach_instr = elem.translate().to_le_bytes();
            writeln!(file, "{counter}\t: {:08b}{sep}{:08b}{sep}{:08b}{sep}{:08b};\t\t-- {elem}", mach_instr[0], mach_instr[1], mach_instr[2], mach_instr[3])?;
            counter += added;
         }
      } else {
         for elem in self.iter() {
            let mach_instr = elem.translate().to_le_bytes();
            writeln!(file, "{counter}\t: {:08b}{sep}{:08b}{sep}{:08b}{sep}{:08b};", mach_instr[0], mach_instr[1], mach_instr[2], mach_instr[3])?;
            counter += added;
         }
      }

      writeln!(file, "END;")?;

      Ok(())
   }
}

fn translate_data_to_word_vec(memdata: &[MemData]) -> Vec<u32> {
   let mut translatable_data: Vec<u8> = vec![];

   for data in memdata.iter() {
      match data {
         MemData::Bytes(data_vec, _) => {
            for byte in data_vec {
               if let ByteData::Byte(num) = byte {
                  let bytes = num.to_le_bytes();
                  translatable_data.push(bytes[0]);
               }
            }
         },
         MemData::Halfs(data_vec) => {
            for half in data_vec {
               if let HalfData::Half(num) = half {
                  let bytes = num.to_le_bytes();
                  translatable_data.push(bytes[0]);
                  translatable_data.push(bytes[1]);
               }
            }
         },
         MemData::Words(data_vec) => {
            for word in data_vec {
               if let WordData::Word(num) = word {
                  let bytes = num.to_le_bytes();
                  for byte in bytes.iter().take(4) {
                     translatable_data.push(*byte);
                  }
               }
            }
         },
         MemData::DWords(data_vec) => {
            for dword in data_vec {
               if let DWordData::DWord(num) = dword {
                  let bytes = num.to_le_bytes();
                  for byte in bytes.iter().take(8) {
                     translatable_data.push(*byte);
                  }
               }
            }
         },
         MemData::Namespace(_) => (),
     }
   }

   let mut word_vec: Vec<u32> = vec![];
   let mut iter = translatable_data.chunks_exact(4);

   for byte in iter.by_ref() {
      word_vec.push(((byte[3] as u32) << 24) + ((byte[2] as u32) << 16) + ((byte[1] as u32) << 8) + (byte[0] as u32));
   }

   let remainder = iter.remainder();

   match remainder.len() {
      0 => (),
      1 => word_vec.push((remainder[0] as u32)<< 24),
      2 => word_vec.push(((remainder[1] as u32) << 24) + ((remainder[0] as u32) << 16)),
      3 => word_vec.push(((remainder[2] as u32) << 24) + ((remainder[1] as u32) << 16) + ((remainder[0] as u32) << 8)),
      _ => unreachable!(),
   }

   word_vec
}

pub fn translate_and_present(output: &PathBuf, translate_code: TranslatableCode, comment: bool, format: &str, size: (u16, u8)) -> Result<(), TranslatorError> {
   let (input, data) = translate_code.get_all_ref();

   let outdata;
   let data_vec;

   let mut translation_vec: Vec<(Box<dyn Writable>, &PathBuf)> = vec![];
   translation_vec.push((Box::from(input), output));

   match format {
      "mif" => {
         let depth = size.0;
         let width = size.1;

         if !data.is_empty() {
            outdata = set_data_path(output, "mem.mif");
            data_vec = translate_data_to_word_vec(data);
            translation_vec.push((Box::from(&data_vec), &outdata));
         }

         for write_tuple in translation_vec {
            write_tuple.0.write_to_mif(write_tuple.1, depth, width, comment)?;
         }
      },
      "raw" => {
         if comment {
            warn!("Comment flag has no effect on raw format!");
         }

         if !data.is_empty() {
            outdata = set_data_path(output, "mem.bin");
            data_vec = translate_data_to_word_vec(data);
            translation_vec.push((Box::from(&data_vec), &outdata));
         }

         for write_tuple in translation_vec {
            write_tuple.0.write_raw(write_tuple.1)?;
         }
      },
      "debug" => {
         if comment {
            warn!("Comment flag has no effect on debug format!");
         }
         if output.to_string_lossy() != "a.bin" {
            warn!("Debug format chosen! Nothing will be written to {:?}!", output);
         }
         for instr in input.iter() {
            let mach_instr = instr.translate_instruction();
            info!("{}", emit_debug_translate_instruction(instr, mach_instr));
         }
      }
      _ => return Err(TranslatorError::UndefinedFormat(format.to_string())),
   }
   Ok(())
}

#[cfg(test)]
mod tests {
   use crate::common::{Instruction, Reg};
   use crate::asm::translator::Translate;

   #[test]
   fn test_translate_rtype_instr() {
      let mut instr = Instruction::Add(Reg::G27, Reg::G28, Reg::G29);
      assert_eq!(0b00_00000_11101_11100_00011_01101_10011, instr.translate_instruction());
      instr = Instruction::And(Reg::G30, Reg::G31, Reg::G0);
      assert_eq!(0b00_00000_00000_11111_11111_11001_10011, instr.translate_instruction());
      instr = Instruction::Sra(Reg::G1, Reg::G2, Reg::G3);
      //println!("{:#034b}", Instruction::translate_instruction(instr.clone()));
      //println!("{:#034b}", (0b01_00000_00011_00010_10100_00101_10011 as u32));
      assert_eq!(0b01_00000_00011_00010_10100_00101_10011, instr.translate_instruction());
   }

   #[test]
   fn test_translate_itype_instr() {
      let mut instr = Instruction::Jalr(Reg::G1, Reg::G2, 1138);
      assert_eq!(0b01_00011_10010_00010_00000_00111_00111, instr.translate_instruction());

      instr = Instruction::Addi(Reg::G13, Reg::G14, 2953);
      assert_eq!(0b10_11100_01001_01110_00001_10100_10011, instr.translate_instruction());
      instr = Instruction::Andi(Reg::G15, Reg::G16, 2953);
      assert_eq!(0b10111000100110000111011110010011, instr.translate_instruction());
      instr = Instruction::Srli(Reg::G17, Reg::G18, 2953);
      assert_eq!(0b00_00000_01001_10010_10110_00100_10011, instr.translate_instruction());
      instr = Instruction::Sltiu(Reg::G19, Reg::G20, 2953);
      assert_eq!(0b10111000100110100011100110010011, instr.translate_instruction());
   }

   #[test]
   fn test_translate_stype_instr() {
      let mut instr = Instruction::Sb(Reg::G21, Reg::G22, 2953);
      //println!("{:#034b}", Instruction::translate_instruction(instr.clone()));
      //println!("{:#034b}", (0b10_11100_10101_10110_10001_00101_00011 as u32));
      assert_eq!(0b10_11100_10101_10110_10001_00101_00011, instr.translate_instruction());
      instr = Instruction::Sh(Reg::G23, Reg::G24, 2953);
      assert_eq!(0b10_11100_10111_11000_10101_00101_00011, instr.translate_instruction());
      instr = Instruction::Sw(Reg::G25, Reg::G26, 2953);
      assert_eq!(0b10_11100_11001_11010_11001_00101_00011, instr.translate_instruction());
   }

   #[test]
   fn test_translate_btype_instr() {
      let mut instr = Instruction::Beq(Reg::G3, Reg::G4, 3364);
      assert_eq!(0b01_01001_00100_00011_00000_10111_00011, instr.translate_instruction());
      instr = Instruction::Bne(Reg::G5, Reg::G6, 3364);
      assert_eq!(0b01_01001_00110_00101_00100_10111_00011, instr.translate_instruction());
      instr = Instruction::Bltu(Reg::G7, Reg::G8, 3364);
      assert_eq!(0b01_01001_01000_00111_11000_10111_00011,instr.translate_instruction());
      instr = Instruction::Bge(Reg::G9, Reg::G10, 3364);
      assert_eq!(0b01_01001_01010_01001_10100_10111_00011, instr.translate_instruction());
   }

   #[test]
   fn test_translate_other_instr() {
      let mut instr: Instruction = Instruction::Jal(Reg::G0, 1864135);
      assert_eq!(0b10_01110_00110_11000_11100_00011_01111, instr.translate_instruction());
      instr = Instruction::Lui(Reg::G12, -2147483648);
      assert_eq!(0b10_00000_00000_00000_00001_10001_10111, instr.translate_instruction());
      instr = Instruction::Lui(Reg::G11, 757305);
      assert_eq!(0b00_00000_10111_00001_01101_10111, instr.translate_instruction());
      instr = Instruction::Auipc(Reg::G12, 757305);
      assert_eq!(0b00_00000_10111_00001_10000_10111, instr.translate_instruction());
   }
}
