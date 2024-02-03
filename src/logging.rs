/*
 * Copyright (c) 2023 Steven Becker
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

use console::{StyledObject, Style};

use crate::common::Instruction;

// console::Style::new().bold().apply_to("Assembled")

fn pretty_imm_long(translation: u32) -> StyledObject<String> {
    Style::new().green().apply_to(format!("{:020b}", ((translation >> 11) & (2_u32.pow(20) - 1))))
}

fn pretty_register(translation: u32, pos: u8) -> StyledObject<String> {
    Style::new().yellow().apply_to(match pos {
        1 => format!("{:05b}", ((translation >> 7) & (2_u32.pow(5) - 1))),
        2 => format!("{:05b}", ((translation >> 15) & (2_u32.pow(5) - 1))),
        3 => format!("{:05b}", ((translation >> 20) & (2_u32.pow(5) - 1))),
        _ => unreachable!(),
    })
}

fn pretty_opcode(translation: u32) -> StyledObject<String> {
    Style::new().red().apply_to(format!("{:07b}", (translation & (2_u32.pow(7) - 1))))
}

pub fn emit_debug_translate_instruction(instr: &Instruction, translation: u32) -> String {
    let opcode = pretty_opcode(translation);
    match instr {
        // J-Type
        Instruction::Jal(_, _) => format!("Emitted {}{}{opcode} from {:?}", pretty_imm_long(translation), pretty_register(translation, 1), instr),
        Instruction::Jalr(_, _, _) => "".to_string(),

        // B-Type instruction ; imm is the address!
        Instruction::Beq(_, _, _) => "".to_string(),
        Instruction::Bne(_, _, _) => "".to_string(),
        Instruction::Blt(_, _, _) => "".to_string(),
        Instruction::Bltu(_, _, _) => "".to_string(),
        Instruction::Bge(_, _, _) => "".to_string(),
        Instruction::Bgeu(_, _, _)=> "".to_string(),

        // 1 reg & 1 imm
        // Need VLd and VSt as well for variables
        // U-Type instruction
        Instruction::Lui(_, _) => "".to_string(),
        Instruction::Auipc(_, _) => "".to_string(),

        // I-Type instruction; reg1 == rd
        Instruction::Addi(_, _, _) => "".to_string(),
        
        Instruction::Xori(_, _, _) => "".to_string(),
        Instruction::Ori(_, _, _) => "".to_string(),
        Instruction::Andi(_, _, _) => "".to_string(),

        Instruction::Lb(_, _, _) => "".to_string(),
        Instruction::Lh(_, _, _) => "".to_string(),
        Instruction::Lw(_, _, _) => "".to_string(),
        
        Instruction::Lbu(_, _, _) => "".to_string(),
        Instruction::Lhu(_, _, _) => "".to_string(),

        // Shift left|right logical|arithmetic|rotate
        Instruction::Slli(_, _, _) => "".to_string(),
        Instruction::Srli(_, _, _) => "".to_string(),
        Instruction::Srai(_, _, _) => "".to_string(),

        Instruction::Slti(_, _, _) => "".to_string(),
        Instruction::Sltiu(_, _, _) => "".to_string(), 

        // S-Type instruction
        Instruction::Sb(_, _, _) |
        Instruction::Sh(_, _, _) |
        Instruction::Sw(_, _, _) => "".to_string(),

        // R-Type instruction; 3 regs
        Instruction::Addn(_, _, _) |
        Instruction::Subn(_, _, _) |
        
        Instruction::Xor(_, _, _) |
        Instruction::Or(_, _, _) |
        Instruction::And(_, _, _) |
        
        Instruction::Slt(_, _, _) |
        Instruction::Sltu(_, _, _) |
        
        Instruction::Sll(_, _, _) |
        Instruction::Srl(_, _, _) |
        Instruction::Sra(_, _, _) |

        Instruction::Xnor(_, _, _) |
        Instruction::Equal(_, _, _) |

        Instruction::Muln(_, _, _) |
        Instruction::Mulh(_, _, _) => "".to_string(),
     }
}