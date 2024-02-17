/*
 * Copyright (c) 2023 Steven Becker
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

use console::{StyledObject, Style};

use crate::common::Instruction;

fn pretty_imm_short(translation: u32) -> StyledObject<String> {
    Style::new().green().apply_to(format!("{:012b}", ((translation >> 20) & (2_u32.pow(12) - 1))))
}

fn pretty_func7(translation: u32) -> StyledObject<String> {
    Style::new().magenta().apply_to(format!("{:07b}", ((translation >> 25) & (2_u32.pow(7) - 1))))
}

fn pretty_func3(translation: u32) -> StyledObject<String> {
    Style::new().blue().apply_to(format!("{:03b}", ((translation >> 12) & (2_u32.pow(3) - 1))))
}

fn pretty_imm_frac(translation: u32, pos: u8) -> StyledObject<String> {
    Style::new().green().apply_to(match pos {
        1 => format!("{:05b}", ((translation >> 7) & (2_u32.pow(5) - 1))),
        2 => format!("{:07b}", ((translation >> 25) & (2_u32.pow(7) - 1))),
        _ => unreachable!(),
    })
}

fn pretty_imm_long(translation: u32) -> StyledObject<String> {
    Style::new().green().apply_to(format!("{:020b}", ((translation >> 12) & (2_u32.pow(20) - 1))))
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
        // U-Type instruction
        Instruction::Lui(_, _) |
        Instruction::Auipc(_, _) |
        // J-Type
        Instruction::Jal(_, _) => format!("Emitted {}{}{opcode} from '{instr}'", pretty_imm_long(translation), 
                                            pretty_register(translation, 1)),

        // S-Type instruction
        Instruction::Sb(_, _, _) |
        Instruction::Sh(_, _, _) |
        Instruction::Sw(_, _, _) |
        // B-Type instruction ; imm is the address!
        Instruction::Beq(_, _, _) |
        Instruction::Bne(_, _, _) |
        Instruction::Blt(_, _, _) |
        Instruction::Bltu(_, _, _) |
        Instruction::Bge(_, _, _) |
        Instruction::Bgeu(_, _, _)=> format!("Emitted {}{}{}{}{}{opcode} from '{instr}'", pretty_imm_frac(translation, 2), 
                                                pretty_register(translation, 3), pretty_register(translation, 2), 
                                                pretty_func3(translation), pretty_imm_frac(translation, 1)),

        // I-Type instruction; reg1 == rd
        Instruction::Jalr(_, _, _) |

        Instruction::Addi(_, _, _) |

        Instruction::Xori(_, _, _) |
        Instruction::Ori(_, _, _) |
        Instruction::Andi(_, _, _) |

        Instruction::Lb(_, _, _) |
        Instruction::Lh(_, _, _) |
        Instruction::Lw(_, _, _) |

        Instruction::Lbu(_, _, _) |
        Instruction::Lhu(_, _, _) |

        // Shift left|right logical|arithmetic|rotate
        Instruction::Slli(_, _, _) |
        Instruction::Srli(_, _, _) |
        Instruction::Srai(_, _, _) |

        Instruction::Slti(_, _, _) |
        Instruction::Sltiu(_, _, _) => format!("Emitted {}{}{}{}{opcode} from '{instr}'", pretty_imm_short(translation), 
                                                pretty_register(translation, 2), pretty_func3(translation), 
                                                pretty_register(translation, 1)),

        // R-Type instruction; 3 regs
        Instruction::Add(_, _, _) |
        Instruction::Sub(_, _, _) |
        
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

        Instruction::Mul(_, _, _) |
        Instruction::Mulh(_, _, _) |
        Instruction::Mulhsu(_, _, _) |
        Instruction::Mulhu(_, _, _) |

        Instruction::Div(_, _, _) |
        Instruction::Divu(_, _, _) |
        Instruction::Rem(_, _, _) |
        Instruction::Remu(_, _, _) => format!("Emitted {}{}{}{}{}{opcode} from '{instr}'", pretty_func7(translation), 
                                                pretty_register(translation, 3), pretty_register(translation, 2), 
                                                pretty_func3(translation), pretty_register(translation, 1)),
     }
}
