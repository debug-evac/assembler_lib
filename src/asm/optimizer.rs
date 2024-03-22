/*
 * Copyright (c) 2023 Steven Becker
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

//
// Optional optimizer that tries to reduce NOOP operations and
// maximizes parallelization (in the pipeline) of operations.
// Relevant techniques include register renaming, instruction
// scheduling, peephole optimization (reduction of certain
// operations into shorter-running operations), among other
// things. This will run on every compilation (? rather
// translations), however changes to the programm will only be
// made, if the programmer includes an (or possibly a number of)
// flag(s). If there are no flags, the assembler will simply
// issue a notice that performance COULD be gained if
// restructuring the assembly code (can be suppressed by
// another flag).

use std::collections::VecDeque;
use log::debug;

use crate::{
    common::{errors::OptimizerError, AssemblyCode, AssemblyCodeNamespaces, ByteData, DWordData, HalfData, Instruction, LabelType, MacroInstr, MemData, Operation, Part, Reg, TranslatableCode, WordData
    },
    asm::linker::Namespaces,
};

#[allow(dead_code)]
#[derive(Clone)]
enum RegActType {
    NA,
    WriteRead2(Reg, Reg, Reg),
    WriteRead(Reg, Reg),
    Read2(Reg, Reg),
    Write(Reg),
    Store(Reg, Reg),
    Load(Reg, Reg)
}

impl RegActType {
    fn is_hazard_before(&self, before_self: &RegActType) -> bool {
        // Check for Read-After-Write (only for feature "raw_nop"), 
        // LOAD-STORE (only for feature "mem_load_nop") 
        // and LOAD-USE (always) dependency
        match before_self {
            RegActType::NA |
            RegActType::Store(_, _) |
            RegActType::Read2(_, _) => false,

            #[cfg(feature = "raw_nop")]
            RegActType::WriteRead2(write_t, _, _) |
            RegActType::Write(write_t) |
            RegActType::WriteRead(write_t, _) => {
                if *write_t == Reg::G0 {
                    return false;
                }
                match self {
                    RegActType::NA | RegActType::Write(_) => false,

                    RegActType::Store(reg1, reg2) |
                    RegActType::Read2(reg1, reg2) |
                    RegActType::WriteRead2(_, reg1, reg2) => (*reg1 == *write_t) || (*reg2 == *write_t),

                    RegActType::Load(_, reg) |
                    RegActType::WriteRead(_, reg) => *reg == *write_t,
                }
            },

            #[cfg(not(feature = "raw_nop"))]
            RegActType::WriteRead2(_, _, _) |
            RegActType::Write(_) |
            RegActType::WriteRead(_, _) => false,

            RegActType::Load(target, _) => {
                // Does not really make much sense to put x0
                // into load target but whatever
                if *target == Reg::G0 {
                    return false;
                }
                match self {
                    RegActType::NA | RegActType::Write(_) => false,

                    #[cfg(feature = "mem_load_nop")]
                    RegActType::Store(reg1, reg2) => (*reg1 == *target) || (*reg2 == *target),

                    #[cfg(not(feature = "mem_load_nop"))]
                    RegActType::Store(_, reg) => *reg == *target,

                    RegActType::Read2(reg1, reg2) |
                    RegActType::WriteRead2(_, reg1, reg2) => (*reg1 == *target) || (*reg2 == *target),

                    RegActType::Load(_, reg) |
                    RegActType::WriteRead(_, reg) => *reg == *target,
                }
            },
        }
    }
}

struct LimitedQueue {
    size: usize,
    queue: VecDeque<RegActType>
}

impl LimitedQueue {
    fn new_sized(size: usize) -> LimitedQueue {
        let queue = VecDeque::with_capacity(size);
        LimitedQueue{ size, queue }
    }

    fn limited_insert(&mut self, reg: RegActType) {
        if self.queue.len() == self.size {
            self.queue.pop_back();
        }
        self.queue.push_front(reg);
    }

    fn compare_and_insert(&mut self, reg: RegActType) -> Option<i8> {
        for (counter, reg_dep) in self.queue.iter().enumerate() {
            if reg.is_hazard_before(reg_dep) {
                for _ in counter..3 {
                    self.limited_insert(RegActType::NA);
                }
                self.limited_insert(reg);
                return Some(counter as i8)
            }
        };
        self.limited_insert(reg);
        None
    }

    fn flush(&mut self) {
        self.queue.clear()
    }
}

impl From<&Operation> for RegActType {
    fn from(item: &Operation) -> RegActType {
        match item {
            Operation::LablInstr(_, instr) | Operation::Instr(instr) => {
                match instr {
                    Instruction::Mul(reg1, reg2, reg3) |
                    Instruction::Mulh(reg1, reg2, reg3) |
                    Instruction::Mulhsu(reg1, reg2, reg3) |
                    Instruction::Mulhu(reg1, reg2, reg3) |
                    Instruction::Div(reg1, reg2, reg3) |
                    Instruction::Divu(reg1, reg2, reg3) |
                    Instruction::Rem(reg1, reg2, reg3) |
                    Instruction::Remu(reg1, reg2, reg3) |
                    Instruction::Xnor(reg1, reg2, reg3) |
                    Instruction::Equal(reg1, reg2, reg3) |
                    Instruction::Add(reg1, reg2, reg3) |
                    Instruction::Sub(reg1, reg2, reg3) |
                    Instruction::Xor(reg1, reg2, reg3) |
                    Instruction::Or(reg1, reg2, reg3) |
                    Instruction::And(reg1, reg2, reg3) |

                    Instruction::Sll(reg1, reg2, reg3) |
                    Instruction::Srl(reg1, reg2, reg3) |
                    Instruction::Sra(reg1, reg2, reg3) |
                    Instruction::Slt(reg1, reg2, reg3) |
                    Instruction::Sltu(reg1, reg2, reg3) => RegActType::WriteRead2(*reg1, *reg2, *reg3),

                    Instruction::Addi(reg1, reg2, _) |
                    Instruction::Xori(reg1, reg2, _) |
                    Instruction::Ori(reg1, reg2, _) |
                    Instruction::Andi(reg1, reg2, _) |
                    Instruction::Slti(reg1, reg2, _) |
                    Instruction::Sltiu(reg1, reg2, _) |
                    Instruction::Slli(reg1, reg2, _) |
                    Instruction::Srli(reg1, reg2, _) |
                    Instruction::Srai(reg1, reg2, _) |
                    Instruction::Jalr(reg1, reg2, _) => RegActType::WriteRead(*reg1, *reg2),

                    Instruction::Lb(reg1, reg2, _) |
                    Instruction::Lh(reg1, reg2, _) |
                    Instruction::Lw(reg1, reg2, _) |
                    Instruction::Lbu(reg1, reg2, _) |
                    Instruction::Lhu(reg1, reg2, _) => RegActType::Load(*reg1, *reg2),

                    Instruction::Sh(reg1, reg2, _) |
                    Instruction::Sb(reg1, reg2, _) |
                    Instruction::Sw(reg1, reg2, _) => RegActType::Store(*reg1, *reg2),

                    Instruction::Beq(reg1, reg2, _) |
                    Instruction::Bne(reg1, reg2, _) |
                    Instruction::Blt(reg1, reg2, _) |
                    Instruction::Bltu(reg1, reg2, _) |
                    Instruction::Bge(reg1, reg2, _) |
                    Instruction::Bgeu(reg1, reg2, _) => RegActType::Read2(*reg1, *reg2),

                    Instruction::Lui(reg, _) |
                    Instruction::Auipc(reg, _) |
                    Instruction::Jal(reg, _) => RegActType::Write(*reg),

                    Instruction::Ecall |
                    Instruction::Ebreak => RegActType::NA,
                }
            },
            Operation::Macro(instr) | Operation::LablMacro(_, instr) => {
                match instr {
                    MacroInstr::Beq(reg1, reg2, _) |
                    MacroInstr::Bne(reg1, reg2, _) |
                    MacroInstr::Blt(reg1, reg2, _) |
                    MacroInstr::Bltu(reg1, reg2, _) |
                    MacroInstr::Bge(reg1, reg2, _) |
                    MacroInstr::Bgeu(reg1, reg2, _) => RegActType::Read2(*reg1, *reg2),

                    MacroInstr::Lui(reg, _, _) |
                    MacroInstr::Auipc(reg, _) |
                    MacroInstr::Jal(reg, _) => RegActType::Write(*reg),

                    MacroInstr::Addi(reg1, reg2, _, _) |
                    MacroInstr::Jalr(reg1, reg2, _, _) => RegActType::WriteRead(*reg1, *reg2),

                    MacroInstr::LbLabl(reg1, reg2, _, _) |
                    MacroInstr::LhLabl(reg1, reg2, _, _) |
                    MacroInstr::LwLabl(reg1, reg2, _, _) |
                    MacroInstr::LbuLabl(reg1, reg2, _) |
                    MacroInstr::LhuLabl(reg1, reg2, _) => RegActType::Load(*reg2, *reg1),

                    MacroInstr::ShLabl(reg1, reg2, _) |
                    MacroInstr::SbLabl(reg1, reg2, _) |
                    MacroInstr::SwLabl(reg1, reg2, _) => RegActType::Store(*reg1, *reg2),

                    _ => unreachable!(),
                }
            },
            _ => RegActType::NA,
        }
    }
}

fn handle_part(lines: &mut i32, part: &Part) {
    match part {
        //Part::Upper => *lines >> 12,
        //Part::Lower => *lines & 0b11_11111_11111,
        #[cfg(feature = "raw_nop")]
        Part::Upper => {
            if *lines < 2_i32.pow(12) {
                *lines += 20;
            } else {
                *lines += 24;
            }
        },
        #[cfg(feature = "raw_nop")]
        Part::Lower => {
            if *lines < 2_i32.pow(12) {
                *lines += 16;
            } else {
                *lines += 20;
            }
            return
        },
        #[cfg(not(feature = "raw_nop"))]
        Part::Upper => {
            // 4 plus offset of lower
            if *lines < 2_i32.pow(12) {
                *lines += 8;
            } else {
                *lines += 12;
            }
        },
        #[cfg(not(feature = "raw_nop"))]
        Part::Lower => {
            // next instruction (4) + 4
            if *lines < 2_i32.pow(12) {
                *lines += 4;
            } else {
                *lines += 8;
            }
            return
        },
        Part::None => return,
    }
    if *lines & 0x800 == 2048 {
        if lines.leading_ones() >= 19 {
            *lines = 0;
        } else {
            let mut mask: i32 = 4096;
            for _ in 12..32 {
                let is_unset = *lines & mask == 0;
                if is_unset {
                    *lines |= mask;
                    break;
                }
                mask <<= 1;
            }
        }
    }
}

trait Translate {
    fn translate(&self, namespace: &mut Namespaces, current_space: &usize, instructions: &mut Vec<Instruction>) -> Result<(), OptimizerError>;
 }

impl Translate for MacroInstr {
    fn translate(&self, namespace: &mut Namespaces, current_space: &usize, instructions: &mut Vec<Instruction>) -> Result<(), OptimizerError> {
        // Do not forget to change the lines function in the parser when changing the amount of lines here! 
        // (TODO: Better method for this)
        match self {
            MacroInstr::Addi(reg1, reg2, labl, part) => {
                let mut lines = translate_label(instructions.len() as i128, labl.to_owned(), namespace, *current_space)?;
                handle_part(&mut lines, part);
                instructions.push(Instruction::Addi(reg1.to_owned(), reg2.to_owned(), lines));
            },

            MacroInstr::Beq(reg1, reg2, labl) => {
                let lines = translate_label(instructions.len() as i128, labl.to_owned(), namespace, *current_space)?;
                let clines = lines >> 1;
                if clines > 0b01_11111_11111_i32 {
                    return Err(OptimizerError::JumpTooFar(self.clone(), instructions.len(), clines, 2047))
                } else if clines < -2048 {
                    return Err(OptimizerError::JumpTooFar(self.clone(), instructions.len(), clines, -2048))
                }
                instructions.push(Instruction::Beq(reg1.to_owned(), reg2.to_owned(), lines));
            },
            MacroInstr::Bne(reg1, reg2, labl) => {
                let lines = translate_label(instructions.len() as i128, labl.to_owned(), namespace, *current_space)?;
                let clines = lines >> 1;
                if clines > 0b01_11111_11111_i32 {
                    return Err(OptimizerError::JumpTooFar(self.clone(), instructions.len(), clines, 2047))
                } else if clines < -2048 {
                    return Err(OptimizerError::JumpTooFar(self.clone(), instructions.len(), clines, -2048))
                }
                instructions.push(Instruction::Bne(reg1.to_owned(), reg2.to_owned(), lines));
            },
            MacroInstr::Blt(reg1, reg2, labl) => {
                let lines = translate_label(instructions.len() as i128, labl.to_owned(), namespace, *current_space)?;
                let clines = lines >> 1;
                if clines > 0b01_11111_11111_i32 {
                    return Err(OptimizerError::JumpTooFar(self.clone(), instructions.len(), clines, 2047))
                } else if clines < -2048 {
                    return Err(OptimizerError::JumpTooFar(self.clone(), instructions.len(), clines, -2048))
                }
                instructions.push(Instruction::Blt(reg1.to_owned(), reg2.to_owned(), lines));
            },
            MacroInstr::Bltu(reg1, reg2, labl) => {
                let lines = translate_label(instructions.len() as i128, labl.to_owned(), namespace, *current_space)?;
                let clines = lines >> 1;
                if clines > 0b01_11111_11111_i32 {
                    return Err(OptimizerError::JumpTooFar(self.clone(), instructions.len(), clines, 2047))
                } else if clines < -2048 {
                    return Err(OptimizerError::JumpTooFar(self.clone(), instructions.len(), clines, -2048))
                }
                instructions.push(Instruction::Bltu(reg1.to_owned(), reg2.to_owned(), lines));
            },
            MacroInstr::Bge(reg1, reg2, labl) => {
                let lines = translate_label(instructions.len() as i128, labl.to_owned(), namespace, *current_space)?;
                let clines = lines >> 1;
                if clines > 0b01_11111_11111_i32 {
                    return Err(OptimizerError::JumpTooFar(self.clone(), instructions.len(), clines, 2047))
                } else if clines < -2048 {
                    return Err(OptimizerError::JumpTooFar(self.clone(), instructions.len(), clines, -2048))
                }
                instructions.push(Instruction::Bge(reg1.to_owned(), reg2.to_owned(), lines));
            },
            MacroInstr::Bgeu(reg1, reg2, labl) => {
                let lines = translate_label(instructions.len() as i128, labl.to_owned(), namespace, *current_space)?;
                let clines = lines >> 1;
                if clines > 0b01_11111_11111_i32 {
                    return Err(OptimizerError::JumpTooFar(self.clone(), instructions.len(), clines, 2047))
                } else if clines < -2048 {
                    return Err(OptimizerError::JumpTooFar(self.clone(), instructions.len(), clines, -2048))
                }
                instructions.push(Instruction::Bgeu(reg1.to_owned(), reg2.to_owned(), lines));
            },

            MacroInstr::Jal(reg, labl) => {
                let lines = translate_label(instructions.len() as i128, labl.to_owned(), namespace, *current_space)?;
                let clines = lines >> 1;
                if clines > 524287 {
                    return Err(OptimizerError::JumpTooFar(self.clone(), instructions.len(), clines, 524287))
                } else if clines < -524288 {
                    return Err(OptimizerError::JumpTooFar(self.clone(), instructions.len(), clines, -524288))
                }
                instructions.push(Instruction::Jal(reg.to_owned(), lines));
            },
            MacroInstr::Jalr(reg1, reg2, labl, part) => {
                let mut lines = translate_label(instructions.len() as i128, labl.to_owned(), namespace, *current_space)?;
                handle_part(&mut lines, part);
                instructions.push(Instruction::Jalr(reg1.to_owned(), reg2.to_owned(), lines));
            },

            MacroInstr::Lui(reg, labl, part) => {
                let mut lines = translate_label(instructions.len() as i128, labl.to_owned(), namespace, *current_space)?;
                handle_part(&mut lines, part);
                instructions.push(Instruction::Lui(reg.to_owned(), lines));
            },
            MacroInstr::Auipc(reg, labl) => {
                let mut lines = translate_label(instructions.len() as i128, labl.to_owned(), namespace, *current_space)?;
                handle_part(&mut lines, &Part::Upper);
                instructions.push(Instruction::Auipc(reg.to_owned(), lines));
            },

            MacroInstr::LbLabl(reg1, reg2, labl, part) => {
                let mut lines = translate_label(instructions.len() as i128, labl.to_owned(), namespace, *current_space)?;
                handle_part(&mut lines, part);
                instructions.push(Instruction::Lb(reg1.to_owned(), reg2.to_owned(), lines));
            },
            MacroInstr::LhLabl(reg1, reg2, labl, part) => {
                let mut lines = translate_label(instructions.len() as i128, labl.to_owned(), namespace, *current_space)?;
                handle_part(&mut lines, part);
                instructions.push(Instruction::Lh(reg1.to_owned(), reg2.to_owned(), lines));
            },
            MacroInstr::LwLabl(reg1, reg2, labl, part) => {
                let mut lines = translate_label(instructions.len() as i128, labl.to_owned(), namespace, *current_space)?;
                handle_part(&mut lines, part);
                instructions.push(Instruction::Lw(reg1.to_owned(), reg2.to_owned(), lines));
            },

            MacroInstr::LbuLabl(reg1, reg2, labl) => {
                let mut lines = translate_label(instructions.len() as i128, labl.to_owned(), namespace, *current_space)?;
                handle_part(&mut lines, &Part::Lower);
                instructions.push(Instruction::Lbu(reg1.to_owned(), reg2.to_owned(), lines));
            },
            MacroInstr::LhuLabl(reg1, reg2, labl) => {
                let mut lines = translate_label(instructions.len() as i128, labl.to_owned(), namespace, *current_space)?;
                handle_part(&mut lines, &Part::Lower);
                instructions.push(Instruction::Lhu(reg1.to_owned(), reg2.to_owned(), lines));
            },

            MacroInstr::SbLabl(reg1, reg2, labl) => {
                let mut lines = translate_label(instructions.len() as i128, labl.to_owned(), namespace, *current_space)?;
                handle_part(&mut lines, &Part::Lower);
                instructions.push(Instruction::Sb(reg1.to_owned(), reg2.to_owned(), lines));
            },
            MacroInstr::ShLabl(reg1, reg2, labl) => {
                let mut lines = translate_label(instructions.len() as i128, labl.to_owned(), namespace, *current_space)?;
                handle_part(&mut lines, &Part::Lower);
                instructions.push(Instruction::Sh(reg1.to_owned(), reg2.to_owned(), lines));
            },
            MacroInstr::SwLabl(reg1, reg2, labl) => {
                let mut lines = translate_label(instructions.len() as i128, labl.to_owned(), namespace, *current_space)?;
                handle_part(&mut lines, &Part::Lower);
                instructions.push(Instruction::Sw(reg1.to_owned(), reg2.to_owned(), lines));
            },

            op => return Err(OptimizerError::LabelSubNotRequiredFor(op.clone())),
        };
        Ok(())
    }
}

fn translate_label(current_line: i128, label: smartstring::alias::String, namespaces: &mut Namespaces, current_space: usize) -> Result<i32, OptimizerError> {
    if let Some(label_elem) = namespaces.get_label(label.clone(), Some(current_space)) {
        // should always work
        if *label_elem.get_type() == LabelType::Address {
            return Ok(<i128 as TryInto<i32>>::try_into((*label_elem.get_def() * 4) - (current_line * 4))?);
        }
        if *label_elem.get_type() == LabelType::Data {
            #[cfg(feature = "raw_nop")] 
                return Ok(<i128 as TryInto<i32>>::try_into((*label_elem.get_def() - 12) - (current_line * 4))?);
            #[cfg(not(feature = "raw_nop"))] {
                let current_linex4 = current_line * 4;
                if *label_elem.get_def() > current_linex4 {
                    return Ok(<i128 as TryInto<i32>>::try_into(*label_elem.get_def() - current_linex4)?);
                } else {
                    return Ok(<i128 as TryInto<i32>>::try_into((*label_elem.get_def() - 4) - current_linex4)?);
                }
            }
        }
    }
    Err(OptimizerError::LabelNonExistent(label))
}

fn cond_add_acc_label(namespaces: &mut Namespaces, accumulator: i128, label: &smartstring::alias::String, space: usize) -> Result<(), OptimizerError> {
    if accumulator != 0 {
        match namespaces.get_label(label.clone(), Some(space)) {
            Some(lablel) => lablel.add_def(accumulator),
            None => return Err(OptimizerError::LabelNonExistent(label.clone())),
        }
    }
    Ok(())
}

fn nop_insertion(code: &mut AssemblyCodeNamespaces) -> Result<(), OptimizerError> {
    
    let mut working_set: LimitedQueue = LimitedQueue::new_sized(3);
    
    let mut accumulator: i128 = 0;
    let mut space = 0;
    let mut pointer = 0;

    // At maximum every instruction requires 3 nop operations
    // to run without hazards. To lower the number of allocations
    // thus increasing performance, trying to allocate these elements
    //code.1.reserve(code.1.len() * 3);

    loop {
        let operation = code.get_text_refmut().get(pointer).cloned();
        match operation {
            Some(opera) => {
                match &opera {
                    Operation::LablInstr(label, instr) => {
                        let reg_dep = RegActType::from(&opera);
                        let nop_insert = working_set.compare_and_insert(reg_dep);
                        if let Some(inserts) = nop_insert {
                            let instr_num = 4 - inserts;
                            debug!("Inserted {} nop's at {pointer}", instr_num - 1);
                            for _ in 1..instr_num {
                                code.get_text_refmut().insert(pointer, Operation::Instr(Instruction::Addi(Reg::G0, Reg::G0, 0)));
                            }
                            pointer += (instr_num - 1) as usize;
                            accumulator += (instr_num - 1) as i128;
                        }
                        cond_add_acc_label(code.get_labels_refmut(), accumulator, label, space)?;
                        match instr {
                            Instruction::Beq(_, _, _) |
                            Instruction::Bne(_, _, _) |
                            Instruction::Blt(_, _, _) |
                            Instruction::Bltu(_, _, _) |
                            Instruction::Bge(_, _, _) |
                            Instruction::Bgeu(_, _, _) |
                            Instruction::Jal(_, _) |
                            Instruction::Jalr(_, _, _) => working_set.flush(),
                            _ => (),
                        }
                    },
                    Operation::LablMacro(label, instr) => {
                        let reg_dep = RegActType::from(&opera);
                        let nop_insert = working_set.compare_and_insert(reg_dep);
                        if let Some(inserts) = nop_insert {
                            let instr_num = 4 - inserts;
                            debug!("Inserted {} nop's at {pointer}", instr_num - 1);
                            for _ in 1..instr_num {
                                code.get_text_refmut().insert(pointer, Operation::Instr(Instruction::Addi(Reg::G0, Reg::G0, 0)));
                            }
                            pointer += (instr_num - 1) as usize;
                            accumulator += (instr_num - 1) as i128;
                        }
                        cond_add_acc_label(code.get_labels_refmut(), accumulator, label, space)?;
                        match instr {
                            MacroInstr::Beq(_, _, _) |
                            MacroInstr::Bne(_, _, _) |
                            MacroInstr::Blt(_, _, _) |
                            MacroInstr::Bltu(_, _, _) |
                            MacroInstr::Bge(_, _, _) |
                            MacroInstr::Bgeu(_, _, _) |
                            MacroInstr::Jal(_, _) |
                            MacroInstr::Jalr(_, _, _, _) => working_set.flush(),
                            _ => (),
                        }
                    },
                    Operation::Labl(label) => {
                        cond_add_acc_label(code.get_labels_refmut(), accumulator, label, space)?;
                    },
                    Operation::Instr(instr) => {
                        let reg_dep = RegActType::from(&opera);
                        let nop_insert = working_set.compare_and_insert(reg_dep);
                        if let Some(inserts) = nop_insert {
                            let instr_num = 4 - inserts;
                            debug!("Inserted {} nop's at {pointer}", instr_num - 1);
                            for _ in 1..instr_num {
                                code.get_text_refmut().insert(pointer, Operation::Instr(Instruction::Addi(Reg::G0, Reg::G0, 0)));
                            }
                            pointer += (instr_num - 1) as usize;
                            accumulator += (instr_num - 1) as i128;
                        }
                        match instr {
                            Instruction::Beq(_, _, _) |
                            Instruction::Bne(_, _, _) |
                            Instruction::Blt(_, _, _) |
                            Instruction::Bltu(_, _, _) |
                            Instruction::Bge(_, _, _) |
                            Instruction::Bgeu(_, _, _) |
                            Instruction::Jal(_, _) |
                            Instruction::Jalr(_, _, _) => working_set.flush(),
                            _ => (),
                        }
                    },
                    Operation::Macro(instr) => {
                        let reg_dep = RegActType::from(&opera);
                        let nop_insert = working_set.compare_and_insert(reg_dep);
                        if let Some(inserts) = nop_insert {
                            let instr_num = 4 - inserts;
                            debug!("Inserted {} nop's at {pointer}", instr_num - 1);
                            for _ in 1..instr_num {
                                code.get_text_refmut().insert(pointer, Operation::Instr(Instruction::Addi(Reg::G0, Reg::G0, 0)));
                            }
                            pointer += (instr_num - 1) as usize;
                            accumulator += (instr_num - 1) as i128;
                        }
                        match instr {
                            MacroInstr::Beq(_, _, _) |
                            MacroInstr::Bne(_, _, _) |
                            MacroInstr::Blt(_, _, _) |
                            MacroInstr::Bltu(_, _, _) |
                            MacroInstr::Bge(_, _, _) |
                            MacroInstr::Bgeu(_, _, _) |
                            MacroInstr::Jal(_, _) |
                            MacroInstr::Jalr(_, _, _, _) => working_set.flush(),
                            _ => (),
                        }
                    },
                    Operation::Namespace(ns) => {
                        space = *ns;
                    },
                }
                pointer += 1;
            },
            None => break,
        }
    }
    Ok(())
}

impl TryFrom<AssemblyCodeNamespaces> for TranslatableCode {
    type Error = OptimizerError;

    fn try_from(mut code: AssemblyCodeNamespaces) -> Result<Self, Self::Error> {
        let mut namespace: usize = 0;

        let (labels, operations, data) = code.get_all_refmut();

        for data_obj in data.iter_mut() {
            match data_obj {
                MemData::Bytes(data_vec, not_containing_labels) => {
                    if *not_containing_labels {
                        continue
                    }

                    // modify vec in place, nice performance
                    let data_slice = data_vec.as_mut_slice();

                    for index in 0..data_slice.len() {
                        if let ByteData::String(label) = &data_slice[index] {
                            match labels.get_label(label.clone(), Some(namespace)) {
                                Some(labelel) => {
                                    let mut line = *labelel.get_def();
                                    if *labelel.get_type() == LabelType::Address {
                                        line *= 4;
                                    }
                                    let label_def = line & (2_i128.pow(9) - 1);
                                    debug!("Label ref '{label}' of byte data {:?} substituted to {label_def}", data_slice);
                                    data_slice[index] = ByteData::Byte(label_def.try_into().unwrap());
                                },
                                None => return Err(OptimizerError::LabelNonExistent(label.clone())),
                            }
                        }
                    }
                },
                MemData::Halfs(data_vec) => {
                    let data_slice = data_vec.as_mut_slice();

                    for index in 0..data_slice.len() {
                        if let HalfData::String(label) = &data_slice[index] {
                            match labels.get_label(label.clone(), Some(namespace)) {
                                Some(labelel) => {
                                    let mut line = *labelel.get_def();
                                    if *labelel.get_type() == LabelType::Address {
                                        line *= 4;
                                    }
                                    let label_def = line & (2_i128.pow(17) - 1);
                                    debug!("{label} of {:?} substituted label ref to {label_def}", data_slice);
                                    data_slice[index] = HalfData::Half(label_def.try_into().unwrap());
                                },
                                None => return Err(OptimizerError::LabelNonExistent(label.clone())),
                            }
                        }
                    }
                },
                MemData::Words(data_vec) => {
                    let data_slice = data_vec.as_mut_slice();

                    for index in 0..data_slice.len() {
                        if let WordData::String(label) = &data_slice[index] {
                            match labels.get_label(label.clone(), Some(namespace)) {
                                Some(labelel) => {
                                    let mut line = *labelel.get_def();
                                    if *labelel.get_type() == LabelType::Address {
                                        line *= 4;
                                    }
                                    let label_def = line & (2_i128.pow(33) - 1);
                                    debug!("{label} of {:?} substituted label ref to {label_def}", data_slice);
                                    data_slice[index] = WordData::Word(label_def.try_into().unwrap());
                                },
                                None => return Err(OptimizerError::LabelNonExistent(label.clone())),
                            }
                        }
                    }
                },
                MemData::DWords(data_vec) => {
                    let data_slice = data_vec.as_mut_slice();

                    for index in 0..data_slice.len() {
                        if let DWordData::String(label) = &data_slice[index] {
                            match labels.get_label(label.clone(), Some(namespace)) {
                                Some(labelel) => {
                                    let mut line = *labelel.get_def();
                                    if *labelel.get_type() == LabelType::Address {
                                        line *= 4;
                                    }
                                    let label_def = line & (2_i128.pow(65) - 1);
                                    debug!("{label} of {:?} substituted label ref to {label_def}", data_slice);
                                    data_slice[index] = DWordData::DWord(label_def);
                                },
                                None => return Err(OptimizerError::LabelNonExistent(label.clone())),
                            }
                        }
                    }
                },
                MemData::Namespace(space) => namespace = *space,
            }
        }

        let mut translate_code = TranslatableCode::new_with_data(data.to_vec());
        namespace = 0;

        for operation in operations.iter() {
            match operation {
                Operation::Namespace(space) => namespace = *space,
                Operation::Macro(instr) | Operation::LablMacro(_, instr) => {
                    let instructions = translate_code.get_text_refmut();
                    instr.translate(labels, &namespace, instructions)?;
                    debug!("Substituted label reference of '{operation}' to '{}'", instructions.last().unwrap());
                },
                Operation::Instr(instr) | Operation::LablInstr(_, instr) => {
                    translate_code.get_text_refmut().push(instr.to_owned());
                }
                Operation::Labl(_) => (),
            };
        }

        debug!("Finished optimization step");

        Ok(translate_code)
    }
}

pub fn optimize(mut code: AssemblyCodeNamespaces, no_nop_insert: bool) -> Result<TranslatableCode, OptimizerError> {
    if !no_nop_insert {
        nop_insertion(&mut code)?;
    } else {
        debug!("Nop insertion has been omitted due to flag 'no-nop-insertion'!");
    }
    code.try_into()
}

// TODO: Tests here & more test cases
#[cfg(test)]
mod tests {
    use super::*;
    use crate::common::{LabelElem, LabelRecog, LabelType};

    #[test]
    #[cfg(feature = "raw_nop")]
    fn test_reg_act_type_raw_hazard() {
        let last = RegActType::WriteRead(Reg::G15, Reg::G10);
        let first = RegActType::Write(Reg::G10);
        let second = RegActType::WriteRead2(Reg::G15, Reg::G10, Reg::G20);

        assert_eq!(last.is_hazard_before(&first), true);
        assert_eq!(second.is_hazard_before(&first), true);
        assert_eq!(last.is_hazard_before(&second), false);
    }

    #[test]
    #[cfg(feature = "mem_load_nop")]
    fn test_reg_act_type_mem_hazard() {
        let last = RegActType::Store(Reg::G15, Reg::G10);
        let first = RegActType::Load(Reg::G15, Reg::G10);
        let second = RegActType::Load(Reg::G15, Reg::G10);

        assert_eq!(last.is_hazard_before(&first), true);
        assert_eq!(second.is_hazard_before(&first), false);
        assert_eq!(last.is_hazard_before(&second), true);
    }

    #[test]
    fn test_reg_act_type_load_use_hazard() {
        let last = RegActType::WriteRead2(Reg::G15, Reg::G10, Reg::G15);
        let first = RegActType::Load(Reg::G15, Reg::G10);
        let second = RegActType::WriteRead(Reg::G15, Reg::G15);

        assert_eq!(last.is_hazard_before(&first), true);
        assert_eq!(second.is_hazard_before(&first), true);
    }

    #[test]
    fn test_reg0_no_hazard() {
        let last = RegActType::WriteRead2(Reg::G0, Reg::G0, Reg::G0);
        let first = RegActType::Load(Reg::G0, Reg::G0);
        let second = RegActType::WriteRead(Reg::G0, Reg::G0);

        assert_eq!(last.is_hazard_before(&first), false);
        assert_eq!(second.is_hazard_before(&first), false);
        assert_eq!(last.is_hazard_before(&second), false);
    }

    #[test]
    fn test_limited_queue() {
        let mut queue = LimitedQueue::new_sized(3);

        #[cfg(feature = "raw_nop")]
        {
            // Full of raw hazards
            let reg_act_vec = Vec::from([
                RegActType::Write(Reg::G10),
                RegActType::WriteRead2(Reg::G15, Reg::G10, Reg::G20),
                RegActType::WriteRead(Reg::G17, Reg::G10),
                RegActType::WriteRead2(Reg::G16, Reg::G15, Reg::G10),
                RegActType::WriteRead(Reg::G10, Reg::G15)
            ]);
    
            for (counter, reg) in reg_act_vec.iter().enumerate() {
                let nop_inserts = queue.compare_and_insert(reg.clone());
                match counter {
                    0 => assert_eq!(nop_inserts, None),
                    1 => assert_eq!(nop_inserts, Some(0)),
                    2 => assert_eq!(nop_inserts, None),
                    3 => assert_eq!(nop_inserts, Some(1)),
                    4 => assert_eq!(nop_inserts, None),
                    _ => (),
                }
            }
        }

        // No hazards
        let reg_act_vec2 = Vec::from([
            RegActType::Write(Reg::G10),
            RegActType::WriteRead(Reg::G10, Reg::G12),
            RegActType::WriteRead(Reg::G10, Reg::G13)
        ]);

        for (counter, reg) in reg_act_vec2.iter().enumerate() {
            let nop_inserts = queue.compare_and_insert(reg.clone());
            match counter {
                0 => assert_eq!(nop_inserts, None),
                1 => assert_eq!(nop_inserts, None),
                2 => assert_eq!(nop_inserts, None),
                _ => (),
            }
        }
    }

    #[test]
    fn test_simple_nop_insertion() {
        let mut assembly_code: AssemblyCodeNamespaces = AssemblyCode::new(Namespaces::new());

        let namespace = assembly_code.get_labels_refmut();
        let _ = namespace.insert_recog(LabelRecog::new());

        let operation_vec = assembly_code.get_text_refmut();
        operation_vec.push(Operation::Namespace(0));
        operation_vec.push(Operation::Instr(Instruction::Add(Reg::G0, Reg::G1, Reg::G12)));
        operation_vec.push(Operation::Instr(Instruction::Or(Reg::G2, Reg::G12, Reg::G12)));
        operation_vec.push(Operation::Instr(Instruction::Sub(Reg::G2, Reg::G12, Reg::G2)));
        operation_vec.push(Operation::Instr(Instruction::Add(Reg::G15, Reg::G1, Reg::G12)));
        operation_vec.push(Operation::Instr(Instruction::Or(Reg::G2, Reg::G12, Reg::G12)));
        operation_vec.push(Operation::Instr(Instruction::Sub(Reg::G2, Reg::G12, Reg::G15)));

        let _ = nop_insertion(&mut assembly_code);

        // ##################################################################################

        let mut assembly_code_ver: AssemblyCodeNamespaces = AssemblyCode::new(Namespaces::new());

        let namespace_ver = assembly_code_ver.get_labels_refmut();
        let _ = namespace_ver.insert_recog(LabelRecog::new());

        #[cfg(feature = "raw_nop")]
        let operation_vec_ver = Vec::from([
            Operation::Namespace(0),
            Operation::Instr(Instruction::Add(Reg::G0, Reg::G1, Reg::G12)),
            Operation::Instr(Instruction::Or(Reg::G2, Reg::G12, Reg::G12)),
            Operation::Instr(Instruction::Addi(Reg::G0, Reg::G0, 0)),
            Operation::Instr(Instruction::Addi(Reg::G0, Reg::G0, 0)),
            Operation::Instr(Instruction::Addi(Reg::G0, Reg::G0, 0)),
            Operation::Instr(Instruction::Sub(Reg::G2, Reg::G12, Reg::G2)),
            Operation::Instr(Instruction::Add(Reg::G15, Reg::G1, Reg::G12)),
            Operation::Instr(Instruction::Or(Reg::G2, Reg::G12, Reg::G12)),
            Operation::Instr(Instruction::Addi(Reg::G0, Reg::G0, 0)),
            Operation::Instr(Instruction::Addi(Reg::G0, Reg::G0, 0)),
            Operation::Instr(Instruction::Sub(Reg::G2, Reg::G12, Reg::G15))
        ]);

        #[cfg(not(feature = "raw_nop"))]
        let operation_vec_ver = Vec::from([
            Operation::Namespace(0),
            Operation::Instr(Instruction::Add(Reg::G0, Reg::G1, Reg::G12)),
            Operation::Instr(Instruction::Or(Reg::G2, Reg::G12, Reg::G12)),
            Operation::Instr(Instruction::Sub(Reg::G2, Reg::G12, Reg::G2)),
            Operation::Instr(Instruction::Add(Reg::G15, Reg::G1, Reg::G12)),
            Operation::Instr(Instruction::Or(Reg::G2, Reg::G12, Reg::G12)),
            Operation::Instr(Instruction::Sub(Reg::G2, Reg::G12, Reg::G15))
        ]);

        assembly_code_ver.get_text_refmut().extend(operation_vec_ver);

        assert_eq!(assembly_code, assembly_code_ver);
    }

    #[test]
    fn test_complex_nop_insertion() {
        let mut assembly_code: AssemblyCodeNamespaces = AssemblyCode::new(Namespaces::new());

        let mut label_recog_1 = LabelRecog::new();
        let mut label_recog_2 = LabelRecog::new();
        let namespace = assembly_code.get_labels_refmut();

        let mut label = LabelElem::new_refd("START".into());
        label.set_type(LabelType::Address);
        label.set_scope(true);
        label.set_def(5);
        let _ = label_recog_1.insert_label(label);

        let _ = namespace.insert_recog(label_recog_1);

        label = LabelElem::new_refd("END".into());
        label.set_type(LabelType::Address);
        label.set_scope(true);
        label.set_def(3);
        let _ = label_recog_2.insert_label(label);

        label_recog_2.add_offset(6, LabelType::Address);

        let _ = namespace.insert_recog(label_recog_2);

        let operation_vec = assembly_code.get_text_refmut();
        operation_vec.push(Operation::Namespace(0));
        operation_vec.push(Operation::Instr(Instruction::Add(Reg::G0, Reg::G1, Reg::G12)));
        operation_vec.push(Operation::Instr(Instruction::Or(Reg::G2, Reg::G12, Reg::G12)));
        operation_vec.push(Operation::Instr(Instruction::Sub(Reg::G2, Reg::G12, Reg::G2)));
        operation_vec.push(Operation::Instr(Instruction::Add(Reg::G15, Reg::G1, Reg::G12)));
        operation_vec.push(Operation::Macro(MacroInstr::Jal(Reg::G0, "END".into())));
        operation_vec.push(Operation::LablInstr("START".into(), Instruction::Or(Reg::G2, Reg::G12, Reg::G12)));
        operation_vec.push(Operation::Instr(Instruction::Sub(Reg::G2, Reg::G12, Reg::G15)));
        operation_vec.push(Operation::Namespace(1));
        operation_vec.push(Operation::Instr(Instruction::Add(Reg::G0, Reg::G1, Reg::G12)));
        operation_vec.push(Operation::Instr(Instruction::Or(Reg::G2, Reg::G12, Reg::G12)));
        operation_vec.push(Operation::Instr(Instruction::Sub(Reg::G2, Reg::G12, Reg::G2)));
        operation_vec.push(Operation::LablInstr("END".into(), Instruction::Add(Reg::G15, Reg::G1, Reg::G12)));
        operation_vec.push(Operation::Instr(Instruction::Or(Reg::G2, Reg::G12, Reg::G12)));
        operation_vec.push(Operation::Instr(Instruction::Sub(Reg::G2, Reg::G12, Reg::G15)));
        operation_vec.push(Operation::Macro(MacroInstr::Jal(Reg::G0, "START".into())));

        let _ = nop_insertion(&mut assembly_code);

        // ##################################################################################

        let mut assembly_code_ver: AssemblyCodeNamespaces = AssemblyCode::new(Namespaces::new());

        let mut label_recog_ver1 = LabelRecog::new();
        let mut label_recog_ver2 = LabelRecog::new();
        let namespace_ver = assembly_code_ver.get_labels_refmut();

        let mut label = LabelElem::new_refd("START".into());
        label.set_type(LabelType::Address);
        label.set_scope(true);
        label.set_def(5);
        let _ = label_recog_ver1.insert_label(label);

        let _ = namespace_ver.insert_recog(label_recog_ver1);

        label = LabelElem::new_refd("END".into());
        label.set_type(LabelType::Address);
        label.set_scope(true);
        label.set_def(3);
        let _ = label_recog_ver2.insert_label(label);

        label_recog_ver2.add_offset(6, LabelType::Address);

        let _ = namespace_ver.insert_recog(label_recog_ver2);

        #[cfg(feature = "raw_nop")] {
            let _ = cond_add_acc_label(assembly_code_ver.get_labels_refmut(), 3, &"START".into(), 0);
            let _ = cond_add_acc_label(assembly_code_ver.get_labels_refmut(), 6, &"END".into(), 0);
        }

        #[cfg(feature = "raw_nop")]
        let operation_vec_ver: Vec<Operation> = Vec::from([
            Operation::Namespace(0),
            Operation::Instr(Instruction::Add(Reg::G0, Reg::G1, Reg::G12)),
            Operation::Instr(Instruction::Or(Reg::G2, Reg::G12, Reg::G12)),
            Operation::Instr(Instruction::Addi(Reg::G0, Reg::G0, 0)),
            Operation::Instr(Instruction::Addi(Reg::G0, Reg::G0, 0)),
            Operation::Instr(Instruction::Addi(Reg::G0, Reg::G0, 0)),
            Operation::Instr(Instruction::Sub(Reg::G2, Reg::G12, Reg::G2)),
            Operation::Instr(Instruction::Add(Reg::G15, Reg::G1, Reg::G12)),
            Operation::Macro(MacroInstr::Jal(Reg::G0, "END".into())),
            Operation::LablInstr("START".into(), Instruction::Or(Reg::G2, Reg::G12, Reg::G12)),
            Operation::Instr(Instruction::Sub(Reg::G2, Reg::G12, Reg::G15)),
            Operation::Namespace(1),
            Operation::Instr(Instruction::Add(Reg::G0, Reg::G1, Reg::G12)),
            Operation::Instr(Instruction::Or(Reg::G2, Reg::G12, Reg::G12)),
            Operation::Instr(Instruction::Addi(Reg::G0, Reg::G0, 0)),
            Operation::Instr(Instruction::Addi(Reg::G0, Reg::G0, 0)),
            Operation::Instr(Instruction::Addi(Reg::G0, Reg::G0, 0)),
            Operation::Instr(Instruction::Sub(Reg::G2, Reg::G12, Reg::G2)),
            Operation::LablInstr("END".into(), Instruction::Add(Reg::G15, Reg::G1, Reg::G12)),
            Operation::Instr(Instruction::Or(Reg::G2, Reg::G12, Reg::G12)),
            Operation::Instr(Instruction::Addi(Reg::G0, Reg::G0, 0)),
            Operation::Instr(Instruction::Addi(Reg::G0, Reg::G0, 0)),
            Operation::Instr(Instruction::Sub(Reg::G2, Reg::G12, Reg::G15)),
            Operation::Macro(MacroInstr::Jal(Reg::G0, "START".into())),
        ]);

        #[cfg(not(feature = "raw_nop"))]
        let operation_vec_ver: Vec<Operation> = Vec::from([
            Operation::Namespace(0),
            Operation::Instr(Instruction::Add(Reg::G0, Reg::G1, Reg::G12)),
            Operation::Instr(Instruction::Or(Reg::G2, Reg::G12, Reg::G12)),
            Operation::Instr(Instruction::Sub(Reg::G2, Reg::G12, Reg::G2)),
            Operation::Instr(Instruction::Add(Reg::G15, Reg::G1, Reg::G12)),
            Operation::Macro(MacroInstr::Jal(Reg::G0, "END".into())),
            Operation::LablInstr("START".into(), Instruction::Or(Reg::G2, Reg::G12, Reg::G12)),
            Operation::Instr(Instruction::Sub(Reg::G2, Reg::G12, Reg::G15)),
            Operation::Namespace(1),
            Operation::Instr(Instruction::Add(Reg::G0, Reg::G1, Reg::G12)),
            Operation::Instr(Instruction::Or(Reg::G2, Reg::G12, Reg::G12)),
            Operation::Instr(Instruction::Sub(Reg::G2, Reg::G12, Reg::G2)),
            Operation::LablInstr("END".into(), Instruction::Add(Reg::G15, Reg::G1, Reg::G12)),
            Operation::Instr(Instruction::Or(Reg::G2, Reg::G12, Reg::G12)),
            Operation::Instr(Instruction::Sub(Reg::G2, Reg::G12, Reg::G15)),
            Operation::Macro(MacroInstr::Jal(Reg::G0, "START".into())),
        ]);

        assembly_code_ver.get_text_refmut().extend(operation_vec_ver);

        assert_eq!(assembly_code, assembly_code_ver);
    }

    #[test]
    fn test_label_substitution() {
        let mut assembly_code: AssemblyCodeNamespaces = AssemblyCode::new(Namespaces::new());

        let namespace_ver = assembly_code.get_labels_refmut();
        let mut label_recog_ver = LabelRecog::new();

        let mut label = LabelElem::new_refd("_GTLOOP".into());
        label.set_type(LabelType::Address);
        label.set_def(7);
        let _ = label_recog_ver.insert_label(label);

        let mut label = LabelElem::new_refd("_GTSHIFT".into());
        label.set_type(LabelType::Address);
        label.set_def(12);
        let _ = label_recog_ver.insert_label(label);

        let mut label = LabelElem::new_refd("_LTLOOP".into());
        label.set_type(LabelType::Address);
        label.set_def(15);
        let _ = label_recog_ver.insert_label(label);

        let mut label = LabelElem::new_refd("_LTSHIFT".into());
        label.set_type(LabelType::Address);
        label.set_def(20);
        let _ = label_recog_ver.insert_label(label);

        let _ = namespace_ver.insert_recog(label_recog_ver);

        let operation_vec = assembly_code.get_text_refmut();
        operation_vec.push(Operation::Namespace(0));
        operation_vec.push(Operation::Instr(Instruction::Addi(Reg::G17, Reg::G0, 0)));
        operation_vec.push(Operation::Instr(Instruction::Addi(Reg::G16, Reg::G0, 1)));
        operation_vec.push(Operation::Instr(Instruction::Addi(Reg::G12, Reg::G10, 0)));
        operation_vec.push(Operation::Instr(Instruction::Addi(Reg::G13, Reg::G11, 0)));
        operation_vec.push(Operation::Instr(Instruction::Addi(Reg::G10, Reg::G0, 0)));
        operation_vec.push(Operation::Instr(Instruction::Addi(Reg::G11, Reg::G0, 0)));

        operation_vec.push(Operation::Macro(MacroInstr::Blt(Reg::G12, Reg::G13, "_LTLOOP".into())));
        operation_vec.push(Operation::LablInstr("_GTLOOP".into(), Instruction::And(Reg::G14, Reg::G16, Reg::G13)));
        operation_vec.push(Operation::Macro(MacroInstr::Beq(Reg::G14, Reg::G0, "_GTSHIFT".into())));
        operation_vec.push(Operation::Instr(Instruction::Sll(Reg::G15, Reg::G12, Reg::G17)));
        operation_vec.push(Operation::Instr(Instruction::Add(Reg::G10, Reg::G10, Reg::G15)));
        operation_vec.push(Operation::Instr(Instruction::Addi(Reg::G17, Reg::G17, 1)));

        operation_vec.push(Operation::LablInstr("_GTSHIFT".into(), Instruction::Slli(Reg::G16, Reg::G16, 1)));
        operation_vec.push(Operation::Macro(MacroInstr::Bge(Reg::G13, Reg::G16, "_GTLOOP".into())));
        operation_vec.push(Operation::Instr(Instruction::Jalr(Reg::G0, Reg::G1, 0)));

        operation_vec.push(Operation::LablInstr("_LTLOOP".into(), Instruction::And(Reg::G14, Reg::G16, Reg::G12)));
        operation_vec.push(Operation::Macro(MacroInstr::Beq(Reg::G14, Reg::G0, "_LTSHIFT".into())));
        operation_vec.push(Operation::Instr(Instruction::Sll(Reg::G15, Reg::G13, Reg::G17)));
        operation_vec.push(Operation::Instr(Instruction::Add(Reg::G10, Reg::G10, Reg::G15)));
        operation_vec.push(Operation::Instr(Instruction::Addi(Reg::G17, Reg::G17, 1)));

        operation_vec.push(Operation::LablInstr("_LTSHIFT".into(), Instruction::Slli(Reg::G16, Reg::G16, 1)));
        operation_vec.push(Operation::Macro(MacroInstr::Bge(Reg::G12, Reg::G16, "_LTLOOP".into())));
        operation_vec.push(Operation::Instr(Instruction::Jalr(Reg::G0, Reg::G1, 0)));

        let mut translatable_code_ver = TranslatableCode::new();

        let instruction_ver: Vec<Instruction> = Vec::from([
            Instruction::Addi(Reg::G17, Reg::G0, 0),
            Instruction::Addi(Reg::G16, Reg::G0, 1),
            Instruction::Addi(Reg::G12, Reg::G10, 0),
            Instruction::Addi(Reg::G13, Reg::G11, 0),
            Instruction::Addi(Reg::G10, Reg::G0, 0),
            Instruction::Addi(Reg::G11, Reg::G0, 0),

            Instruction::Blt(Reg::G12, Reg::G13, 36),
            Instruction::And(Reg::G14, Reg::G16, Reg::G13),
            Instruction::Beq(Reg::G14, Reg::G0, 16),
            Instruction::Sll(Reg::G15, Reg::G12, Reg::G17),
            Instruction::Add(Reg::G10, Reg::G10, Reg::G15),
            Instruction::Addi(Reg::G17, Reg::G17, 1),

            Instruction::Slli(Reg::G16, Reg::G16, 1),
            Instruction::Bge(Reg::G13, Reg::G16, -24),
            Instruction::Jalr(Reg::G0, Reg::G1, 0),

            Instruction::And(Reg::G14, Reg::G16, Reg::G12),
            Instruction::Beq(Reg::G14, Reg::G0, 16),
            Instruction::Sll(Reg::G15, Reg::G13, Reg::G17),
            Instruction::Add(Reg::G10, Reg::G10, Reg::G15),
            Instruction::Addi(Reg::G17, Reg::G17, 1),

            Instruction::Slli(Reg::G16, Reg::G16, 1),
            Instruction::Bge(Reg::G12, Reg::G16, -24),
            Instruction::Jalr(Reg::G0, Reg::G1, 0)
        ]);

        translatable_code_ver.get_text_refmut().extend(instruction_ver);

        assert_eq!(<AssemblyCodeNamespaces as TryInto<TranslatableCode>>::try_into(assembly_code).unwrap(), translatable_code_ver);
    }

    #[test]
    fn test_optimize() {
        let mut assembly_code: AssemblyCodeNamespaces = AssemblyCode::new(Namespaces::new());

        let namespace_ver = assembly_code.get_labels_refmut();
        let mut label_recog_ver = LabelRecog::new();

        let mut label = LabelElem::new_refd("_JUMP2".into());
        label.set_type(LabelType::Address);
        label.set_def(7);
        let _ = label_recog_ver.insert_label(label);

        let mut label = LabelElem::new_refd("_JUMP".into());
        label.set_type(LabelType::Address);
        label.set_def(9);
        let _ = label_recog_ver.insert_label(label);

        let mut label = LabelElem::new_refd("_GTDIVLOOP".into());
        label.set_type(LabelType::Address);
        label.set_def(10);
        let _ = label_recog_ver.insert_label(label);

        let mut label = LabelElem::new_refd("_LTDIVLOOP".into());
        label.set_type(LabelType::Address);
        label.set_def(20);
        let _ = label_recog_ver.insert_label(label);

        let mut label = LabelElem::new_refd("_Vergleich".into());
        label.set_type(LabelType::Address);
        label.set_def(30);
        let _ = label_recog_ver.insert_label(label);

        let _ = namespace_ver.insert_recog(label_recog_ver);

        let operation_vec = assembly_code.get_text_refmut();
        operation_vec.push(Operation::Namespace(0));
        operation_vec.push(Operation::Instr(Instruction::Addi(Reg::G17, Reg::G0, 1)));
        operation_vec.push(Operation::Instr(Instruction::Addi(Reg::G12, Reg::G10, 0)));
        operation_vec.push(Operation::Instr(Instruction::Addi(Reg::G13, Reg::G11, 0)));
        operation_vec.push(Operation::Instr(Instruction::Addi(Reg::G10, Reg::G0, 0)));
        operation_vec.push(Operation::Instr(Instruction::Addi(Reg::G11, Reg::G0, 0)));

        operation_vec.push(Operation::Macro(MacroInstr::Bne(Reg::G12, Reg::G13, "_JUMP".into())));
        operation_vec.push(Operation::Instr(Instruction::Slli(Reg::G13, Reg::G13, 1)));
        operation_vec.push(Operation::LablInstr("_JUMP2".into(), Instruction::Sub(Reg::G12, Reg::G12, Reg::G13)));
        operation_vec.push(Operation::Instr(Instruction::Add(Reg::G10, Reg::G10, Reg::G17)));
        operation_vec.push(Operation::LablMacro("_JUMP".into(), MacroInstr::Blt(Reg::G12, Reg::G13, "_LTDIVLOOP".into()))); // 9

        operation_vec.push(Operation::LablInstr("_GTDIVLOOP".into(), Instruction::Slli(Reg::G13, Reg::G13, 1)));
        operation_vec.push(Operation::Instr(Instruction::Slli(Reg::G17, Reg::G17, 1)));
        operation_vec.push(Operation::Macro(MacroInstr::Blt(Reg::G13, Reg::G12, "_GTDIVLOOP".into())));
        operation_vec.push(Operation::Instr(Instruction::Srli(Reg::G13, Reg::G13, 1)));
        operation_vec.push(Operation::Instr(Instruction::Srli(Reg::G17, Reg::G17, 1)));
        operation_vec.push(Operation::Instr(Instruction::Sub(Reg::G12, Reg::G12, Reg::G13)));
        operation_vec.push(Operation::Instr(Instruction::Add(Reg::G10, Reg::G10, Reg::G17)));
        operation_vec.push(Operation::Macro(MacroInstr::Bne(Reg::G12, Reg::G0, "_JUMP".into()))); // 17
        operation_vec.push(Operation::Macro(MacroInstr::Beq(Reg::G0, Reg::G0, "_Vergleich".into()))); // 18
        operation_vec.push(Operation::Instr(Instruction::Jalr(Reg::G0, Reg::G1, 0)));

        operation_vec.push(Operation::LablInstr("_LTDIVLOOP".into(), Instruction::Srli(Reg::G13, Reg::G13, 1)));
        operation_vec.push(Operation::Instr(Instruction::Srli(Reg::G17, Reg::G17, 1)));
        operation_vec.push(Operation::Macro(MacroInstr::Blt(Reg::G12, Reg::G13, "_LTDIVLOOP".into())));
        operation_vec.push(Operation::Instr(Instruction::Slli(Reg::G13, Reg::G13, 1)));
        operation_vec.push(Operation::Instr(Instruction::Slli(Reg::G17, Reg::G17, 1)));
        operation_vec.push(Operation::Instr(Instruction::Beq(Reg::G12, Reg::G13, 8)));
        operation_vec.push(Operation::Instr(Instruction::Srli(Reg::G13, Reg::G13, 1)));
        operation_vec.push(Operation::Instr(Instruction::Srli(Reg::G17, Reg::G17, 1)));
        operation_vec.push(Operation::Instr(Instruction::Sub(Reg::G12, Reg::G12, Reg::G13)));
        operation_vec.push(Operation::Instr(Instruction::Add(Reg::G10, Reg::G10, Reg::G17)));
        operation_vec.push(Operation::LablMacro("_Vergleich".into(), MacroInstr::Bne(Reg::G12, Reg::G0, "_JUMP".into()))); // 30
        operation_vec.push(Operation::Instr(Instruction::Jalr(Reg::G0, Reg::G1, 0)));

        let mut translatable_code = TranslatableCode::new();

        #[cfg(feature = "raw_nop")]
        let instruction_ver: Vec<Instruction> = Vec::from([
            Instruction::Addi(Reg::G17, Reg::G0, 1),
            Instruction::Addi(Reg::G12, Reg::G10, 0),
            Instruction::Addi(Reg::G13, Reg::G11, 0),
            Instruction::Addi(Reg::G10, Reg::G0, 0),
            Instruction::Addi(Reg::G11, Reg::G0, 0),

            Instruction::Addi(Reg::G0, Reg::G0, 0),
            Instruction::Bne(Reg::G12, Reg::G13, 36),
            Instruction::Slli(Reg::G13, Reg::G13, 1),
            Instruction::Addi(Reg::G0, Reg::G0, 0),
            Instruction::Addi(Reg::G0, Reg::G0, 0),
            Instruction::Addi(Reg::G0, Reg::G0, 0),
            Instruction::Sub(Reg::G12, Reg::G12, Reg::G13),
            Instruction::Add(Reg::G10, Reg::G10, Reg::G17),
            Instruction::Addi(Reg::G0, Reg::G0, 0),
            Instruction::Addi(Reg::G0, Reg::G0, 0),
            Instruction::Blt(Reg::G12, Reg::G13, 68),

            Instruction::Slli(Reg::G13, Reg::G13, 1), // 16
            Instruction::Slli(Reg::G17, Reg::G17, 1),
            Instruction::Addi(Reg::G0, Reg::G0, 0),
            Instruction::Addi(Reg::G0, Reg::G0, 0),
            Instruction::Blt(Reg::G13, Reg::G12, -16), // 20
            Instruction::Srli(Reg::G13, Reg::G13, 1),
            Instruction::Srli(Reg::G17, Reg::G17, 1),
            Instruction::Addi(Reg::G0, Reg::G0, 0),
            Instruction::Addi(Reg::G0, Reg::G0, 0),
            Instruction::Sub(Reg::G12, Reg::G12, Reg::G13),
            Instruction::Add(Reg::G10, Reg::G10, Reg::G17),
            Instruction::Addi(Reg::G0, Reg::G0, 0),
            Instruction::Addi(Reg::G0, Reg::G0, 0),
            Instruction::Bne(Reg::G12, Reg::G0, -56),
            Instruction::Beq(Reg::G0, Reg::G0, 80), // 20
            Instruction::Jalr(Reg::G0, Reg::G1, 0),

            Instruction::Srli(Reg::G13, Reg::G13, 1), // THIS 31
            Instruction::Srli(Reg::G17, Reg::G17, 1),
            Instruction::Addi(Reg::G0, Reg::G0, 0),
            Instruction::Addi(Reg::G0, Reg::G0, 0),
            Instruction::Blt(Reg::G12, Reg::G13, -16),
            Instruction::Slli(Reg::G13, Reg::G13, 1),
            Instruction::Slli(Reg::G17, Reg::G17, 1),
            Instruction::Addi(Reg::G0, Reg::G0, 0),
            Instruction::Addi(Reg::G0, Reg::G0, 0),
            Instruction::Beq(Reg::G12, Reg::G13, 8),
            Instruction::Srli(Reg::G13, Reg::G13, 1),
            Instruction::Srli(Reg::G17, Reg::G17, 1),
            Instruction::Addi(Reg::G0, Reg::G0, 0),
            Instruction::Addi(Reg::G0, Reg::G0, 0),
            Instruction::Sub(Reg::G12, Reg::G12, Reg::G13),
            Instruction::Add(Reg::G10, Reg::G10, Reg::G17),
            Instruction::Addi(Reg::G0, Reg::G0, 0),
            Instruction::Addi(Reg::G0, Reg::G0, 0),
            Instruction::Bne(Reg::G12, Reg::G0, -140),
            Instruction::Jalr(Reg::G0, Reg::G1, 0)
        ]);

        #[cfg(not(feature = "raw_nop"))]
        let instruction_ver: Vec<Instruction> = Vec::from([
            Instruction::Addi(Reg::G17, Reg::G0, 1),
            Instruction::Addi(Reg::G12, Reg::G10, 0),
            Instruction::Addi(Reg::G13, Reg::G11, 0),
            Instruction::Addi(Reg::G10, Reg::G0, 0),
            Instruction::Addi(Reg::G11, Reg::G0, 0),

            Instruction::Bne(Reg::G12, Reg::G13, 16),
            Instruction::Slli(Reg::G13, Reg::G13, 1),
            Instruction::Sub(Reg::G12, Reg::G12, Reg::G13),
            Instruction::Add(Reg::G10, Reg::G10, Reg::G17),
            Instruction::Blt(Reg::G12, Reg::G13, 44),

            Instruction::Slli(Reg::G13, Reg::G13, 1),
            Instruction::Slli(Reg::G17, Reg::G17, 1),
            Instruction::Blt(Reg::G13, Reg::G12, -8),
            Instruction::Srli(Reg::G13, Reg::G13, 1),
            Instruction::Srli(Reg::G17, Reg::G17, 1),
            Instruction::Sub(Reg::G12, Reg::G12, Reg::G13),
            Instruction::Add(Reg::G10, Reg::G10, Reg::G17),
            Instruction::Bne(Reg::G12, Reg::G0, -32),
            Instruction::Beq(Reg::G0, Reg::G0, 48),
            Instruction::Jalr(Reg::G0, Reg::G1, 0),

            Instruction::Srli(Reg::G13, Reg::G13, 1),
            Instruction::Srli(Reg::G17, Reg::G17, 1),
            Instruction::Blt(Reg::G12, Reg::G13, -8),
            Instruction::Slli(Reg::G13, Reg::G13, 1),
            Instruction::Slli(Reg::G17, Reg::G17, 1),
            Instruction::Beq(Reg::G12, Reg::G13, 8),
            Instruction::Srli(Reg::G13, Reg::G13, 1),
            Instruction::Srli(Reg::G17, Reg::G17, 1),
            Instruction::Sub(Reg::G12, Reg::G12, Reg::G13),
            Instruction::Add(Reg::G10, Reg::G10, Reg::G17),
            Instruction::Bne(Reg::G12, Reg::G0, -84),
            Instruction::Jalr(Reg::G0, Reg::G1, 0)
        ]);

        translatable_code.get_text_refmut().extend(instruction_ver);

        assert_eq!(optimize(assembly_code, false).unwrap(), translatable_code);
    }

    #[test]
    fn test_macro_translate() {
        let cs: usize = 0;
        let mut namespace = Namespaces::new();

        let mut lr = LabelRecog::new();
        lr.crt_def_ref(&"GLOBAL".into(), true, LabelType::Address, 0);
        lr.crt_def_ref(&".LOCAL".into(), false, LabelType::Address, 1);

        let mut instructions = Vec::from([
            Instruction::Addi(Reg::G0, Reg::G0, 0),
            Instruction::Addi(Reg::G0, Reg::G0, 0)
        ]);

        let _ = namespace.insert_recog(lr);

        {
            let test_addi_macros = [
                (MacroInstr::Addi(Reg::G0, Reg::G0, ".LOCAL".into(), Part::Lower), Instruction::Addi(Reg::G0, Reg::G0, -4)),
                (MacroInstr::Addi(Reg::G0, Reg::G0, "GLOBAL".into(), Part::Lower), Instruction::Addi(Reg::G0, Reg::G0, -8))
            ];

            instructions.push(Instruction::Auipc(Reg::G0, 0));

            #[cfg(feature = "raw_nop")] {
                instructions.push(Instruction::Addi(Reg::G0, Reg::G0, 0));
                instructions.push(Instruction::Addi(Reg::G0, Reg::G0, 0));
                instructions.push(Instruction::Addi(Reg::G0, Reg::G0, 0));
            }

            for (test, corr) in test_addi_macros {
                let _ = test.translate(&mut namespace, &cs, &mut instructions);
                #[cfg(feature = "raw_nop")] {
                    assert_eq!(instructions[6], corr);
                    instructions.remove(6);
                }
                #[cfg(not(feature = "raw_nop"))] {
                    assert_eq!(instructions[3], corr);
                    instructions.remove(3);
                }
            }

            #[cfg(feature = "raw_nop")] {
                instructions.remove(5);
                instructions.remove(4);
                instructions.remove(3);
            }

            instructions.remove(2);
        }

        let mut test_macros: Vec<(MacroInstr, Instruction)> = vec![];

        test_macros.push((MacroInstr::Lui(Reg::G21, "GLOBAL".into(), Part::None), Instruction::Lui(Reg::G21, -8)));

        for (test, corr) in test_macros {
            let _ = test.translate(&mut namespace, &cs, &mut instructions);
            assert_eq!(instructions[2], corr);
            instructions.remove(2);
        }
    }
}
