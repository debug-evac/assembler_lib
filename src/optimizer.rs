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
use std::borrow::Cow;
use log::debug;

use crate::{
    common::{Instruction, Operation, MacroInstr, Reg, Part,
        errors::OptimizerError,
    },
    linker::Namespaces,
};

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

impl From<&Operation<'_>> for RegActType {
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
                    Instruction::Sltu(reg1, reg2, reg3) => RegActType::WriteRead2(reg1.clone(), reg2.clone(), reg3.clone()),

                    Instruction::Addi(reg1, reg2, _) |
                    Instruction::Xori(reg1, reg2, _) |
                    Instruction::Ori(reg1, reg2, _) |
                    Instruction::Andi(reg1, reg2, _) |
                    Instruction::Slti(reg1, reg2, _) |
                    Instruction::Sltiu(reg1, reg2, _) |
                    Instruction::Slli(reg1, reg2, _) |
                    Instruction::Srli(reg1, reg2, _) |
                    Instruction::Srai(reg1, reg2, _) |
                    Instruction::Jalr(reg1, reg2, _) => RegActType::WriteRead(reg1.clone(), reg2.clone()),

                    Instruction::Lb(reg1, reg2, _) |
                    Instruction::Lh(reg1, reg2, _) |
                    Instruction::Lw(reg1, reg2, _) |
                    Instruction::Lbu(reg1, reg2, _) |
                    Instruction::Lhu(reg1, reg2, _) => RegActType::Load(reg1.clone(), reg2.clone()),

                    Instruction::Sh(reg1, reg2, _) |
                    Instruction::Sb(reg1, reg2, _) |
                    Instruction::Sw(reg1, reg2, _) => RegActType::Store(reg1.clone(), reg2.clone()),

                    Instruction::Beq(reg1, reg2, _) |
                    Instruction::Bne(reg1, reg2, _) |
                    Instruction::Blt(reg1, reg2, _) |
                    Instruction::Bltu(reg1, reg2, _) |
                    Instruction::Bge(reg1, reg2, _) |
                    Instruction::Bgeu(reg1, reg2, _) => RegActType::Read2(reg1.clone(), reg2.clone()),

                    Instruction::Lui(reg, _) |
                    Instruction::Auipc(reg, _) |
                    Instruction::Jal(reg, _) => RegActType::Write(reg.clone()),
                }
            },
            Operation::Macro(instr) | Operation::LablMacro(_, instr) => {
                match instr {
                    MacroInstr::Beq(reg1, reg2, _) |
                    MacroInstr::Bne(reg1, reg2, _) |
                    MacroInstr::Blt(reg1, reg2, _) |
                    MacroInstr::Bltu(reg1, reg2, _) |
                    MacroInstr::Bge(reg1, reg2, _) |
                    MacroInstr::Bgeu(reg1, reg2, _) => RegActType::Read2(reg1.clone(), reg2.clone()),

                    MacroInstr::Lui(reg, _) |
                    MacroInstr::Auipc(reg, _, _) |
                    MacroInstr::Jal(reg, _) => RegActType::Write(reg.clone()),

                    MacroInstr::Addi(reg1, reg2, _, _) |
                    MacroInstr::Slli(reg1, reg2, _) |
                    MacroInstr::Srli(reg1, reg2, _) |
                    MacroInstr::Srai(reg1, reg2, _) |
                    MacroInstr::Jalr(reg1, reg2, _, _) => RegActType::WriteRead(reg1.clone(), reg2.clone()),

                    MacroInstr::Lb(reg1, reg2, _, _) |
                    MacroInstr::Lh(reg1, reg2, _, _) |
                    MacroInstr::Lw(reg1, reg2, _, _) |
                    MacroInstr::Lbu(reg1, reg2, _) |
                    MacroInstr::Lhu(reg1, reg2, _) => RegActType::Load(reg2.clone(), reg1.clone()),

                    MacroInstr::Sh(reg1, reg2, _, _) |
                    MacroInstr::Sb(reg1, reg2, _, _) |
                    MacroInstr::Sw(reg1, reg2, _, _) => RegActType::Store(reg1.clone(), reg2.clone()),

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

impl MacroInstr {
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
                instructions.push(Instruction::Beq(reg1.to_owned(), reg2.to_owned(), lines));
            },
            MacroInstr::Bne(reg1, reg2, labl) => {
                let lines = translate_label(instructions.len() as i128, labl.to_owned(), namespace, *current_space)?;
                instructions.push(Instruction::Bne(reg1.to_owned(), reg2.to_owned(), lines));
            },
            MacroInstr::Blt(reg1, reg2, labl) => {
                let lines = translate_label(instructions.len() as i128, labl.to_owned(), namespace, *current_space)?;
                instructions.push(Instruction::Blt(reg1.to_owned(), reg2.to_owned(), lines));
            },
            MacroInstr::Bltu(reg1, reg2, labl) => {
                let lines = translate_label(instructions.len() as i128, labl.to_owned(), namespace, *current_space)?;
                instructions.push(Instruction::Bltu(reg1.to_owned(), reg2.to_owned(), lines));
            },
            MacroInstr::Bge(reg1, reg2, labl) => {
                let lines = translate_label(instructions.len() as i128, labl.to_owned(), namespace, *current_space)?;
                instructions.push(Instruction::Bge(reg1.to_owned(), reg2.to_owned(), lines));
            },
            MacroInstr::Bgeu(reg1, reg2, labl) => {
                let lines = translate_label(instructions.len() as i128, labl.to_owned(), namespace, *current_space)?;
                instructions.push(Instruction::Bgeu(reg1.to_owned(), reg2.to_owned(), lines));
            },

            MacroInstr::Jal(reg, labl) => {
                let lines = translate_label(instructions.len() as i128, labl.to_owned(), namespace, *current_space)?;
                instructions.push(Instruction::Jal(reg.to_owned(), lines));
            },
            MacroInstr::Jalr(reg1, reg2, labl, part) => {
                let mut lines = translate_label(instructions.len() as i128, labl.to_owned(), namespace, *current_space)?;
                handle_part(&mut lines, part);
                instructions.push(Instruction::Jalr(reg1.to_owned(), reg2.to_owned(), lines));
            },

            MacroInstr::Lui(reg, labl) => {
                let lines = translate_label(instructions.len() as i128, labl.to_owned(), namespace, *current_space)?;
                instructions.push(Instruction::Lui(reg.to_owned(), lines));
            },
            MacroInstr::Auipc(reg, labl, part) => {
                let mut lines = translate_label(instructions.len() as i128, labl.to_owned(), namespace, *current_space)?;
                handle_part(&mut lines, part);
                instructions.push(Instruction::Auipc(reg.to_owned(), lines));
            },

            MacroInstr::Slli(reg1, reg2, labl) => {
                let lines = translate_label(instructions.len() as i128, labl.to_owned(), namespace, *current_space)?;
                instructions.push(Instruction::Slli(reg1.to_owned(), reg2.to_owned(), lines));
            },
            MacroInstr::Srli(reg1, reg2, labl) => {
                let lines = translate_label(instructions.len() as i128, labl.to_owned(), namespace, *current_space)?;
                instructions.push(Instruction::Srli(reg1.to_owned(), reg2.to_owned(), lines));
            },
            MacroInstr::Srai(reg1, reg2, labl) => {
                let lines = translate_label(instructions.len() as i128, labl.to_owned(), namespace, *current_space)?;
                instructions.push(Instruction::Srai(reg1.to_owned(), reg2.to_owned(), lines));
            },

            MacroInstr::Lb(reg1, reg2, labl, part) => {
                let mut lines = translate_label(instructions.len() as i128, labl.to_owned(), namespace, *current_space)?;
                handle_part(&mut lines, part);
                instructions.push(Instruction::Lb(reg1.to_owned(), reg2.to_owned(), lines));
            },
            MacroInstr::Lh(reg1, reg2, labl, part) => {
                let mut lines = translate_label(instructions.len() as i128, labl.to_owned(), namespace, *current_space)?;
                handle_part(&mut lines, part);
                instructions.push(Instruction::Lh(reg1.to_owned(), reg2.to_owned(), lines));
            },
            MacroInstr::Lw(reg1, reg2, labl, part) => {
                let mut lines = translate_label(instructions.len() as i128, labl.to_owned(), namespace, *current_space)?;
                handle_part(&mut lines, part);
                instructions.push(Instruction::Lw(reg1.to_owned(), reg2.to_owned(), lines));
            },
            MacroInstr::Lbu(reg1, reg2, labl) => {
                let lines = translate_label(instructions.len() as i128, labl.to_owned(), namespace, *current_space)?;
                instructions.push(Instruction::Lbu(reg1.to_owned(), reg2.to_owned(), lines));
            },
            MacroInstr::Lhu(reg1, reg2, labl) => {
                let lines = translate_label(instructions.len() as i128, labl.to_owned(), namespace, *current_space)?;
                instructions.push(Instruction::Lhu(reg1.to_owned(), reg2.to_owned(), lines));
            },

            MacroInstr::Sb(reg1, reg2, labl, part) => {
                let mut lines = translate_label(instructions.len() as i128, labl.to_owned(), namespace, *current_space)?;
                handle_part(&mut lines, part);
                instructions.push(Instruction::Sb(reg1.to_owned(), reg2.to_owned(), lines));
            },
            MacroInstr::Sh(reg1, reg2, labl, part) => {
                let mut lines = translate_label(instructions.len() as i128, labl.to_owned(), namespace, *current_space)?;
                handle_part(&mut lines, part);
                instructions.push(Instruction::Sh(reg1.to_owned(), reg2.to_owned(), lines));
            },
            MacroInstr::Sw(reg1, reg2, labl, part) => {
                let mut lines = translate_label(instructions.len() as i128, labl.to_owned(), namespace, *current_space)?;
                handle_part(&mut lines, part);
                instructions.push(Instruction::Sw(reg1.to_owned(), reg2.to_owned(), lines));
            },

            op => return Err(OptimizerError::LabelSubNotRequiredFor(op.clone())),
        };
        Ok(())
    }
}

fn translate_label(current_line: i128, label: String, namespaces: &mut Namespaces, current_space: usize) -> Result<i32, OptimizerError> {
    match namespaces.get_label(label.clone(), Some(current_space)) {
        Some(label_elem) => {
            // should always work
            Ok(<i128 as TryInto<i32>>::try_into((*label_elem.get_def() * 4) - (current_line * 4)).unwrap())
        },
        None => Err(OptimizerError::LabelNonExistent(label)),
    }
}

fn cond_add_acc_label(namespaces: &mut Namespaces, accumulator: i128, label: Cow<str>, space: usize) -> Result<(), OptimizerError> {
    if accumulator != 0 {
        match label.strip_prefix('.') {
            Some(labell) => {
                match namespaces.get_label(labell.to_string(), Some(space)) {
                    Some(lablel) => lablel.add_def(accumulator),
                    None => return Err(OptimizerError::LabelNonExistent(label.to_string())),
                }
            },
            None => {
                match namespaces.get_label(label.to_string(), Some(space)) {
                    Some(lablel) => lablel.add_def(accumulator),
                    None => return Err(OptimizerError::LabelNonExistent(label.to_string())),
                }
            }
        }
    }
    Ok(())
}

fn nop_insertion(code: &mut (Namespaces, Vec<Operation>)) -> Result<(), OptimizerError> {
    
    let mut working_set: LimitedQueue = LimitedQueue::new_sized(3);
    
    let mut accumulator: i128 = 0;
    let mut space = 0;
    let mut pointer = 0;

    // At maximum every instruction requires 3 nop operations
    // to run without hazards. To lower the number of allocations
    // thus increasing performance, trying to allocate these elements
    //code.1.reserve(code.1.len() * 3);

    loop {
        let operation = code.1.get(pointer).cloned();
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
                                code.1.insert(pointer, Operation::Instr(Instruction::Addi(Reg::G0, Reg::G0, 0)));
                            }
                            pointer += (instr_num - 1) as usize;
                            accumulator += (instr_num - 1) as i128;
                        }
                        cond_add_acc_label(&mut code.0, accumulator, std::borrow::Cow::Borrowed(label), space)?;
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
                                code.1.insert(pointer, Operation::Instr(Instruction::Addi(Reg::G0, Reg::G0, 0)));
                            }
                            pointer += (instr_num - 1) as usize;
                            accumulator += (instr_num - 1) as i128;
                        }
                        cond_add_acc_label(&mut code.0, accumulator, std::borrow::Cow::Borrowed(label), space)?;
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
                        cond_add_acc_label(&mut code.0, accumulator, std::borrow::Cow::Borrowed(label), space)?;
                    },
                    Operation::Instr(instr) => {
                        let reg_dep = RegActType::from(&opera);
                        let nop_insert = working_set.compare_and_insert(reg_dep);
                        if let Some(inserts) = nop_insert {
                            let instr_num = 4 - inserts;
                            debug!("Inserted {} nop's at {pointer}", instr_num - 1);
                            for _ in 1..instr_num {
                                code.1.insert(pointer, Operation::Instr(Instruction::Addi(Reg::G0, Reg::G0, 0)));
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
                                code.1.insert(pointer, Operation::Instr(Instruction::Addi(Reg::G0, Reg::G0, 0)));
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

fn substitute_labels(mut code: (Namespaces, Vec<Operation>)) -> Result<Vec<Instruction>, OptimizerError> {
    let mut instructions: Vec<Instruction> = vec![];
    let mut namespace: usize = 0;

    for operation in code.1.iter() {
        match operation {
            Operation::Namespace(space) => namespace = *space,
            Operation::Macro(instr) | Operation::LablMacro(_, instr) => {
                instr.translate(&mut code.0, &namespace, &mut instructions)?;
                debug!("{:?} substituted label ref to {:?}", operation, instructions.last().unwrap());
            },
            Operation::Instr(instr) | Operation::LablInstr(_, instr) => {
                instructions.push(instr.to_owned());
            }
            Operation::Labl(_) => (),
        };
    }

    debug!("Finished optimization step");

    Ok(instructions)
}

pub fn optimize(mut code: (Namespaces, Vec<Operation>), no_nop_insert: bool) -> Result<Vec<Instruction>, OptimizerError> {
    if !no_nop_insert {
        nop_insertion(&mut code)?;
    } else {
        debug!("Nop insertion has been omitted due to flag 'no-nop-insertion'!");
    }
    substitute_labels(code)
}

// TODO: Tests here & more test cases
#[cfg(test)]
mod tests {
    use super::*;
    use crate::common::{LabelRecog, LabelElem};

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
        let label_recog = LabelRecog::new();

        let mut namespace = Namespaces::new();
        let _ = namespace.insert_recog(label_recog);

        let mut operation_vec: Vec<Operation> = vec![];
        operation_vec.push(Operation::Namespace(0));
        operation_vec.push(Operation::Instr(Instruction::Add(Reg::G0, Reg::G1, Reg::G12)));
        operation_vec.push(Operation::Instr(Instruction::Or(Reg::G2, Reg::G12, Reg::G12)));
        operation_vec.push(Operation::Instr(Instruction::Sub(Reg::G2, Reg::G12, Reg::G2)));
        operation_vec.push(Operation::Instr(Instruction::Add(Reg::G15, Reg::G1, Reg::G12)));
        operation_vec.push(Operation::Instr(Instruction::Or(Reg::G2, Reg::G12, Reg::G12)));
        operation_vec.push(Operation::Instr(Instruction::Sub(Reg::G2, Reg::G12, Reg::G15)));

        let mut code = (namespace, operation_vec);

        let _ = nop_insertion(&mut code);

        // ##################################################################################

        let label_recog_ver = LabelRecog::new();

        let mut namespace_ver = Namespaces::new();
        let _ = namespace_ver.insert_recog(label_recog_ver);

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
            Operation::Instr(Instruction::Or(Reg::G2, Reg::G12, Reg::G12)),
            Operation::Instr(Instruction::Addi(Reg::G0, Reg::G0, 0)),
            Operation::Instr(Instruction::Addi(Reg::G0, Reg::G0, 0)),
            Operation::Instr(Instruction::Sub(Reg::G2, Reg::G12, Reg::G15))
        ]);

        #[cfg(not(feature = "raw_nop"))]
        let operation_vec_ver: Vec<Operation> = Vec::from([
            Operation::Namespace(0),
            Operation::Instr(Instruction::Add(Reg::G0, Reg::G1, Reg::G12)),
            Operation::Instr(Instruction::Or(Reg::G2, Reg::G12, Reg::G12)),
            Operation::Instr(Instruction::Sub(Reg::G2, Reg::G12, Reg::G2)),
            Operation::Instr(Instruction::Add(Reg::G15, Reg::G1, Reg::G12)),
            Operation::Instr(Instruction::Or(Reg::G2, Reg::G12, Reg::G12)),
            Operation::Instr(Instruction::Sub(Reg::G2, Reg::G12, Reg::G15))
        ]);

        assert_eq!(code, (namespace_ver, operation_vec_ver));
    }

    #[test]
    fn test_complex_nop_insertion() {
        let mut label_recog_1 = LabelRecog::new();
        let mut label_recog_2 = LabelRecog::new();
        let mut namespace = Namespaces::new();

        let mut label = LabelElem::new_refd("START".to_string());
        label.set_scope(true);
        label.set_def(5);
        let _ = label_recog_1.insert_label(label);

        let _ = namespace.insert_recog(label_recog_1);

        label = LabelElem::new_refd("END".to_string());
        label.set_scope(true);
        label.set_def(3);
        let _ = label_recog_2.insert_label(label);

        label_recog_2.add_offset(6);

        let _ = namespace.insert_recog(label_recog_2);

        let mut operation_vec: Vec<Operation> = vec![];
        operation_vec.push(Operation::Namespace(0));
        operation_vec.push(Operation::Instr(Instruction::Add(Reg::G0, Reg::G1, Reg::G12)));
        operation_vec.push(Operation::Instr(Instruction::Or(Reg::G2, Reg::G12, Reg::G12)));
        operation_vec.push(Operation::Instr(Instruction::Sub(Reg::G2, Reg::G12, Reg::G2)));
        operation_vec.push(Operation::Instr(Instruction::Add(Reg::G15, Reg::G1, Reg::G12)));
        operation_vec.push(Operation::Macro(MacroInstr::Jal(Reg::G0, "END".to_string())));
        operation_vec.push(Operation::LablInstr(Cow::from("START"), Instruction::Or(Reg::G2, Reg::G12, Reg::G12)));
        operation_vec.push(Operation::Instr(Instruction::Sub(Reg::G2, Reg::G12, Reg::G15)));
        operation_vec.push(Operation::Namespace(1));
        operation_vec.push(Operation::Instr(Instruction::Add(Reg::G0, Reg::G1, Reg::G12)));
        operation_vec.push(Operation::Instr(Instruction::Or(Reg::G2, Reg::G12, Reg::G12)));
        operation_vec.push(Operation::Instr(Instruction::Sub(Reg::G2, Reg::G12, Reg::G2)));
        operation_vec.push(Operation::LablInstr(Cow::from("END"), Instruction::Add(Reg::G15, Reg::G1, Reg::G12)));
        operation_vec.push(Operation::Instr(Instruction::Or(Reg::G2, Reg::G12, Reg::G12)));
        operation_vec.push(Operation::Instr(Instruction::Sub(Reg::G2, Reg::G12, Reg::G15)));
        operation_vec.push(Operation::Macro(MacroInstr::Jal(Reg::G0, "START".to_string())));

        let mut code = (namespace, operation_vec);

        let _ = nop_insertion(&mut code);

        // ##################################################################################

        let mut label_recog_ver1 = LabelRecog::new();
        let mut label_recog_ver2 = LabelRecog::new();
        let mut namespace_ver = Namespaces::new();

        let mut label = LabelElem::new_refd("START".to_string());
        label.set_scope(true);
        label.set_def(5);
        let _ = label_recog_ver1.insert_label(label);

        let _ = namespace_ver.insert_recog(label_recog_ver1);

        label = LabelElem::new_refd("END".to_string());
        label.set_scope(true);
        label.set_def(3);
        let _ = label_recog_ver2.insert_label(label);

        label_recog_ver2.add_offset(6);

        let _ = namespace_ver.insert_recog(label_recog_ver2);

        #[cfg(feature = "raw_nop")] {
            let _ = cond_add_acc_label(&mut namespace_ver, 3, Cow::from("START"), 0);
            let _ = cond_add_acc_label(&mut namespace_ver, 6, Cow::from("END"), 0);
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
            Operation::Macro(MacroInstr::Jal(Reg::G0, "END".to_string())),
            Operation::LablInstr(Cow::from("START"), Instruction::Or(Reg::G2, Reg::G12, Reg::G12)),
            Operation::Instr(Instruction::Sub(Reg::G2, Reg::G12, Reg::G15)),
            Operation::Namespace(1),
            Operation::Instr(Instruction::Add(Reg::G0, Reg::G1, Reg::G12)),
            Operation::Instr(Instruction::Or(Reg::G2, Reg::G12, Reg::G12)),
            Operation::Instr(Instruction::Addi(Reg::G0, Reg::G0, 0)),
            Operation::Instr(Instruction::Addi(Reg::G0, Reg::G0, 0)),
            Operation::Instr(Instruction::Addi(Reg::G0, Reg::G0, 0)),
            Operation::Instr(Instruction::Sub(Reg::G2, Reg::G12, Reg::G2)),
            Operation::LablInstr(Cow::from("END"), Instruction::Add(Reg::G15, Reg::G1, Reg::G12)),
            Operation::Instr(Instruction::Or(Reg::G2, Reg::G12, Reg::G12)),
            Operation::Instr(Instruction::Addi(Reg::G0, Reg::G0, 0)),
            Operation::Instr(Instruction::Addi(Reg::G0, Reg::G0, 0)),
            Operation::Instr(Instruction::Sub(Reg::G2, Reg::G12, Reg::G15)),
            Operation::Macro(MacroInstr::Jal(Reg::G0, "START".to_string())),
        ]);

        #[cfg(not(feature = "raw_nop"))]
        let operation_vec_ver: Vec<Operation> = Vec::from([
            Operation::Namespace(0),
            Operation::Instr(Instruction::Add(Reg::G0, Reg::G1, Reg::G12)),
            Operation::Instr(Instruction::Or(Reg::G2, Reg::G12, Reg::G12)),
            Operation::Instr(Instruction::Sub(Reg::G2, Reg::G12, Reg::G2)),
            Operation::Instr(Instruction::Add(Reg::G15, Reg::G1, Reg::G12)),
            Operation::Macro(MacroInstr::Jal(Reg::G0, "END".to_string())),
            Operation::LablInstr(Cow::from("START"), Instruction::Or(Reg::G2, Reg::G12, Reg::G12)),
            Operation::Instr(Instruction::Sub(Reg::G2, Reg::G12, Reg::G15)),
            Operation::Namespace(1),
            Operation::Instr(Instruction::Add(Reg::G0, Reg::G1, Reg::G12)),
            Operation::Instr(Instruction::Or(Reg::G2, Reg::G12, Reg::G12)),
            Operation::Instr(Instruction::Sub(Reg::G2, Reg::G12, Reg::G2)),
            Operation::LablInstr(Cow::from("END"), Instruction::Add(Reg::G15, Reg::G1, Reg::G12)),
            Operation::Instr(Instruction::Or(Reg::G2, Reg::G12, Reg::G12)),
            Operation::Instr(Instruction::Sub(Reg::G2, Reg::G12, Reg::G15)),
            Operation::Macro(MacroInstr::Jal(Reg::G0, "START".to_string())),
        ]);

        assert_eq!(code, (namespace_ver, operation_vec_ver));
    }

    #[test]
    fn test_label_substitution() {
        let mut namespace_ver = Namespaces::new();
        let mut label_recog_ver = LabelRecog::new();

        let mut label = LabelElem::new_refd("_GTLOOP".to_string());
        label.set_def(7);
        let _ = label_recog_ver.insert_label(label);

        let mut label = LabelElem::new_refd("_GTSHIFT".to_string());
        label.set_def(12);
        let _ = label_recog_ver.insert_label(label);

        let mut label = LabelElem::new_refd("_LTLOOP".to_string());
        label.set_def(15);
        let _ = label_recog_ver.insert_label(label);

        let mut label = LabelElem::new_refd("_LTSHIFT".to_string());
        label.set_def(20);
        let _ = label_recog_ver.insert_label(label);

        let _ = namespace_ver.insert_recog(label_recog_ver);

        let mut operation_vec: Vec<Operation> = vec![];
        operation_vec.push(Operation::Namespace(0));
        operation_vec.push(Operation::Instr(Instruction::Addi(Reg::G17, Reg::G0, 0)));
        operation_vec.push(Operation::Instr(Instruction::Addi(Reg::G16, Reg::G0, 1)));
        operation_vec.push(Operation::Instr(Instruction::Addi(Reg::G12, Reg::G10, 0)));
        operation_vec.push(Operation::Instr(Instruction::Addi(Reg::G13, Reg::G11, 0)));
        operation_vec.push(Operation::Instr(Instruction::Addi(Reg::G10, Reg::G0, 0)));
        operation_vec.push(Operation::Instr(Instruction::Addi(Reg::G11, Reg::G0, 0)));

        operation_vec.push(Operation::Macro(MacroInstr::Blt(Reg::G12, Reg::G13, "_LTLOOP".to_string())));
        operation_vec.push(Operation::LablInstr(Cow::from("_GTLOOP"), Instruction::And(Reg::G14, Reg::G16, Reg::G13)));
        operation_vec.push(Operation::Macro(MacroInstr::Beq(Reg::G14, Reg::G0, "_GTSHIFT".to_string())));
        operation_vec.push(Operation::Instr(Instruction::Sll(Reg::G15, Reg::G12, Reg::G17)));
        operation_vec.push(Operation::Instr(Instruction::Add(Reg::G10, Reg::G10, Reg::G15)));
        operation_vec.push(Operation::Instr(Instruction::Addi(Reg::G17, Reg::G17, 1)));

        operation_vec.push(Operation::LablInstr(Cow::from("_GTSHIFT"), Instruction::Slli(Reg::G16, Reg::G16, 1)));
        operation_vec.push(Operation::Macro(MacroInstr::Bge(Reg::G13, Reg::G16, "_GTLOOP".to_string())));
        operation_vec.push(Operation::Instr(Instruction::Jalr(Reg::G0, Reg::G1, 0)));

        operation_vec.push(Operation::LablInstr(Cow::from("_LTLOOP"), Instruction::And(Reg::G14, Reg::G16, Reg::G12)));
        operation_vec.push(Operation::Macro(MacroInstr::Beq(Reg::G14, Reg::G0, "_LTSHIFT".to_string())));
        operation_vec.push(Operation::Instr(Instruction::Sll(Reg::G15, Reg::G13, Reg::G17)));
        operation_vec.push(Operation::Instr(Instruction::Add(Reg::G10, Reg::G10, Reg::G15)));
        operation_vec.push(Operation::Instr(Instruction::Addi(Reg::G17, Reg::G17, 1)));

        operation_vec.push(Operation::LablInstr(Cow::from("_LTSHIFT"), Instruction::Slli(Reg::G16, Reg::G16, 1)));
        operation_vec.push(Operation::Macro(MacroInstr::Bge(Reg::G12, Reg::G16, "_LTLOOP".to_string())));
        operation_vec.push(Operation::Instr(Instruction::Jalr(Reg::G0, Reg::G1, 0)));

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

        assert_eq!(substitute_labels((namespace_ver, operation_vec)).unwrap(), instruction_ver);
    }

    #[test]
    fn test_optimize() {
        let mut namespace_ver = Namespaces::new();
        let mut label_recog_ver = LabelRecog::new();

        let mut label = LabelElem::new_refd("_JUMP2".to_string());
        label.set_def(7);
        let _ = label_recog_ver.insert_label(label);

        let mut label = LabelElem::new_refd("_JUMP".to_string());
        label.set_def(9);
        let _ = label_recog_ver.insert_label(label);

        let mut label = LabelElem::new_refd("_GTDIVLOOP".to_string());
        label.set_def(10);
        let _ = label_recog_ver.insert_label(label);

        let mut label = LabelElem::new_refd("_LTDIVLOOP".to_string());
        label.set_def(20);
        let _ = label_recog_ver.insert_label(label);

        let mut label = LabelElem::new_refd("_Vergleich".to_string());
        label.set_def(30);
        let _ = label_recog_ver.insert_label(label);

        let _ = namespace_ver.insert_recog(label_recog_ver);

        let mut operation_vec: Vec<Operation> = vec![];
        operation_vec.push(Operation::Namespace(0));
        operation_vec.push(Operation::Instr(Instruction::Addi(Reg::G17, Reg::G0, 1)));
        operation_vec.push(Operation::Instr(Instruction::Addi(Reg::G12, Reg::G10, 0)));
        operation_vec.push(Operation::Instr(Instruction::Addi(Reg::G13, Reg::G11, 0)));
        operation_vec.push(Operation::Instr(Instruction::Addi(Reg::G10, Reg::G0, 0)));
        operation_vec.push(Operation::Instr(Instruction::Addi(Reg::G11, Reg::G0, 0)));

        operation_vec.push(Operation::Macro(MacroInstr::Bne(Reg::G12, Reg::G13, "_JUMP".to_string())));
        operation_vec.push(Operation::Instr(Instruction::Slli(Reg::G13, Reg::G13, 1)));
        operation_vec.push(Operation::LablInstr(Cow::from("_JUMP2"), Instruction::Sub(Reg::G12, Reg::G12, Reg::G13)));
        operation_vec.push(Operation::Instr(Instruction::Add(Reg::G10, Reg::G10, Reg::G17)));
        operation_vec.push(Operation::LablMacro(Cow::from("_JUMP"), MacroInstr::Blt(Reg::G12, Reg::G13, "_LTDIVLOOP".to_string()))); // 9

        operation_vec.push(Operation::LablInstr(Cow::from("_GTDIVLOOP"), Instruction::Slli(Reg::G13, Reg::G13, 1)));
        operation_vec.push(Operation::Instr(Instruction::Slli(Reg::G17, Reg::G17, 1)));
        operation_vec.push(Operation::Macro(MacroInstr::Blt(Reg::G13, Reg::G12, "_GTDIVLOOP".to_string())));
        operation_vec.push(Operation::Instr(Instruction::Srli(Reg::G13, Reg::G13, 1)));
        operation_vec.push(Operation::Instr(Instruction::Srli(Reg::G17, Reg::G17, 1)));
        operation_vec.push(Operation::Instr(Instruction::Sub(Reg::G12, Reg::G12, Reg::G13)));
        operation_vec.push(Operation::Instr(Instruction::Add(Reg::G10, Reg::G10, Reg::G17)));
        operation_vec.push(Operation::Macro(MacroInstr::Bne(Reg::G12, Reg::G0, "_JUMP".to_string()))); // 17
        operation_vec.push(Operation::Macro(MacroInstr::Beq(Reg::G0, Reg::G0, "_Vergleich".to_string()))); // 18
        operation_vec.push(Operation::Instr(Instruction::Jalr(Reg::G0, Reg::G1, 0)));

        operation_vec.push(Operation::LablInstr(Cow::from("_LTDIVLOOP"), Instruction::Srli(Reg::G13, Reg::G13, 1)));
        operation_vec.push(Operation::Instr(Instruction::Srli(Reg::G17, Reg::G17, 1)));
        operation_vec.push(Operation::Macro(MacroInstr::Blt(Reg::G12, Reg::G13, "_LTDIVLOOP".to_string())));
        operation_vec.push(Operation::Instr(Instruction::Slli(Reg::G13, Reg::G13, 1)));
        operation_vec.push(Operation::Instr(Instruction::Slli(Reg::G17, Reg::G17, 1)));
        operation_vec.push(Operation::Instr(Instruction::Beq(Reg::G12, Reg::G13, 8)));
        operation_vec.push(Operation::Instr(Instruction::Srli(Reg::G13, Reg::G13, 1)));
        operation_vec.push(Operation::Instr(Instruction::Srli(Reg::G17, Reg::G17, 1)));
        operation_vec.push(Operation::Instr(Instruction::Sub(Reg::G12, Reg::G12, Reg::G13)));
        operation_vec.push(Operation::Instr(Instruction::Add(Reg::G10, Reg::G10, Reg::G17)));
        operation_vec.push(Operation::LablMacro(Cow::from("_Vergleich"), MacroInstr::Bne(Reg::G12, Reg::G0, "_JUMP".to_string()))); // 30
        operation_vec.push(Operation::Instr(Instruction::Jalr(Reg::G0, Reg::G1, 0)));

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

        assert_eq!(optimize((namespace_ver.clone(), operation_vec), false).unwrap(), instruction_ver);
    }

    #[test]
    fn test_macro_translate() {
        let cs: usize = 0;
        let mut namespace = Namespaces::new();

        let mut lr = LabelRecog::new();
        lr.crt_def_ref(&"GLOBAL".to_string(), true, 0);
        lr.crt_def_ref(&"LOCAL".to_string(), false, 1);

        let mut instructions = Vec::from([
            Instruction::Addi(Reg::G0, Reg::G0, 0),
            Instruction::Addi(Reg::G0, Reg::G0, 0)
        ]);

        let _ = namespace.insert_recog(lr);

        {
            let test_addi_macros = [
                (MacroInstr::Addi(Reg::G0, Reg::G0, "LOCAL".to_string(), Part::Lower), Instruction::Addi(Reg::G0, Reg::G0, -4)),
                (MacroInstr::Addi(Reg::G0, Reg::G0, "GLOBAL".to_string(), Part::Lower), Instruction::Addi(Reg::G0, Reg::G0, -8))
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

        test_macros.push((MacroInstr::Lui(Reg::G21, "GLOBAL".to_string()), Instruction::Lui(Reg::G21, -8)));
        test_macros.push((MacroInstr::Slli(Reg::G30, Reg::G19, "LOCAL".to_string()), Instruction::Slli(Reg::G30, Reg::G19, -4)));
        test_macros.push((MacroInstr::Srli(Reg::G5, Reg::G20, "GLOBAL".to_string()), Instruction::Srli(Reg::G5, Reg::G20, -8)));
        test_macros.push((MacroInstr::Srai(Reg::G7, Reg::G15, "LOCAL".to_string()), Instruction::Srai(Reg::G7, Reg::G15, -4)));

        for (test, corr) in test_macros {
            let _ = test.translate(&mut namespace, &cs, &mut instructions);
            assert_eq!(instructions[2], corr);
            instructions.remove(2);
        }
    }
}
