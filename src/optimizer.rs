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

use std::collections::{BTreeMap, VecDeque};

use crate::{
    common::{Instruction, Operation, MacroInstr, Reg, LabelElem}, 
    linker::Namespaces
};

#[derive(Clone)]
enum RegActType<'a> {
    NA,
    WriteRead2(&'a Reg, &'a Reg, &'a Reg),
    WriteRead(&'a Reg, &'a Reg),
    Read2(&'a Reg, &'a Reg),
    Write(&'a Reg),
    Store(&'a Reg, &'a Reg),
    Load(&'a Reg, &'a Reg)
}

impl <'a> RegActType<'a> {
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
                match self {
                    RegActType::NA | RegActType::Write(_) => false,

                    RegActType::Store(reg1, reg2) |
                    RegActType::Read2(reg1, reg2) |
                    RegActType::WriteRead2(_, reg1, reg2) => (**reg1 == **write_t) || (**reg2 == **write_t),

                    RegActType::Load(_, reg) |
                    RegActType::WriteRead(_, reg) => **reg == **write_t,
                }
            },

            #[cfg(not(feature = "raw_nop"))]
            RegActType::WriteRead2(_, _, _) |
            RegActType::Write(_) |
            RegActType::WriteRead(_, _) => false,

            RegActType::Load(target, _) => {
                match self {
                    RegActType::NA | RegActType::Write(_) => false,

                    #[cfg(feature = "mem_load_nop")]
                    RegActType::Store(reg, _) => **reg == **target,

                    #[cfg(not(feature = "mem_load_nop"))]
                    RegActType::Store(reg, _) => false,

                    RegActType::Read2(reg1, reg2) |
                    RegActType::WriteRead2(_, reg1, reg2) => (**reg1 == **target) || (**reg2 == **target),

                    RegActType::Load(_, reg) |
                    RegActType::WriteRead(_, reg) => **reg == **target,
                }
            },
        }
    }
}

/*
struct RegAct<'a> {
    regtype: RegActType<'a>,
    pos: usize
}

impl <'a> RegAct<'a> {
    fn new(operation: &'a Operation<'_>, pos: usize) -> RegAct<'a> {
        let regtype = operation.into();
        RegAct { 
            regtype,
            pos 
        }
    }
}*/

struct LimitedQueue<'a> {
    size: usize,
    //queue: VecDeque<RegAct<'a>>
    queue: VecDeque<RegActType<'a>>
}

impl <'a> LimitedQueue<'a> {
    fn new_sized(size: usize) -> LimitedQueue<'a> {
        let queue = VecDeque::with_capacity(size);
        LimitedQueue{ size, queue }
    }

    fn limited_insert(&mut self, reg: RegActType<'a>) {
        if self.queue.len() == self.size {
            self.queue.pop_back();
        }
        self.queue.push_front(reg);
    }

    fn compare_and_insert(&mut self, reg: RegActType<'a>) -> i8 {
        for (counter, reg_dep) in self.queue.iter().enumerate() {
            if reg.is_hazard_before(reg_dep) {
                self.queue.truncate(counter);
                self.limited_insert(reg);
                return counter as i8
            }
        };
        self.limited_insert(reg);
        -1
    }
}

impl <'a> From<&'a Operation<'_>> for RegActType<'a> {
    fn from(item: &'a Operation) -> RegActType<'a> {
        match item {
            Operation::LablInstr(_, instr) | Operation::Instr(instr) => {
                match instr {
                    Instruction::NA => RegActType::NA,

                    Instruction::Addn(reg1, reg2, reg3) |
                    Instruction::Subn(reg1, reg2, reg3) |
                    Instruction::Xor(reg1, reg2, reg3) |
                    Instruction::Or(reg1, reg2, reg3) |
                    Instruction::And(reg1, reg2, reg3) |

                    Instruction::Sll(reg1, reg2, reg3) |
                    Instruction::Srl(reg1, reg2, reg3) |
                    Instruction::Sra(reg1, reg2, reg3) |
                    Instruction::Slt(reg1, reg2, reg3) |
                    Instruction::Sltu(reg1, reg2, reg3) => RegActType::WriteRead2(reg1, reg2, reg3),

                    Instruction::Addi(reg1, reg2, _) |
                    Instruction::Xori(reg1, reg2, _) |
                    Instruction::Ori(reg1, reg2, _) |
                    Instruction::Andi(reg1, reg2, _) |
                    Instruction::Slti(reg1, reg2, _) |
                    Instruction::Sltiu(reg1, reg2, _) |
                    Instruction::Slli(reg1, reg2, _) |
                    Instruction::Srli(reg1, reg2, _) |
                    Instruction::Srai(reg1, reg2, _) |
                    Instruction::Jalr(reg1, reg2, _) => RegActType::WriteRead(reg1, reg2),

                    Instruction::Lb(reg1, reg2, _) |
                    Instruction::Lh(reg1, reg2, _) |
                    Instruction::Lw(reg1, reg2, _) |
                    Instruction::Lbu(reg1, reg2, _) |
                    Instruction::Lhu(reg1, reg2, _) => RegActType::Load(reg1, reg2),

                    Instruction::Sh(reg1, reg2, _) |
                    Instruction::Sb(reg1, reg2, _) |
                    Instruction::Sw(reg1, reg2, _) => RegActType::Store(reg1, reg2),

                    Instruction::Beq(reg1, reg2, _) |
                    Instruction::Bne(reg1, reg2, _) |
                    Instruction::Blt(reg1, reg2, _) |
                    Instruction::Bltu(reg1, reg2, _) |
                    Instruction::Bge(reg1, reg2, _) |
                    Instruction::Bgeu(reg1, reg2, _) => RegActType::Read2(reg1, reg2),


                    Instruction::Lui(reg, _) |
                    Instruction::Auipc(reg, _) |
                    Instruction::Jal(reg, _) => RegActType::Write(reg),
                }
            },
            Operation::Macro(instr) | Operation::LablMacro(_, instr) => {
                match instr {
                    MacroInstr::Beq(reg1, reg2, _) |
                    MacroInstr::Bne(reg1, reg2, _) |
                    MacroInstr::Blt(reg1, reg2, _) |
                    MacroInstr::Bltu(reg1, reg2, _) |
                    MacroInstr::Bge(reg1, reg2, _) |
                    MacroInstr::Bgeu(reg1, reg2, _) => RegActType::Read2(reg1, reg2),

                    MacroInstr::Lui(reg, _) |
                    MacroInstr::Auipc(reg, _) |
                    MacroInstr::Jal(reg, _) => RegActType::Write(reg),

                    MacroInstr::Slli(reg1, reg2, _) |
                    MacroInstr::Srli(reg1, reg2, _) |
                    MacroInstr::Srai(reg1, reg2, _) |
                    MacroInstr::Jalr(reg1, reg2, _) => RegActType::WriteRead(reg1, reg2),

                    MacroInstr::Lb(reg1, reg2, _) |
                    MacroInstr::Lh(reg1, reg2, _) |
                    MacroInstr::Lw(reg1, reg2, _) |
                    MacroInstr::Lbu(reg1, reg2, _) |
                    MacroInstr::Lhu(reg1, reg2, _) => RegActType::Load(reg1, reg2),

                    MacroInstr::Sh(reg1, reg2, _) |
                    MacroInstr::Sb(reg1, reg2, _) |
                    MacroInstr::Sw(reg1, reg2, _) => RegActType::Store(reg1, reg2),
                }
            },
            _ => RegActType::NA,
        }
    }
}

impl MacroInstr {
    fn translate(&self, namespace: &mut Namespaces, current_space: &usize, instructions: &mut Vec<Instruction>) {
        // Do not forget to change the lines function in the parser when changing the amount of lines here! 
        // (TODO: Better method for this)
        match self {
            MacroInstr::Beq(reg1, reg2, labl) => {
                let lines = translate_label(instructions.len() as i128, labl.to_owned(), namespace, *current_space);
                instructions.push(Instruction::Beq(reg1.to_owned(), reg2.to_owned(), lines));
            },
            MacroInstr::Bne(reg1, reg2, labl) => {
                let lines = translate_label(instructions.len() as i128, labl.to_owned(), namespace, *current_space);
                instructions.push(Instruction::Bne(reg1.to_owned(), reg2.to_owned(), lines));
            },
            MacroInstr::Blt(reg1, reg2, labl) => {
                let lines = translate_label(instructions.len() as i128, labl.to_owned(), namespace, *current_space);
                instructions.push(Instruction::Blt(reg1.to_owned(), reg2.to_owned(), lines));
            },
            MacroInstr::Bltu(reg1, reg2, labl) => {
                let lines = translate_label(instructions.len() as i128, labl.to_owned(), namespace, *current_space);
                instructions.push(Instruction::Bltu(reg1.to_owned(), reg2.to_owned(), lines));
            },
            MacroInstr::Bge(reg1, reg2, labl) => {
                let lines = translate_label(instructions.len() as i128, labl.to_owned(), namespace, *current_space);
                instructions.push(Instruction::Bge(reg1.to_owned(), reg2.to_owned(), lines));
            },
            MacroInstr::Bgeu(reg1, reg2, labl) => {
                let lines = translate_label(instructions.len() as i128, labl.to_owned(), namespace, *current_space);
                instructions.push(Instruction::Bgeu(reg1.to_owned(), reg2.to_owned(), lines));
            },

            MacroInstr::Jal(reg, labl) => {
                let lines = translate_label(instructions.len() as i128, labl.to_owned(), namespace, *current_space);
                instructions.push(Instruction::Jal(reg.to_owned(), lines));
            },
            MacroInstr::Jalr(reg1, reg2, labl) => {
                let lines = translate_label(instructions.len() as i128, labl.to_owned(), namespace, *current_space);
                instructions.push(Instruction::Jalr(reg1.to_owned(), reg2.to_owned(), lines));
            },

            MacroInstr::Lui(reg, labl) => {
                let lines = translate_label(instructions.len() as i128, labl.to_owned(), namespace, *current_space);
                instructions.push(Instruction::Lui(reg.to_owned(), lines));
            },
            MacroInstr::Auipc(reg, labl) => {
                let lines = translate_label(instructions.len() as i128, labl.to_owned(), namespace, *current_space);
                instructions.push(Instruction::Auipc(reg.to_owned(), lines));
            },

            MacroInstr::Slli(reg1, reg2, labl) => {
                let lines = translate_label(instructions.len() as i128, labl.to_owned(), namespace, *current_space);
                instructions.push(Instruction::Slli(reg1.to_owned(), reg2.to_owned(), lines));
            },
            MacroInstr::Srli(reg1, reg2, labl) => {
                let lines = translate_label(instructions.len() as i128, labl.to_owned(), namespace, *current_space);
                instructions.push(Instruction::Srli(reg1.to_owned(), reg2.to_owned(), lines));
            },
            MacroInstr::Srai(reg1, reg2, labl) => {
                let lines = translate_label(instructions.len() as i128, labl.to_owned(), namespace, *current_space);
                instructions.push(Instruction::Srai(reg1.to_owned(), reg2.to_owned(), lines));
            },

            // TODO: Evaluate if this is right? Spec paper seems to add upper half of symbol to PC (needed for our case?)
            MacroInstr::Lb(reg1, reg2, labl) => {
                let lines = translate_label(instructions.len() as i128, labl.to_owned(), namespace, *current_space);
                instructions.push(Instruction::Lb(reg1.to_owned(), reg2.to_owned(), lines));
            },
            MacroInstr::Lh(reg1, reg2, labl) => {
                let lines = translate_label(instructions.len() as i128, labl.to_owned(), namespace, *current_space);
                instructions.push(Instruction::Lh(reg1.to_owned(), reg2.to_owned(), lines));
            },
            MacroInstr::Lw(reg1, reg2, labl) => {
                let lines = translate_label(instructions.len() as i128, labl.to_owned(), namespace, *current_space);
                instructions.push(Instruction::Lw(reg1.to_owned(), reg2.to_owned(), lines));
            },
            MacroInstr::Lbu(reg1, reg2, labl) => {
                let lines = translate_label(instructions.len() as i128, labl.to_owned(), namespace, *current_space);
                instructions.push(Instruction::Lbu(reg1.to_owned(), reg2.to_owned(), lines));
            },
            MacroInstr::Lhu(reg1, reg2, labl) => {
                let lines = translate_label(instructions.len() as i128, labl.to_owned(), namespace, *current_space);
                instructions.push(Instruction::Lhu(reg1.to_owned(), reg2.to_owned(), lines));
            },

            MacroInstr::Sb(reg1, reg2, labl) => {
                let lines = translate_label(instructions.len() as i128, labl.to_owned(), namespace, *current_space);
                instructions.push(Instruction::Sb(reg1.to_owned(), reg2.to_owned(), lines));
            },
            MacroInstr::Sh(reg1, reg2, labl) => {
                let lines = translate_label(instructions.len() as i128, labl.to_owned(), namespace, *current_space);
                instructions.push(Instruction::Sh(reg1.to_owned(), reg2.to_owned(), lines));
            },
            MacroInstr::Sw(reg1, reg2, labl) => {
                let lines = translate_label(instructions.len() as i128, labl.to_owned(), namespace, *current_space);
                instructions.push(Instruction::Sw(reg1.to_owned(), reg2.to_owned(), lines));
            },
        };
    }
}

fn translate_label(total_instructions: i128, label: String, namespaces: &mut Namespaces, current_space: usize) -> i32 {
    match namespaces.get_label(label, Some(current_space)) {
        Some(label_elem) => <i128 as TryInto<i32>>::try_into((total_instructions * 4) - (*label_elem.get_def() * 4)).unwrap(),
        None => panic!("[Error] Label does not exist! Could not get position of label!"),
    }
}

fn find_and_set_label_ns(namespace: &mut Namespaces, ) -> usize {
    let testing_spaces = code.0.get_namespaces().filter(|b| b > &space);
    for pot_space in testing_spaces {
        match code.0.get_label(label.to_string(), Some(pot_space)) {
            Some(lablel) => {
                lablel.add_def((nop_pointer as i128) - (real_pointer as i128));
                break;
            },
            None => (),
        }
    }
}

fn nop_insertion(mut code: (Namespaces, Vec<Operation>)) -> (Namespaces, Vec<Operation>) {
    let mut code_inserts: BTreeMap<String, usize> = BTreeMap::new();

    let mut working_set: LimitedQueue = LimitedQueue::new_sized(3);
    let mut branch_ptr_queue: VecDeque<(usize, LimitedQueue)> = VecDeque::new();

    let mut space = 0;

    let mut real_pointer: usize = 0;
    let mut nop_pointer: usize = 0;
    // Try to circumvent too many allocations thus reducing performance;
    // TODO: Better approach
    let mut nop_inserted_code: Vec<Operation> = Vec::with_capacity(code.1.len() * 2);

    loop {
        let operation = code.1.get(real_pointer);
        match operation {
            Some(opera) => {
                let reg_dep = RegActType::from(opera);
                let nop_insert = working_set.compare_and_insert(reg_dep);
                if nop_insert != -1 {
                    let instr_num = 3 - nop_insert;
                    for _ in 1..instr_num {
                        nop_inserted_code.insert(nop_pointer, Operation::Instr(Instruction::Addi(Reg::G0, Reg::G0, 0)));
                    }
                    nop_pointer += instr_num as usize;
                }
                match opera {
                    Operation::LablInstr(label, instr) => {
                        if real_pointer != nop_pointer {
                            match code.0.get_label(label.to_string(), Some(space)) {
                                Some(lablel) => lablel.add_def((nop_pointer as i128) - (real_pointer as i128)),
                                None => {
                                    // Could happen, when Namespace is jumped over; In such a case
                                    // apply some heuristics to quickly find correct namespace
                                    let part = (code.1.len() / 2) - real_pointer;
                                    if part > 0 {
                                        
                                    } else {

                                    }
                                },
                            }
                        }
                    },
                    Operation::LablMacro(label, instr) => (),
                    Operation::Labl(_) => (),
                    Operation::Instr(instr) => (),
                    Operation::Macro(instr) => (),
                    Operation::Namespace(ns) => space = *ns,
                }
                real_pointer += 1;
            },
            None => break,
        }
    }
    //let mut reg_dep_graph: [Vec<RegAct>; 31] = Default::default();

    (code.0, nop_inserted_code)
}

fn substitute_labels(mut code: (Namespaces, Vec<Operation>)) -> Vec<Instruction> {
    let mut instructions: Vec<Instruction> = vec![];
    let mut namespace: usize = 0;

    for operation in code.1 {
        match operation {
            Operation::Namespace(space) => namespace = space,
            Operation::Macro(instr) | Operation::LablMacro(_, instr) => {
                instr.translate(&mut code.0, &namespace, &mut instructions);
            },
            Operation::Instr(instr) | Operation::LablInstr(_, instr) => {
                instructions.push(instr);
            }
            Operation::Labl(_) => (),
        };
    }

    instructions
}

pub fn optimize(code: (Namespaces, Vec<Operation>)) -> Vec<Instruction> {
    //let nop_code = nop_insertion(code);
    substitute_labels(code)
}

// TODO: Tests here & more test cases
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[cfg(feature = "raw_nop")]
    fn test_reg_act_type_raw_hazard() {
        let last = RegActType::WriteRead(&Reg::G15, &Reg::G10);
        let first = RegActType::Write(&Reg::G10);
        let second = RegActType::WriteRead2(&Reg::G15, &Reg::G10, &Reg::G20);

        assert_eq!(last.is_hazard_before(&first), true);
        assert_eq!(second.is_hazard_before(&first), true);
        assert_eq!(last.is_hazard_before(&second), false);
    }

    #[test]
    #[cfg(feature = "mem_load_nop")]
    fn test_reg_act_type_mem_hazard() {
        let last = RegActType::Store(&Reg::G15, &Reg::G10);
        let first = RegActType::Load(&Reg::G15, &Reg::G10);
        let second = RegActType::Load(&Reg::G15, &Reg::G10);

        assert_eq!(last.is_hazard_before(&first), true);
        assert_eq!(second.is_hazard_before(&first), false);
        assert_eq!(last.is_hazard_before(&second), true);
    }

    #[test]
    fn test_reg_act_type_load_use_hazard() {
        let last = RegActType::WriteRead2(&Reg::G15, &Reg::G10, &Reg::G15);
        let first = RegActType::Load(&Reg::G15, &Reg::G10);
        let second = RegActType::WriteRead(&Reg::G15, &Reg::G15);

        assert_eq!(last.is_hazard_before(&first), true);
        assert_eq!(second.is_hazard_before(&first), true);
    }

    #[test]
    fn test_limited_queue() {
        let mut queue = LimitedQueue::new_sized(3);

        // Full of raw hazards
        let reg_act_vec = Vec::from([
            RegActType::Write(&Reg::G10),
            RegActType::WriteRead2(&Reg::G15, &Reg::G10, &Reg::G20),
            RegActType::WriteRead(&Reg::G15, &Reg::G10)
        ]);

        for (counter, reg) in reg_act_vec.iter().enumerate() {
            let nop_inserts = queue.compare_and_insert(reg.clone());
            match counter {
                0 => assert_eq!(nop_inserts, -1),
                1 => assert_eq!(nop_inserts, 0),
                2 => assert_eq!(nop_inserts, -1),
                _ => (),
            }
        }

        // No hazards
        let reg_act_vec2 = Vec::from([
            RegActType::Write(&Reg::G10),
            RegActType::WriteRead(&Reg::G10, &Reg::G12),
            RegActType::WriteRead(&Reg::G10, &Reg::G13)
        ]);

        for (counter, reg) in reg_act_vec2.iter().enumerate() {
            let nop_inserts = queue.compare_and_insert(reg.clone());
            match counter {
                0 => assert_eq!(nop_inserts, -1),
                1 => assert_eq!(nop_inserts, -1),
                2 => assert_eq!(nop_inserts, -1),
                _ => (),
            }
        }
    }
}