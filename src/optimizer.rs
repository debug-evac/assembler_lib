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

use crate::{parser::{Instruction, Operation, LabelElem, Reg, Imm}, linker::Namespaces};


/* 
fn replace_instruction(input:Instruction) -> Vec<Operation>{
    let mut output: Vec<Operation> = vec![];
    match input {
        Instruction::Muln(Reg3, Reg1, Reg2) =>  output = mul_subroutine(),
    }

    output
}*/

fn mul_subroutine(n_spaces: &mut Namespaces, counter: usize, regs:&[Reg], imm: Option<Imm>) -> (Vec<Operation>, Vec<Operation>){
    let mut output: (Vec<Operation>, Vec<Operation>) = (vec![], vec![]);
    
    
    let mut label = LabelElem::new();
    label.set_scope(true);
    label.set_def(counter as i128);
    label.set_name("_mul".to_string());

    let mut label = LabelElem::new();
    label.set_scope(true);
    label.set_def(counter as i128);
    label.set_name("_mulI".to_string());

    match imm {
        Some() => {

            output.0.push(Operation::Instr(Instruction::VJmp("_mulI".to_string())));
            output.1.push(Operation::Instr(Instruction::Lw(Reg::G5, Reg::G0, 1)));
            output.1.push(Operation::Instr(Instruction::Lw(Reg::G28, Reg::G0, 0)));

            output.1.push(Operation::Instr(Instruction::Blt(imm, regs.1, 28)));           //Binär oder dezimal angegeben? Wie viele müssen pro Anweisung übersprungen werden
            output.1.push(Operation::Instr(Instruction::And(Reg::G6, Reg::G5, imm)));
            output.1.push(Operation::Instr(Instruction::Bne(Reg::G6, Reg::G0, 8)));         //Binär oder dezimal angegeben? Wie viele müssen pro Anweisung übersprungen werden
            output.1.push(Operation::Instr(Instruction::Sll(Reg::G7, Reg::G28, regs.1)));
            output.1.push(Operation::Instr(Instruction::Addn(Reg::G31, Reg::G31, Reg::G7)));
            output.1.push(Operation::Instr(Instruction::Addi(Reg::G28, Reg::G28, 1)));
            output.1.push(Operation::Instr(Instruction::Slli(Reg::G5, Reg::G5, 1)));
            output.1.push(Operation::Instr(Instruction::Bge(regs.1, Reg::G5, -24)));         //Binär oder dezimal angegeben? Wie viele müssen pro Anweisung übersprungen werden, kann man negativ springen?
            
            output.1.push(Operation::Instr(Instruction::Beq(Reg::G0, Reg::G0, 28)));         //Binär oder dezimal angegeben? Wie viele müssen pro Anweisung übersprungen werden


            output.1.push(Operation::Instr(Instruction::And(Reg::G6, Reg::G5, regs.1)));
            output.1.push(Operation::Instr(Instruction::Bne(Reg::G6, Reg::G0, 8)));         //Binär oder dezimal angegeben? Wie viele müssen pro Anweisung übersprungen werden
            output.1.push(Operation::Instr(Instruction::Sll(Reg::G7, Reg::G28, imm)));
            output.1.push(Operation::Instr(Instruction::Addn(Reg::G31, Reg::G31, Reg::G7)));
            output.1.push(Operation::Instr(Instruction::Addi(Reg::G28, Reg::G28, 1)));
            output.1.push(Operation::Instr(Instruction::Slli(Reg::G5, Reg::G5, 1)));
            output.1.push(Operation::Instr(Instruction::Bge(imm, Reg::G5, -24)));          //Binär oder dezimal angegeben? Wie viele müssen pro Anweisung übersprungen werden, kann man negativ springen?


           
        },
        None => {
            output.0.push(Operation::Instr(Instruction::VJmp("_mulI".to_string())));
            output.1.push(Operation::Instr(Instruction::Lw(Reg::G5, Reg::G0, 1)));
            output.1.push(Operation::Instr(Instruction::Lw(Reg::G28, Reg::G0, 0)));

            output.1.push(Operation::Instr(Instruction::Blt(regs.2, regs.1, 28)));           //Binär oder dezimal angegeben? Wie viele müssen pro Anweisung übersprungen werden
            output.1.push(Operation::Instr(Instruction::And(Reg::G6, Reg::G5, regs.2)));
            output.1.push(Operation::Instr(Instruction::Bne(Reg::G6, Reg::G0, 8)));         //Binär oder dezimal angegeben? Wie viele müssen pro Anweisung übersprungen werden
            output.1.push(Operation::Instr(Instruction::Sll(Reg::G7, Reg::G28, regs.1)));
            output.1.push(Operation::Instr(Instruction::Addn(Reg::G31, Reg::G31, Reg::G7)));
            output.1.push(Operation::Instr(Instruction::Addi(Reg::G28, Reg::G28, 1)));
            output.1.push(Operation::Instr(Instruction::Slli(Reg::G5, Reg::G5, 1)));
            output.1.push(Operation::Instr(Instruction::Bge(regs.1, Reg::G5, -24)));         //Binär oder dezimal angegeben? Wie viele müssen pro Anweisung übersprungen werden, kann man negativ springen?

            output.1.push(Operation::Instr(Instruction::Beq(Reg::G0, Reg::G0, 28)));         //Binär oder dezimal angegeben? Wie viele müssen pro Anweisung übersprungen werden


            output.1.push(Operation::Instr(Instruction::And(Reg::G6, Reg::G5, regs.1)));
            output.1.push(Operation::Instr(Instruction::Bne(Reg::G6, Reg::G0, 8)));         //Binär oder dezimal angegeben? Wie viele müssen pro Anweisung übersprungen werden
            output.1.push(Operation::Instr(Instruction::Sll(Reg::G7, Reg::G28, regs.2)));
            output.1.push(Operation::Instr(Instruction::Addn(Reg::G31, Reg::G31, Reg::G7)));
            output.1.push(Operation::Instr(Instruction::Addi(Reg::G28, Reg::G28, 1)));
            output.1.push(Operation::Instr(Instruction::Slli(Reg::G5, Reg::G5, 1)));
            output.1.push(Operation::Instr(Instruction::Bge(regs.2, Reg::G5, -24)));          //Binär oder dezimal angegeben? Wie viele müssen pro Anweisung übersprungen werden, kann man negativ springen?
        },
    }

    output
} 

pub fn instr_sub(input: (Namespaces, Vec<Operation>)) -> (Namespaces, Vec<Operation>){
    let mut output = input.clone();
    output.1.clear();
    let mut counter: usize = 0;
    //let mut n_space: usize = 0;
    let mut sub_rout: Vec<Operation> = vec![];
    for op in input.1{
        match op {
            Operation::Instr(instr) => {
                match instr {
                    Instruction::Muln(R3,R1,R2) => mul_subroutine(&mut input.0, counter, &[R3,R1,R2]),
                    
                }
            },
            Operation::LablInstr(_, instr) => (),
            Operation::Labl(_) => output.1.push(op),
            Operation::Namespace(space) => {
               // n_space = space.parse::<usize>().unwrap();
                output.1.push(op);
            },
        };
    }
    output
}
use std::collections::VecDeque;
use std::borrow::Cow;

use crate::{
    common::{Instruction, Operation, MacroInstr, Reg}, 
    linker::Namespaces
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
                    RegActType::Store(reg, _) => *reg == *target,

                    #[cfg(not(feature = "mem_load_nop"))]
                    RegActType::Store(reg, _) => false,

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

    fn compare_and_insert(&mut self, reg: RegActType) -> i8 {
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

    fn flush(&mut self) {
        self.queue.clear()
    }
}

impl From<&Operation<'_>> for RegActType {
    fn from(item: &Operation) -> RegActType {
        match item {
            Operation::LablInstr(_, instr) | Operation::Instr(instr) => {
                match instr {
                    Instruction::NA => RegActType::NA,

                    Instruction::Xnor(reg1, reg2, reg3) |
                    Instruction::Equal(reg1, reg2, reg3) |
                    Instruction::Addn(reg1, reg2, reg3) |
                    Instruction::Subn(reg1, reg2, reg3) |
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
                    MacroInstr::Auipc(reg, _) |
                    MacroInstr::Jal(reg, _) => RegActType::Write(reg.clone()),

                    MacroInstr::Slli(reg1, reg2, _) |
                    MacroInstr::Srli(reg1, reg2, _) |
                    MacroInstr::Srai(reg1, reg2, _) |
                    MacroInstr::Jalr(reg1, reg2, _) => RegActType::WriteRead(reg1.clone(), reg2.clone()),

                    MacroInstr::Lb(reg1, reg2, _) |
                    MacroInstr::Lh(reg1, reg2, _) |
                    MacroInstr::Lw(reg1, reg2, _) |
                    MacroInstr::Lbu(reg1, reg2, _) |
                    MacroInstr::Lhu(reg1, reg2, _) => RegActType::Load(reg1.clone(), reg2.clone()),

                    MacroInstr::Sh(reg1, reg2, _) |
                    MacroInstr::Sb(reg1, reg2, _) |
                    MacroInstr::Sw(reg1, reg2, _) => RegActType::Store(reg1.clone(), reg2.clone()),
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

fn cond_add_acc_label(namespaces: &mut Namespaces, accumulator: i128, label: Cow<str>, space: usize) {
    if accumulator != 0 {
        match namespaces.get_label(label.to_string(), Some(space)) {
            Some(lablel) => lablel.add_def(accumulator),
            None => panic!("[Error] Label not found: {}", label),
        }
    }
}

fn nop_insertion(code: &mut (Namespaces, Vec<Operation>)) {
    
    let mut working_set: LimitedQueue = LimitedQueue::new_sized(3);
    
    let mut accumulator: i128 = 0;
    let mut space = 0;
    let mut pointer = 0;

    // At maximum every instruction requires 3 nop operations
    // to run without hazards. To lower the number of allocations
    // thus increasing performance, trying to allocate these elements
    code.1.reserve(code.1.len() * 3);

    loop {
        let operation = code.1.get(pointer).cloned();
        match operation {
            Some(opera) => {
                match &opera {
                    Operation::LablInstr(label, instr) => {
                        match instr {
                            Instruction::Beq(_, _, _) |
                            Instruction::Bne(_, _, _) |
                            Instruction::Blt(_, _, _) |
                            Instruction::Bltu(_, _, _) |
                            Instruction::Bge(_, _, _) |
                            Instruction::Bgeu(_, _, _) |
                            Instruction::Jal(_, _) |
                            Instruction::Jalr(_, _, _) => {
                                working_set.flush();
                                cond_add_acc_label(&mut code.0, accumulator - 1, std::borrow::Cow::Borrowed(label), space);
                                pointer += 1;
                                continue;
                            },
                            _ => {
                                let reg_dep = RegActType::from(&opera);
                                let nop_insert = working_set.compare_and_insert(reg_dep);
                                if nop_insert > -1 {
                                    let instr_num = 4 - nop_insert;
                                    for _ in 1..instr_num {
                                        code.1.insert(pointer, Operation::Instr(Instruction::Addi(Reg::G0, Reg::G0, 0)));
                                    }
                                    pointer += (instr_num - 1) as usize;
                                    accumulator += instr_num as i128;
                                }
                                cond_add_acc_label(&mut code.0, accumulator - 1, std::borrow::Cow::Borrowed(label), space);
                            },
                        }
                    },
                    Operation::LablMacro(label, instr) => {
                        match instr {
                            MacroInstr::Beq(_, _, _) |
                            MacroInstr::Bne(_, _, _) |
                            MacroInstr::Blt(_, _, _) |
                            MacroInstr::Bltu(_, _, _) |
                            MacroInstr::Bge(_, _, _) |
                            MacroInstr::Bgeu(_, _, _) |
                            MacroInstr::Jal(_, _) |
                            MacroInstr::Jalr(_, _, _) => {
                                working_set.flush();
                                cond_add_acc_label(&mut code.0, accumulator - 1, std::borrow::Cow::Borrowed(label), space);
                                pointer += 1;
                                continue;
                            },
                            _ => {
                                let reg_dep = RegActType::from(&opera);
                                let nop_insert = working_set.compare_and_insert(reg_dep);
                                if nop_insert > -1 {
                                    let instr_num = 4 - nop_insert;
                                    for _ in 1..instr_num {
                                        code.1.insert(pointer, Operation::Instr(Instruction::Addi(Reg::G0, Reg::G0, 0)));
                                    }
                                    pointer += (instr_num - 1) as usize;
                                    accumulator += instr_num as i128;
                                }
                                cond_add_acc_label(&mut code.0, accumulator - 1, std::borrow::Cow::Borrowed(label), space);
                            },
                        }
                    },
                    Operation::Labl(label) => {
                        cond_add_acc_label(&mut code.0, accumulator - 1, std::borrow::Cow::Borrowed(label), space);
                    },
                    Operation::Instr(instr) => {
                        match instr {
                            Instruction::Beq(_, _, _) |
                            Instruction::Bne(_, _, _) |
                            Instruction::Blt(_, _, _) |
                            Instruction::Bltu(_, _, _) |
                            Instruction::Bge(_, _, _) |
                            Instruction::Bgeu(_, _, _) |
                            Instruction::Jal(_, _) |
                            Instruction::Jalr(_, _, _) => {
                                working_set.flush();
                                pointer += 1;
                                continue;
                            },
                            _ => {
                                let reg_dep = RegActType::from(&opera);
                                let nop_insert = working_set.compare_and_insert(reg_dep);
                                if nop_insert > -1 {
                                    let instr_num = 4 - nop_insert;
                                    for _ in 1..instr_num {
                                        code.1.insert(pointer, Operation::Instr(Instruction::Addi(Reg::G0, Reg::G0, 0)));
                                    }
                                    pointer += (instr_num - 1) as usize;
                                    accumulator += instr_num as i128;
                                }
                            },
                        }
                    },
                    Operation::Macro(instr) => {
                        match instr {
                            MacroInstr::Beq(_, _, _) |
                            MacroInstr::Bne(_, _, _) |
                            MacroInstr::Blt(_, _, _) |
                            MacroInstr::Bltu(_, _, _) |
                            MacroInstr::Bge(_, _, _) |
                            MacroInstr::Bgeu(_, _, _) |
                            MacroInstr::Jal(_, _) |
                            MacroInstr::Jalr(_, _, _) => {
                                working_set.flush();
                                pointer += 1;
                                continue;
                            },
                            _ => {
                                let reg_dep = RegActType::from(&opera);
                                let nop_insert = working_set.compare_and_insert(reg_dep);
                                if nop_insert > -1 {
                                    let instr_num = 4 - nop_insert;
                                    for _ in 1..instr_num {
                                        code.1.insert(pointer, Operation::Instr(Instruction::Addi(Reg::G0, Reg::G0, 0)));
                                    }
                                    pointer += (instr_num - 1) as usize;
                                    accumulator += instr_num as i128;
                                }
                            },
                        }
                    },
                    Operation::Namespace(ns) => {
                        space = *ns;
                        pointer += 1;
                    },
                }
                pointer += 1;
            },
            None => break,
        }
    }
}

// This assumes that branching does not clear the pipeline such that data races can happen.
// However the CPU does flush its pipeline, so that this is not needed. In this case we can
// just iterate over the instructions and clear the working set when branches are detected.
// We don't even need to follow unconditional branches. Cool!
// Do not remove!
/*
fn find_and_set_label_ns(namespace: &mut Namespaces, space: &usize, difference: &i128, label: String, direction: bool) {
    let testing_spaces = namespace.get_namespaces().filter(|b| b > space);
    for pot_space in testing_spaces {
        match namespace.get_label(label.clone(), Some(pot_space)) {
            Some(lablel) => {
                lablel.add_def(*difference);
                return;
            },
            None => (),
        }
    }
    find_and_set_label_ns(namespace, space, difference, label, !direction)
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
                                    find_and_set_label_ns(&mut code.0, &space, (), label.to_string(), part > 0);
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
}*/

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

pub fn optimize(mut code: (Namespaces, Vec<Operation>)) -> Vec<Instruction> {
    nop_insertion(&mut code);
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
                    0 => assert_eq!(nop_inserts, -1),
                    1 => assert_eq!(nop_inserts, 0),
                    2 => assert_eq!(nop_inserts, -1),
                    3 => assert_eq!(nop_inserts, 1),
                    4 => assert_eq!(nop_inserts, -1),
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
                0 => assert_eq!(nop_inserts, -1),
                1 => assert_eq!(nop_inserts, -1),
                2 => assert_eq!(nop_inserts, -1),
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
        operation_vec.push(Operation::Instr(Instruction::Addn(Reg::G0, Reg::G1, Reg::G12)));
        operation_vec.push(Operation::Instr(Instruction::Or(Reg::G2, Reg::G12, Reg::G12)));
        operation_vec.push(Operation::Instr(Instruction::Subn(Reg::G2, Reg::G12, Reg::G2)));
        operation_vec.push(Operation::Instr(Instruction::Addn(Reg::G15, Reg::G1, Reg::G12)));
        operation_vec.push(Operation::Instr(Instruction::Or(Reg::G2, Reg::G12, Reg::G12)));
        operation_vec.push(Operation::Instr(Instruction::Subn(Reg::G2, Reg::G12, Reg::G15)));

        let mut code = (namespace, operation_vec);

        nop_insertion(&mut code);

        // ##################################################################################

        let label_recog_ver = LabelRecog::new();

        let mut namespace_ver = Namespaces::new();
        let _ = namespace_ver.insert_recog(label_recog_ver);

        let mut operation_vec_ver: Vec<Operation> = vec![];
        operation_vec_ver.push(Operation::Namespace(0));
        operation_vec_ver.push(Operation::Instr(Instruction::Addn(Reg::G0, Reg::G1, Reg::G12)));
        operation_vec_ver.push(Operation::Instr(Instruction::Or(Reg::G2, Reg::G12, Reg::G12)));
        operation_vec_ver.push(Operation::Instr(Instruction::Addi(Reg::G0, Reg::G0, 0)));
        operation_vec_ver.push(Operation::Instr(Instruction::Addi(Reg::G0, Reg::G0, 0)));
        operation_vec_ver.push(Operation::Instr(Instruction::Addi(Reg::G0, Reg::G0, 0)));
        operation_vec_ver.push(Operation::Instr(Instruction::Subn(Reg::G2, Reg::G12, Reg::G2)));
        operation_vec_ver.push(Operation::Instr(Instruction::Addn(Reg::G15, Reg::G1, Reg::G12)));
        operation_vec_ver.push(Operation::Instr(Instruction::Or(Reg::G2, Reg::G12, Reg::G12)));
        operation_vec_ver.push(Operation::Instr(Instruction::Addi(Reg::G0, Reg::G0, 0)));
        operation_vec_ver.push(Operation::Instr(Instruction::Addi(Reg::G0, Reg::G0, 0)));
        operation_vec_ver.push(Operation::Instr(Instruction::Subn(Reg::G2, Reg::G12, Reg::G15)));

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
        operation_vec.push(Operation::Instr(Instruction::Addn(Reg::G0, Reg::G1, Reg::G12)));
        operation_vec.push(Operation::Instr(Instruction::Or(Reg::G2, Reg::G12, Reg::G12)));
        operation_vec.push(Operation::Instr(Instruction::Subn(Reg::G2, Reg::G12, Reg::G2)));
        operation_vec.push(Operation::Instr(Instruction::Addn(Reg::G15, Reg::G1, Reg::G12)));
        operation_vec.push(Operation::Macro(MacroInstr::Jal(Reg::G0, "END".to_string())));
        operation_vec.push(Operation::LablInstr(Cow::from("START"), Instruction::Or(Reg::G2, Reg::G12, Reg::G12)));
        operation_vec.push(Operation::Instr(Instruction::Subn(Reg::G2, Reg::G12, Reg::G15)));
        operation_vec.push(Operation::Namespace(1));
        operation_vec.push(Operation::Instr(Instruction::Addn(Reg::G0, Reg::G1, Reg::G12)));
        operation_vec.push(Operation::Instr(Instruction::Or(Reg::G2, Reg::G12, Reg::G12)));
        operation_vec.push(Operation::Instr(Instruction::Subn(Reg::G2, Reg::G12, Reg::G2)));
        operation_vec.push(Operation::LablInstr(Cow::from("END"), Instruction::Addn(Reg::G15, Reg::G1, Reg::G12)));
        operation_vec.push(Operation::Instr(Instruction::Or(Reg::G2, Reg::G12, Reg::G12)));
        operation_vec.push(Operation::Instr(Instruction::Subn(Reg::G2, Reg::G12, Reg::G15)));
        operation_vec.push(Operation::Macro(MacroInstr::Jal(Reg::G0, "START".to_string())));

        let mut code = (namespace, operation_vec);

        nop_insertion(&mut code);

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

        cond_add_acc_label(&mut namespace_ver, 3, Cow::from("START"), 0);
        cond_add_acc_label(&mut namespace_ver, 7, Cow::from("END"), 0);

        let operation_vec_ver: Vec<Operation> = Vec::from([
            Operation::Namespace(0),
            Operation::Instr(Instruction::Addn(Reg::G0, Reg::G1, Reg::G12)),
            Operation::Instr(Instruction::Or(Reg::G2, Reg::G12, Reg::G12)),
            Operation::Instr(Instruction::Addi(Reg::G0, Reg::G0, 0)),
            Operation::Instr(Instruction::Addi(Reg::G0, Reg::G0, 0)),
            Operation::Instr(Instruction::Addi(Reg::G0, Reg::G0, 0)),
            Operation::Instr(Instruction::Subn(Reg::G2, Reg::G12, Reg::G2)),
            Operation::Instr(Instruction::Addn(Reg::G15, Reg::G1, Reg::G12)),
            Operation::Macro(MacroInstr::Jal(Reg::G0, "END".to_string())),
            Operation::LablInstr(Cow::from("START"), Instruction::Or(Reg::G2, Reg::G12, Reg::G12)),
            Operation::Instr(Instruction::Subn(Reg::G2, Reg::G12, Reg::G15)),
            Operation::Namespace(1),
            Operation::Instr(Instruction::Addn(Reg::G0, Reg::G1, Reg::G12)),
            Operation::Instr(Instruction::Or(Reg::G2, Reg::G12, Reg::G12)),
            Operation::Instr(Instruction::Addi(Reg::G0, Reg::G0, 0)),
            Operation::Instr(Instruction::Addi(Reg::G0, Reg::G0, 0)),
            Operation::Instr(Instruction::Addi(Reg::G0, Reg::G0, 0)),
            Operation::Instr(Instruction::Subn(Reg::G2, Reg::G12, Reg::G2)),
            Operation::LablInstr(Cow::from("END"), Instruction::Addn(Reg::G15, Reg::G1, Reg::G12)),
            Operation::Instr(Instruction::Or(Reg::G2, Reg::G12, Reg::G12)),
            Operation::Instr(Instruction::Addi(Reg::G0, Reg::G0, 0)),
            Operation::Instr(Instruction::Addi(Reg::G0, Reg::G0, 0)),
            Operation::Instr(Instruction::Subn(Reg::G2, Reg::G12, Reg::G15)),
            Operation::Macro(MacroInstr::Jal(Reg::G0, "START".to_string())),
        ]);

        assert_eq!(code, (namespace_ver, operation_vec_ver));
    }
}