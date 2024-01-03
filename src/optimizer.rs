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

enum RegActType {
    NA,
    WriteRead2(Reg, Reg, Reg),
    WriteRead(Reg, Reg),
    Read2(Reg, Reg),
    Write(Reg),
    Store(Reg, Reg),
    Load(Reg, Reg)
}

struct RegAct {
    reg: RegActType,
    pos: usize
}

impl RegAct {
    fn new_from_macro(macro_in: MacroInstr, pos: usize) -> RegAct {
        let reg = macro_in.into();
        RegAct { 
            reg,
            pos 
        }
    }
}

struct LimitedQueue {
    size: usize,
    queue: VecDeque<RegAct>
}

impl LimitedQueue {
    fn new_sized(size: usize) -> LimitedQueue {
        let queue = VecDeque::with_capacity(size);
        LimitedQueue{ size, queue }
    }


}

impl From<MacroInstr> for RegActType {
    fn from(item: MacroInstr) -> Self {
        match item {
            MacroInstr::Beq(reg1, reg2, _) |
            MacroInstr::Bne(reg1, reg2, _) |
            MacroInstr::Blt(reg1, reg2, _) |
            MacroInstr::Bltu(reg1, reg2, _) |
            MacroInstr::Bge(reg1, reg2, _) |
            MacroInstr::Bgeu(reg1, reg2, _) => RegActType::Read2(reg1, reg2),

            MacroInstr::Jal(_, _) => todo!(),
            MacroInstr::Jalr(_, _, _) => todo!(),

            MacroInstr::Lui(_, _) => todo!(),
            MacroInstr::Auipc(_, _) => todo!(),
            MacroInstr::Slli(_, _, _) => todo!(),
            MacroInstr::Srli(_, _, _) => todo!(),
            MacroInstr::Srai(_, _, _) => todo!(),
            
            MacroInstr::Lb(_, _, _) => todo!(),
            MacroInstr::Lh(_, _, _) => todo!(),
            MacroInstr::Lw(_, _, _) => todo!(),
            MacroInstr::Lbu(_, _, _) => todo!(),
            MacroInstr::Lhu(_, _, _) => todo!(),
            MacroInstr::Sb(_, _, _) => todo!(),
            MacroInstr::Sh(_, _, _) => todo!(),
            MacroInstr::Sw(_, _, _) => todo!(),
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

fn nop_insertion(mut code: (Namespaces, Vec<Operation>)) -> (Namespaces, Vec<Operation>) {
    /*let mut code_inserts: BTreeMap<usize, i128> = BTreeMap::new();
    let mut label_pos: BTreeMap<usize, LabelElem> = BTreeMap::new();

    let mut working_set: VecDeque<Operation> = VecDeque::new();
    let mut branch_ptr_queue: VecDeque<usize> = VecDeque::new();

    loop {
        let instr = 

        positions.0 = positions.1;
        positions.1 = positions.1 + 1;
    }*/

    //let mut reg_dep_graph: [Vec<RegAct>; 31] = Default::default();



    code
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

// TODO: Tests here
#[cfg(test)]
mod tests {

}