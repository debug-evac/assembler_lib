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
// operations into shoter-running operations), among other
// things. This will run on every compilation (? rather
// translations), however changes to the programm will only be
// made, if the programmer includes an (or possibly a number of)
// flag(s). If there are no flags, the assembler will simply
// issue a notice that performance COULD be gained if
// restructuring the assembly code (can be suppressed by
// another flag).

use crate::{
    parser::Instruction, 
    parser::Operation, 
    linker::Namespaces
};

fn translate_label(label: String, namespaces: &mut Namespaces, space: usize) -> i32 {
    match namespaces.get_label(label, Some(space)) {
        Some(label_elem) => <i128 as TryInto<i32>>::try_into(*label_elem.get_def()).unwrap(),
        None => i32::MIN,
    }
}

fn substitute_labels(mut code: (Namespaces, Vec<Operation>)) -> Vec<Instruction> {
    let mut instructions: Vec<Instruction> = vec![];
    let mut namespace: usize = 0;

    /*
    for operation in code.1 {
        match operation {
            Operation::Namespace(space) => namespace = space,
            Operation::Macro(instr) | Operation::LablMacro(_, instr) => {
                let tr_instr = match instr {
                    Instruction::VJmp(labl) => Instruction::Jmp(translate_label(labl, &mut code.0, namespace)),
                    Instruction::VBt(labl) => Instruction::Bt(translate_label(labl, &mut code.0, namespace)),
                    Instruction::VBf(labl) => Instruction::Bf(translate_label(labl, &mut code.0, namespace)),
                    Instruction::VBeq(reg1, reg2, labl) => Instruction::Beq(reg1, reg2, 
                        translate_label(labl, &mut code.0, namespace)),
                    Instruction::VBne(reg1, reg2, labl) => Instruction::Bne(reg1, reg2, 
                        translate_label(labl, &mut code.0, namespace)),
                    Instruction::VBlt(reg1, reg2, labl) => Instruction::Blt(reg1, reg2, 
                        translate_label(labl, &mut code.0, namespace)),
                    Instruction::VBltu(reg1, reg2, labl) => Instruction::Bltu(reg1, reg2, 
                        translate_label(labl, &mut code.0, namespace)),
                    Instruction::VBge(reg1, reg2, labl) => Instruction::Bge(reg1, reg2, 
                        translate_label(labl, &mut code.0, namespace)),
                    Instruction::VBgeu(reg1, reg2, labl) => Instruction::Bgeu(reg1, reg2, 
                        translate_label(labl, &mut code.0, namespace)),

                    _ => instr,
                };

                instructions.push(tr_instr)
            },
            Operation::Labl(_) => (),
        };
    }*/

    return instructions;
}

pub fn optimize(code: (Namespaces, Vec<Operation>)) -> Vec<Instruction> {
    return substitute_labels(code);
}

// TODO: Tests here