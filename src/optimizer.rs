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
            output.1.push(Operation::Instr(Instruction::Lw(Reg::G5, 10)));
            output.1.push(Operation::Instr(Instruction::VBge((), (), ())))

        },
        None => {
            output.0.push(Operation::Instr(Instruction::VJmp("_mul".to_string()))); // Jump subroutine
            output.1.push(Operation::Instr(Instruction::Lw(Reg::G5, 10)));
            output.1.push(Operation::Instr(Instruction::VBge(match(op),Reg2,"_mul".to_string()))); //warum match(op)? 
            output.1.push(Operation::Instr(Instruction::And(Reg)));
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