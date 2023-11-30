/*
 * Copyright (c) 2023 Steven Becker
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

//
// Links multiple sources that have been parsed together to one source.
// Will require some rewrites of the parser to retrieve symbol maps and
// symbol res map. Might also require parser to not replace labels with
// absolute addresses.
// Linked files need to be specified and ordered from flags or a
// particular file (probably flags only, but we'll see).
use std::borrow::Cow;
//use std::collections::HashSet;
use crate::parser::{LabelRecog, Instruction, Operation};

pub struct LinkError {

}

/*
fn check_link_possible(code1: &LabelRecog, code2: &LabelRecog) -> bool {
    code1.def_shadow(code2)
}*/

// TODO: Move to parser
fn replace_label(code: (LabelRecog, Vec<Operation>), new_label: String) -> () {
    for pos in code.0.get_poss(new_label) {
        code.1[*pos] = match code.1[*pos] {
            Operation::Instr(instr) => {
                let new_instr = match instr {
                    Instruction::VJmp(_) => Instruction::VJmp(new_label),
                    Instruction::VBt(_) => Instruction::VBt(new_label),
                    Instruction::VBf(_) => Instruction::VBf(new_label),

                    // TODO: Virtual instructions

                    instr => instr,
                };
                Operation::Instr(new_instr)
            },
            Operation::Labl(_) => Operation::Labl(Cow::from(new_label)),
            Operation::LablInstr(_, instr) => {
                let new_instr = match instr {
                    Instruction::VJmp(_) => Instruction::VJmp(new_label),
                    Instruction::VBt(_) => Instruction::VBt(new_label),
                    Instruction::VBf(_) => Instruction::VBf(new_label),

                    // TODO: Virtual instructions

                    instr => instr,
                };
                Operation::LablInstr(Cow::from(new_label), new_instr)
            },
        }
    }
}

pub fn link(parsed_instr: Vec<(LabelRecog, Vec<Operation>)>) -> Result<(LabelRecog, Vec<Operation>), LinkError> {
    let mut gl_symbol_map = LabelRecog::new();
    let mut total_code: Vec<Operation> = vec![];
    let mut offset: usize = 0;
    let mut total_label = 0;

    for code in parsed_instr {
        total_label += code.0.get_gdefs().len();
        gl_symbol_map.extend_gdefs(&code.0);
    }

    let after_union = gl_symbol_map.get_gdefs().into_iter().count();

    if after_union != total_label {
        return Err(LinkError {})
    }

    let mut conflict_counter: u128 = 0;

    for code in parsed_instr {
        if gl_symbol_map.gllc_def_shadowing(&code.0) {
            if gl_symbol_map.lclc_def_shadowing(&code.0) {
                // trivial, both sets are disjoint
                gl_symbol_map.extend_ldefs(&code.0);
            } else {
                // non-trivial, both sets contain some labels that are equal
                let gllc_labels = gl_symbol_map.get_ldefs();
                let lclc_labels = code.0.get_ldefs();
                let labelunion = gllc_labels.union(&lclc_labels);
                for label in labelunion {
                    let mut gl_new_label = label.to_string();
                    gl_new_label.push_str("_");
                    let mut lc_new_label = gl_new_label.clone();
                    gl_new_label.push_str(&conflict_counter.to_string());
                    replace_label(code, lc_new_label);
                    let local_conflict_counter = conflict_counter + 1;
                    lc_new_label.push_str(&local_conflict_counter.to_string());
                    gl_symbol_map.redefine_def(false, label, gl_new_label);
                    code.0.redefine_def(false, label, lc_new_label);
                    replace_label(code, lc_new_label);
                }
                gl_symbol_map.extend_ldefs(&code.0);
                conflict_counter = conflict_counter + 1;
            }
            total_code.extend(code.1);
        } else {
            return Err(LinkError {  })
        }
    }

    let mut counter = 0;
    let mut offset = 0;

    // Optimization possibility? Check which labels overlap and only rename those
    // that overlap (local to local)
    while counter < parsed_instr.len() {
        let symbol_map = parsed_instr[counter].0;
        for labl in symbol_map.get_ldefs().into_iter() {
            gl_symbol_map.insert_def(false, Cow::from(labl));
            symbol_map.get_pos_vec(labl); // Get label positions, change labels at positions and
                                          // add offset to pos
        }

        counter += 1;
    }

    todo!("TODO!")
}
