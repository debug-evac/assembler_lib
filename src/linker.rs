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
use crate::parser::{LabelRecog, Instruction, Operation};

pub struct LinkError {

}

fn check_link_possible(code1: &LabelRecog, code2: &LabelRecog) -> bool {
    code1.def_shadow(code2)
}

pub fn link(parsed_instr: Vec<(LabelRecog, Vec<Operation>)>) -> Result<(LabelRecog, Vec<Operation>), LinkError> {
    let mut gl_symbol_map = LabelRecog::new();
    let mut shadowed = false;

    for codes in parsed_instr.windows(2) {
        shadowed |= check_link_possible(&codes[0].0, &codes[1].0);
    }

    if shadowed {
        todo!("Implement error handling for overshadowed label definitions!");
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
