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
use std::{
    borrow::Cow,
    collections::{
        HashMap,
        HashSet,
    },
};
use crate::parser::{LabelStr, LabelRecog, Operation};

#[derive(Debug, PartialEq)]
pub struct Namespaces {
    global_definitions: Box<HashSet<LabelStr>>,
    namespaces: Box<HashSet<LabelStr>>,
    ind_symbol_sets: Box<HashMap<LabelStr, LabelRecog>>,
}

impl Namespaces {
    pub fn new() -> Namespaces {
        let global_definitions: Box<HashSet<LabelStr>> =
        Box::new(HashSet::new());
        let namespaces: Box<HashSet<LabelStr>> =
        Box::new(HashSet::new());
        let ind_symbol_sets: Box<HashMap<LabelStr, LabelRecog>> =
        Box::new(HashMap::new());

        Namespaces {
            global_definitions,
            namespaces,
            ind_symbol_sets
        }
    }

    pub fn insert_recog(&mut self, space: &LabelStr, recog: LabelRecog) -> Result<String, LinkError> {
        if self.global_definitions.is_disjoint(&recog.get_gdefs()) &&
        self.global_definitions.is_disjoint(&recog.get_ldefs()) && !self.namespaces.contains(space) {
            self.global_definitions.extend(recog.get_gdefs());
            self.namespaces.insert(space.clone());
            self.ind_symbol_sets.insert(space.clone(), recog);

            Ok("Purfect".to_string())
        } else {
            Err(LinkError { })
        }
    }

    pub fn get_recog(&mut self, space: &LabelStr) -> Option<&LabelRecog> {
        self.ind_symbol_sets.get(space)
    }

    pub fn get_namespaces(&self) -> HashSet<LabelStr> {
        *self.namespaces.clone()
    }
}

#[derive(Debug)]
pub struct LinkError {

}

pub fn link(mut parsed_instr: Vec<(LabelRecog, Vec<Operation>)>) -> Result<(Namespaces, Vec<Operation>), LinkError> {
    let mut new_code: (Namespaces, Vec<Operation>) = (Namespaces::new(), vec![]);
    let mut offset: Vec<usize> = vec![0; parsed_instr.len() + 1];

    let mut file_counter: usize = 0;

    // Break out into multiple functions?
    for code in parsed_instr.iter_mut() {
        match offset.get(file_counter) {
            Some(val) => code.0.set_offset(*val as u128),
            None => code.0.set_offset(0),
        };
        let _ = new_code.0.insert_recog(&file_counter.to_string(), code.0.clone());
        new_code.1.push(Operation::Namespace(Cow::from(file_counter.to_string())));
        new_code.1.extend(code.1.clone());
        offset.insert(file_counter + 1, code.1.len());
        file_counter = file_counter + 1;
    }

    Ok(new_code)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::Instruction;

    #[test]
    fn test_correct_link() {
        let mut parsed_vector: Vec<(LabelRecog, Vec<Operation>)> = vec![];
        let mut operation_vec_one: Vec<Operation> = vec![];
        operation_vec_one.push(Operation::LablInstr(Cow::from("SEHR_SCHOEN"), Instruction::VJmp("END".to_string())));
        operation_vec_one.push(Operation::Labl(Cow::from("END")));

        let mut label_recog_one = LabelRecog::new();
        let _ = label_recog_one.insert_def(true, Cow::from("SEHR_SCHOEN"));
        let _ = label_recog_one.insert_def(false, Cow::from("END"));

        label_recog_one.insert_pos("SEHR_SCHOEN".to_string(), &(0 as usize));
        label_recog_one.insert_pos("END".to_string(), &(0 as usize));
        label_recog_one.insert_pos("END".to_string(), &(1 as usize));

        /*
            Assembly file one:

            SEHR_SCHOEN: jmp END
            .END:
        */

        parsed_vector.push((label_recog_one.clone(), operation_vec_one));

        let mut operation_vec_two: Vec<Operation> = vec![];

        operation_vec_two.push(Operation::Instr(Instruction::VJmp("SEHR_SCHOEN".to_string())));
        operation_vec_two.push(Operation::Labl(Cow::from("END")));

        let mut label_recog_two = LabelRecog::new();
        let _ = label_recog_two.insert_def(false, Cow::from("END"));

        /*
            Assembly file two:

            jmp SEHR_SCHOEN
            .END:
        */

        label_recog_two.insert_pos("SEHR_SCHOEN".to_string(), &(0 as usize));
        label_recog_two.insert_pos("END".to_string(), &(1 as usize));

        parsed_vector.push((label_recog_two.clone(), operation_vec_two));

        // ####################################################################

        let mut label_recog_ver1 = label_recog_one;
        let mut label_recog_ver2 = label_recog_two;

        label_recog_ver1.set_offset(0);
        label_recog_ver2.set_offset(2);

        let mut namespace_ver = Namespaces::new();

        let _ = namespace_ver.insert_recog(&"0".to_string(), label_recog_ver1);
        let _ = namespace_ver.insert_recog(&"1".to_string(), label_recog_ver2);

        /*
            Resulting assembly file:

            SEHR_SCHOEN: jmp END
            .END:
            jmp SEHR_SCHOEN
            .END:
        */

        let mut operation_vec_ver: Vec<Operation> = vec![];

        operation_vec_ver.push(Operation::Namespace(Cow::from("0")));
        operation_vec_ver.push(Operation::LablInstr(Cow::from("SEHR_SCHOEN"), Instruction::VJmp("END".to_string())));
        operation_vec_ver.push(Operation::Labl(Cow::from("END")));
        operation_vec_ver.push(Operation::Namespace(Cow::from("1")));
        operation_vec_ver.push(Operation::Instr(Instruction::VJmp("SEHR_SCHOEN".to_string())));
        operation_vec_ver.push(Operation::Labl(Cow::from("END")));

        assert_eq!((namespace_ver, operation_vec_ver), link(parsed_vector).unwrap());
    }

    // TODO: Add more test cases
}
