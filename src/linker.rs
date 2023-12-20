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
    collections::HashMap,
};
use crate::parser::{LabelRecog, Operation, LabelElem};

#[derive(Debug, PartialEq)]
pub struct Namespaces {
    global_definitions: Box<HashMap<String, usize>>,
    global_namespace: Box<Vec<LabelElem>>,
    namespaces: Box<Vec<LabelRecog>>,
}

impl Namespaces {
    pub fn new() -> Namespaces {
        let global_definitions: Box<HashMap<String, usize>> =
        Box::new(HashMap::new());
        let global_namespace: Box<Vec<LabelElem>> =
        Box::new(vec![]);
        let namespaces: Box<Vec<LabelRecog>> =
        Box::new(vec![]);

        Namespaces {
            global_definitions,
            global_namespace,
            namespaces
        }
    }

    pub fn insert_recog(&mut self, recog: LabelRecog) -> Result<&str, LinkError> {
        let other_gl_elems = recog.get_global_labels();
        let offset = recog.get_offset();
        for labelelem in other_gl_elems.iter() {
            let elem_name = labelelem.get_name();
            match self.global_definitions.get(elem_name) {
                Some(val) => {
                    match self.global_namespace.get_mut(*val) {
                        Some(gl_label) => {
                            match gl_label.combine(labelelem, offset) {
                                Ok(_) => (),
                                Err(msg) => return Err(LinkError {}),
                            }
                        },
                        None => (),
                    }
                },
                None => {
                    self.global_definitions.insert(elem_name.clone(), self.global_namespace.len());
                    self.global_namespace.push((*labelelem).clone());
                },
            }
        };

        // Possibly remove global labels from LabelRecog
        self.namespaces.push(recog);

        Ok("Success!")

    }

    pub fn get_recog(&mut self, space: usize) -> Option<&LabelRecog> {
        self.namespaces.get(space)
    }

    pub fn get_namespaces(&self) -> std::ops::Range<usize> {
        0..self.namespaces.len()
    }

    pub fn get_global_labels(&self) -> Box<Vec<LabelElem>> {
        self.global_namespace.clone()
    }
}

#[derive(Debug, PartialEq)]
pub struct LinkError {

}

pub fn link(mut parsed_instr: Vec<(LabelRecog, Vec<Operation>)>) -> Result<(Namespaces, Vec<Operation>), LinkError> {
    let mut new_code: (Namespaces, Vec<Operation>) = (Namespaces::new(), vec![]);
    let mut offset: Vec<usize> = vec![0; parsed_instr.len() + 1];

    let mut file_counter: usize = 0;

    // Break out into multiple functions? Probably not..
    for code in parsed_instr.iter_mut() {
        match offset.get(file_counter) {
            Some(val) => code.0.set_offset(*val),
            None => code.0.set_offset(0),
        };
        match new_code.0.insert_recog(code.0.clone()) {
            Ok(_) => (),
            Err(e) => return Err(e),
        }
        new_code.1.push(Operation::Namespace(Cow::from(file_counter.to_string())));
        new_code.1.extend(code.1.clone());
        offset.insert(file_counter + 1, code.1.len());
        file_counter = file_counter + 1;
    }

    // Check if all globals are defined!
    for gl_label in new_code.0.get_global_labels().iter() {
        if *gl_label.get_def() == -1 {
            return Err(LinkError {  });
        }
    }

    Ok(new_code)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::{Instruction, Reg};

    #[test]
    fn test_correct_link() {
        let mut parsed_vector: Vec<(LabelRecog, Vec<Operation>)> = vec![];
        let mut operation_vec_one: Vec<Operation> = vec![];
        operation_vec_one.push(Operation::LablInstr(Cow::from("SEHR_SCHOEN"), Instruction::VJmp("END".to_string())));
        operation_vec_one.push(Operation::Labl(Cow::from("END")));

        let mut label_recog_one = LabelRecog::new();

        let mut label = LabelElem::new();

        label.set_name("SEHR_SCHOEN".to_string());
        label.set_scope(true);
        label.set_def(0);
        label.add_ref(5);
        let _ = label_recog_one.insert_label(label);

        label = LabelElem::new();
        label.set_name("END".to_string());
        label.set_scope(false);
        label.set_def(1);
        label.add_ref(0);
        let _ = label_recog_one.insert_label(label);

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

        label = LabelElem::new();

        label.set_name("SEHR_SCHOEN".to_string());
        label.set_scope(true);
        label.add_ref(0);
        let _ = label_recog_two.insert_label(label);

        label = LabelElem::new();
        label.set_name("END".to_string());
        label.set_scope(false);
        label.set_def(1);
        let _ = label_recog_two.insert_label(label);

        /*
            Assembly file two:

            jmp SEHR_SCHOEN
            .END:
        */

        parsed_vector.push((label_recog_two.clone(), operation_vec_two));

        // ####################################################################

        let mut label_recog_ver1 = label_recog_one;
        let mut label_recog_ver2 = label_recog_two;

        label_recog_ver1.set_offset(0);
        label_recog_ver2.set_offset(2);

        let mut namespace_ver = Namespaces::new();

        let _ = namespace_ver.insert_recog(label_recog_ver1);
        let _ = namespace_ver.insert_recog(label_recog_ver2);

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

    #[test]
    fn test_multiple_defined_global() {
        let mut parsed_vector: Vec<(LabelRecog, Vec<Operation>)> = vec![];
        let mut operation_vec_one: Vec<Operation> = vec![];
        operation_vec_one.push(Operation::LablInstr(Cow::from("SEHR_SCHOEN"), Instruction::Addn(Reg::G0, Reg::G0, Reg::G1)));
        operation_vec_one.push(Operation::Instr(Instruction::Ld(Reg::G11, 56)));
        operation_vec_one.push(Operation::Instr(Instruction::Mov(Reg::G1, Reg::G11)));

        let mut label_recog_one = LabelRecog::new();

        let mut label = LabelElem::new();

        label.set_name("SEHR_SCHOEN".to_string());
        label.set_scope(true);
        label.set_def(0);
        let _ = label_recog_one.insert_label(label);

        /*
            Assembly file one:

            SEHR_SCHOEN: Add $0, $0, $1
            Ld $11, 56
            Mov $1, $11
        */

        parsed_vector.push((label_recog_one.clone(), operation_vec_one));

        let mut operation_vec_two: Vec<Operation> = vec![];

        operation_vec_two.push(Operation::LablInstr(Cow::from("SEHR_SCHOEN"), Instruction::Subn(Reg::G0, Reg::G0, Reg::G0)));
        operation_vec_two.push(Operation::Instr(Instruction::VJmp("SEHR_SCHOEN".to_string())));

        let mut label_recog_two = LabelRecog::new();

        label = LabelElem::new();

        label.set_name("SEHR_SCHOEN".to_string());
        label.set_scope(true);
        label.set_def(0);
        label.add_ref(1);
        let _ = label_recog_two.insert_label(label);

        /*
            Assembly file two:

            SEHR_SCHOEN: Subn $0, $0, $0
            jmp SEHR_SCHOEN
        */

        parsed_vector.push((label_recog_two.clone(), operation_vec_two));

        // ####################################################################

        /*
            Resulting assembly file:

            SEHR_SCHOEN: Add $0, $0, $1 # !!
            Ld $11, 56
            Mov $1, $11
            SEHR_SCHOEN: Subn $0, $0, $0 # !!
            jmp SEHR_SCHOEN
        */

        assert_eq!(Err(LinkError {}), link(parsed_vector));
    }

    #[test]
    fn test_no_defined_labels() {
        let mut parsed_vector: Vec<(LabelRecog, Vec<Operation>)> = vec![];
        let mut operation_vec_one: Vec<Operation> = vec![];
        operation_vec_one.push(Operation::Instr(Instruction::Addn(Reg::G0, Reg::G1, Reg::G12)));
        operation_vec_one.push(Operation::Instr(Instruction::Or(Reg::G2, Reg::G12, Reg::G12)));
        operation_vec_one.push(Operation::Instr(Instruction::Subn(Reg::G2, Reg::G12, Reg::G12)));

        let label_recog_one = LabelRecog::new();

        /*
            Assembly file one:

            Add $0, $1, $12
            Or $2, $12, $12
            Sub $3, $15, $15
        */

        parsed_vector.push((label_recog_one.clone(), operation_vec_one));

        let mut operation_vec_two: Vec<Operation> = vec![];

        operation_vec_two.push(Operation::Instr(Instruction::Addn(Reg::G0, Reg::G1, Reg::G12)));
        operation_vec_two.push(Operation::Instr(Instruction::Or(Reg::G2, Reg::G12, Reg::G12)));
        operation_vec_two.push(Operation::Instr(Instruction::Subn(Reg::G2, Reg::G12, Reg::G12)));

        let label_recog_two = LabelRecog::new();

        /*
            Assembly file two:

            Add $0, $1, $12
            Or $2, $12, $12
            Sub $3, $15, $15
        */

        parsed_vector.push((label_recog_two.clone(), operation_vec_two));

        // ####################################################################

        let mut label_recog_ver1 = label_recog_one;
        let mut label_recog_ver2 = label_recog_two;

        label_recog_ver1.set_offset(0);
        label_recog_ver2.set_offset(3);

        let mut namespace_ver = Namespaces::new();

        let _ = namespace_ver.insert_recog(label_recog_ver1);
        let _ = namespace_ver.insert_recog(label_recog_ver2);

        /*
            Resulting assembly file:

            Add $0, $1, $12
            Or $2, $12, $12
            Sub $3, $15, $15
            Add $0, $1, $12
            Or $2, $12, $12
            Sub $3, $15, $15
        */

        let mut operation_vec_ver: Vec<Operation> = vec![];

        operation_vec_ver.push(Operation::Namespace(Cow::from("0")));
        operation_vec_ver.push(Operation::Instr(Instruction::Addn(Reg::G0, Reg::G1, Reg::G12)));
        operation_vec_ver.push(Operation::Instr(Instruction::Or(Reg::G2, Reg::G12, Reg::G12)));
        operation_vec_ver.push(Operation::Instr(Instruction::Subn(Reg::G2, Reg::G12, Reg::G12)));
        operation_vec_ver.push(Operation::Namespace(Cow::from("1")));
        operation_vec_ver.push(Operation::Instr(Instruction::Addn(Reg::G0, Reg::G1, Reg::G12)));
        operation_vec_ver.push(Operation::Instr(Instruction::Or(Reg::G2, Reg::G12, Reg::G12)));
        operation_vec_ver.push(Operation::Instr(Instruction::Subn(Reg::G2, Reg::G12, Reg::G12)));

        assert_eq!((namespace_ver, operation_vec_ver), link(parsed_vector).unwrap());
    }

    // Some more tests?
}