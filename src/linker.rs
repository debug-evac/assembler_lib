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
use std::collections::HashMap;
use crate::common::{LabelRecog, Operation, LabelElem};

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
        for labelelem in other_gl_elems.iter() {
            let elem_name = labelelem.get_name();
            match self.global_definitions.get(elem_name) {
                Some(val) => {
                    match self.global_namespace.get_mut(*val) {
                        Some(gl_label) => {
                            match gl_label.combine(labelelem) {
                                Ok(_) => (),
                                Err(_msg) => return Err(LinkError {}),
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

    pub fn get_label(&mut self, label: String, space: Option<usize>) -> Option<&mut LabelElem> {
        match self.global_definitions.get(&label) {
            Some(pos) => {
                self.global_namespace.get_mut(*pos)
            },
            None => {
                if let Some(pos) = space {
                    match self.namespaces.get_mut(pos) {
                        Some(recog) => recog.get_label(&label),
                        None => None,
                    }
                } else {
                    None
                }
            },
        }
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
            Some(val) => code.0.add_offset(*val as i128),
            None => (),
        };
        match new_code.0.insert_recog(code.0.clone()) {
            Ok(_) => (),
            Err(e) => return Err(e),
        }
        new_code.1.push(Operation::Namespace(file_counter));
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
    use std::borrow::Cow;
    use crate::common::{Instruction, Reg, MacroInstr};

    #[test]
    fn test_correct_link() {
        let mut parsed_vector: Vec<(LabelRecog, Vec<Operation>)> = vec![];
        let mut operation_vec_one: Vec<Operation> = vec![];
        operation_vec_one.push(Operation::LablMacro(Cow::from("SEHR_SCHOEN"), MacroInstr::Jl("END".to_string())));
        operation_vec_one.push(Operation::Labl(Cow::from("END")));

        let mut label_recog_one = LabelRecog::new();

        let mut label = LabelElem::new_refd("SEHR_SCHOEN".to_string());
        label.set_scope(true);
        label.set_def(0);
        let _ = label_recog_one.insert_label(label);

        label = LabelElem::new_refd("END".to_string());
        label.set_scope(false);
        label.set_def(1);
        let _ = label_recog_one.insert_label(label);

        /*
            Assembly file one:

            SEHR_SCHOEN: jmp END
            .END:
        */

        parsed_vector.push((label_recog_one.clone(), operation_vec_one));

        let mut operation_vec_two: Vec<Operation> = vec![];

        operation_vec_two.push(Operation::Macro(MacroInstr::Jl("SEHR_SCHOEN".to_string())));
        operation_vec_two.push(Operation::Labl(Cow::from("END")));

        let mut label_recog_two = LabelRecog::new();

        label = LabelElem::new_refd("SEHR_SCHOEN".to_string());
        label.set_scope(true);
        let _ = label_recog_two.insert_label(label);

        label = LabelElem::new_def("END".to_string(), 1);
        label.set_scope(false);
        let _ = label_recog_two.insert_label(label);

        /*
            Assembly file two:

            jmp SEHR_SCHOEN
            .END:
        */

        parsed_vector.push((label_recog_two.clone(), operation_vec_two));

        // ####################################################################

        let label_recog_ver1 = label_recog_one;
        let mut label_recog_ver2 = label_recog_two;

        label_recog_ver2.add_offset(2);

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

        operation_vec_ver.push(Operation::Namespace(0));
        operation_vec_ver.push(Operation::LablMacro(Cow::from("SEHR_SCHOEN"), MacroInstr::Jl("END".to_string())));
        operation_vec_ver.push(Operation::Labl(Cow::from("END")));
        operation_vec_ver.push(Operation::Namespace(1));
        operation_vec_ver.push(Operation::Macro(MacroInstr::Jl("SEHR_SCHOEN".to_string())));
        operation_vec_ver.push(Operation::Labl(Cow::from("END")));

        assert_eq!((namespace_ver, operation_vec_ver), link(parsed_vector).unwrap());
    }

    #[test]
    fn test_multiple_defined_global() {
        let mut parsed_vector: Vec<(LabelRecog, Vec<Operation>)> = vec![];
        let mut operation_vec_one: Vec<Operation> = vec![];
        operation_vec_one.push(Operation::LablInstr(Cow::from("SEHR_SCHOEN"), Instruction::Addn(Reg::G0, Reg::G0, Reg::G1)));
        operation_vec_one.push(Operation::Instr(Instruction::Lb(Reg::G11, Reg::G12, 56)));
        operation_vec_one.push(Operation::Instr(Instruction::Addi(Reg::G1, Reg::G11, 0)));

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
        operation_vec_two.push(Operation::Macro(MacroInstr::Jl("SEHR_SCHOEN".to_string())));

        let mut label_recog_two = LabelRecog::new();

        label = LabelElem::new_refd("SEHR_SCHOEN".to_string());
        label.set_scope(true);
        label.set_def(0);
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

        let label_recog_ver1 = label_recog_one;
        let mut label_recog_ver2 = label_recog_two;

        label_recog_ver2.add_offset(3);

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

        operation_vec_ver.push(Operation::Namespace(0));
        operation_vec_ver.push(Operation::Instr(Instruction::Addn(Reg::G0, Reg::G1, Reg::G12)));
        operation_vec_ver.push(Operation::Instr(Instruction::Or(Reg::G2, Reg::G12, Reg::G12)));
        operation_vec_ver.push(Operation::Instr(Instruction::Subn(Reg::G2, Reg::G12, Reg::G12)));
        operation_vec_ver.push(Operation::Namespace(1));
        operation_vec_ver.push(Operation::Instr(Instruction::Addn(Reg::G0, Reg::G1, Reg::G12)));
        operation_vec_ver.push(Operation::Instr(Instruction::Or(Reg::G2, Reg::G12, Reg::G12)));
        operation_vec_ver.push(Operation::Instr(Instruction::Subn(Reg::G2, Reg::G12, Reg::G12)));

        assert_eq!((namespace_ver, operation_vec_ver), link(parsed_vector).unwrap());
    }

    #[test]
    fn test_single_link() {
        let mut parsed_vector: Vec<(LabelRecog, Vec<Operation>)> = vec![];
        let mut operation_vec: Vec<Operation> = vec![];
        operation_vec.push(Operation::Instr(Instruction::Addn(Reg::G0, Reg::G1, Reg::G12)));
        operation_vec.push(Operation::Instr(Instruction::Or(Reg::G2, Reg::G12, Reg::G12)));
        operation_vec.push(Operation::Instr(Instruction::Subn(Reg::G2, Reg::G12, Reg::G12)));
        operation_vec.push(Operation::Instr(Instruction::Addn(Reg::G0, Reg::G1, Reg::G12)));
        operation_vec.push(Operation::Instr(Instruction::Or(Reg::G2, Reg::G12, Reg::G12)));
        operation_vec.push(Operation::Instr(Instruction::Subn(Reg::G2, Reg::G12, Reg::G12)));

        let label_recog = LabelRecog::new();

        /*
            Assembly file:

            Add $0, $1, $12
            Or $2, $12, $12
            Sub $3, $15, $15
            Add $0, $1, $12
            Or $2, $12, $12
            Sub $3, $15, $15
        */

        parsed_vector.push((label_recog.clone(), operation_vec));

        let label_recog_ver = label_recog;

        let mut namespace_ver = Namespaces::new();
        let _ = namespace_ver.insert_recog(label_recog_ver);

        let mut operation_vec_ver: Vec<Operation> = vec![];

        operation_vec_ver.push(Operation::Namespace(0));
        operation_vec_ver.push(Operation::Instr(Instruction::Addn(Reg::G0, Reg::G1, Reg::G12)));
        operation_vec_ver.push(Operation::Instr(Instruction::Or(Reg::G2, Reg::G12, Reg::G12)));
        operation_vec_ver.push(Operation::Instr(Instruction::Subn(Reg::G2, Reg::G12, Reg::G12)));
        operation_vec_ver.push(Operation::Instr(Instruction::Addn(Reg::G0, Reg::G1, Reg::G12)));
        operation_vec_ver.push(Operation::Instr(Instruction::Or(Reg::G2, Reg::G12, Reg::G12)));
        operation_vec_ver.push(Operation::Instr(Instruction::Subn(Reg::G2, Reg::G12, Reg::G12)));

        assert_eq!((namespace_ver, operation_vec_ver), link(parsed_vector).unwrap());
    }
}