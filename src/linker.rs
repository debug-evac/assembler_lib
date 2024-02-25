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
use std::fmt::Display;
use log::debug;

use crate::common::{LabelType, MemData, AssemblyCode};
use crate::common::{RestrictLabelData, LabelRecog, Operation, LabelElem,
    errors::LinkError
};

#[derive(Debug, Clone, PartialEq)]
pub struct Namespaces {
    global_definitions: HashMap<String, usize>,
    global_namespace: Vec<LabelElem>,
    namespaces: Vec<LabelRecog>,
}

impl Namespaces {
    pub fn new() -> Namespaces {
        let global_definitions: HashMap<String, usize> = HashMap::new();
        let global_namespace: Vec<LabelElem> = vec![];
        let namespaces: Vec<LabelRecog> = vec![];

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
                    if let Some(gl_label) = self.global_namespace.get_mut(*val) {
                        gl_label.combine(labelelem)?;
                        debug!("Successfully merged global label {:?} with {:?}", gl_label, labelelem);
                    }
                },
                None => {
                    self.global_definitions.insert(elem_name.clone(), self.global_namespace.len());
                    self.global_namespace.push((*labelelem).clone());
                },
            }
        };

        debug!("Successfully inserted '{recog}' into Namespace");

        // Possibly remove global labels from LabelRecog
        self.namespaces.push(recog);

        Ok("Success!")

    }

    #[allow(dead_code)]
    pub fn get_recog(&mut self, space: usize) -> Option<&LabelRecog> {
        self.namespaces.get(space)
    }

    #[allow(dead_code)]
    pub fn get_namespaces(&self) -> std::ops::Range<usize> {
        0..self.namespaces.len()
    }

    pub fn get_global_labels(&self) -> Vec<LabelElem> {
        self.global_namespace.clone()
    }

    pub fn get_label(&mut self, label: String, space: Option<usize>) -> Option<&mut LabelElem> {
        match self.global_definitions.get(&label) {
            Some(pos) => {
                let labelel = self.global_namespace.get_mut(*pos)?;
                if *labelel.get_type() == LabelType::Uninit {
                    None
                } else {
                    Some(labelel)
                }
            },
            None => {
                let pos = space?;
                let recog = self.namespaces.get_mut(pos)?;
                let labelel = recog.get_label(&label)?;
                if *labelel.get_type() == LabelType::Uninit {
                    None
                } else {
                    Some(labelel)
                }
            },
        }
    }
}

impl RestrictLabelData for Namespaces {}

impl Display for Namespaces {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Namespace {{\nglobals: {:?},\nnamespaces: [{}", self.global_namespace, self.namespaces[0])?;
        for labelrec in &self.namespaces[1..] {
            write!(f, ", {labelrec}")?;
        }
        write!(f, "] }}")
    }
}

impl Default for Namespaces {
    fn default() -> Self {
        Self::new()
    }
}

pub fn link(mut parsed_instr: Vec<AssemblyCode<LabelRecog>>) -> Result<AssemblyCode<Namespaces>, LinkError> {
    let mut new_assembly = AssemblyCode::new(Namespaces::new());
    let mut text_offset: usize = 0;
    let mut data_offset: usize = 0;

    // Break out into multiple functions? Probably not..
    for (file_counter, code) in parsed_instr.iter_mut().enumerate() {
        if text_offset > 0  {
            debug!("Added address offset {text_offset} to file {file_counter}");
            code.get_labels_refmut().add_offset(text_offset as i128, LabelType::Address);
        };

        if data_offset > 0 {
            debug!("Added data offset {data_offset} to file {file_counter}");
            code.get_labels_refmut().add_offset(data_offset as i128, LabelType::Data);
        };

        new_assembly.get_labels_refmut().insert_recog(code.get_labels_refmut().clone())?;

        let existent = code.get_data_refmut().len();
        if existent > 0 {
            new_assembly.get_data_refmut().push(MemData::Namespace(file_counter));
            new_assembly.get_data_refmut().extend(code.get_data_refmut().clone());

            data_offset += existent;
        }

        new_assembly.get_text_refmut().push(Operation::Namespace(file_counter));
        new_assembly.get_text_refmut().extend(code.get_text_refmut().clone());

        text_offset += code.get_text_refmut().len();
    }

    // Check if all globals are defined!
    for gl_label in new_assembly.get_labels_refmut().get_global_labels().iter() {
        if *gl_label.get_def() == -1 {
            return Err(LinkError::UndefinedGlobal(gl_label.clone()));
        }
    }

    debug!("{}", new_assembly.get_labels_refmut());

    debug!("Finished linking step");

    Ok(new_assembly)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::borrow::Cow;
    use crate::common::{Instruction, Reg, MacroInstr,
        errors::{LinkError, CommonError},
    };

    #[test]
    fn test_correct_link() {
        let mut parsed_vector: Vec<AssemblyCode<LabelRecog>> = vec![];

        let mut assembly_code_one = AssemblyCode::new(LabelRecog::new());

        let operation_vec_one = assembly_code_one.get_text_refmut();
        operation_vec_one.push(Operation::LablMacro(Cow::from("SEHR_SCHOEN"), MacroInstr::Jal(Reg::G1, "END".to_string())));
        operation_vec_one.push(Operation::Labl(Cow::from("END")));

        let label_recog_ver1;

        {
            let label_recog_one = assembly_code_one.get_labels_refmut();

            let mut label = LabelElem::new_refd("SEHR_SCHOEN".to_string());
            label.set_scope(true);
            label.set_def(0);
            let _ = label_recog_one.insert_label(label);

            label = LabelElem::new_refd("END".to_string());
            label.set_scope(false);
            label.set_def(1);
            let _ = label_recog_one.insert_label(label);

            label_recog_ver1 = label_recog_one.clone();
        }

        /*
            Assembly file one:

            SEHR_SCHOEN: jmp END
            .END:
        */

        parsed_vector.push(assembly_code_one);

        let mut assembly_code_two = AssemblyCode::new(LabelRecog::new());

        let operation_vec_two = assembly_code_two.get_text_refmut();

        operation_vec_two.push(Operation::Macro(MacroInstr::Jal(Reg::G1, "SEHR_SCHOEN".to_string())));
        operation_vec_two.push(Operation::Labl(Cow::from("END")));

        let mut label_recog_ver2;

        {
            let label_recog_two = assembly_code_two.get_labels_refmut();

            let mut label = LabelElem::new_refd("SEHR_SCHOEN".to_string());
            label.set_scope(true);
            let _ = label_recog_two.insert_label(label);

            label = LabelElem::new_def("END".to_string(), 1);
            label.set_scope(false);
            let _ = label_recog_two.insert_label(label);

            label_recog_ver2 = label_recog_two.clone();
        }

        /*
            Assembly file two:

            jmp SEHR_SCHOEN
            .END:
        */

        parsed_vector.push(assembly_code_two);

        // ####################################################################

        let mut assembly_code_ver = AssemblyCode::new(Namespaces::new());

        label_recog_ver2.add_offset(2, LabelType::Address);

        let namespace_ver = assembly_code_ver.get_labels_refmut();

        let _ = namespace_ver.insert_recog(label_recog_ver1.clone());
        let _ = namespace_ver.insert_recog(label_recog_ver2.clone());

        /*
            Resulting assembly file:

            SEHR_SCHOEN: jmp END
            .END:
            jmp SEHR_SCHOEN
            .END:
        */

        let operation_vec_ver = assembly_code_ver.get_text_refmut();

        operation_vec_ver.push(Operation::Namespace(0));
        operation_vec_ver.push(Operation::LablMacro(Cow::from("SEHR_SCHOEN"), MacroInstr::Jal(Reg::G1, "END".to_string())));
        operation_vec_ver.push(Operation::Labl(Cow::from("END")));
        operation_vec_ver.push(Operation::Namespace(1));
        operation_vec_ver.push(Operation::Macro(MacroInstr::Jal(Reg::G1, "SEHR_SCHOEN".to_string())));
        operation_vec_ver.push(Operation::Labl(Cow::from("END")));

        assert_eq!(assembly_code_ver, link(parsed_vector).unwrap())
    }

    #[test]
    fn test_multiple_defined_global() {
        let mut parsed_vector: Vec<AssemblyCode<LabelRecog>> = vec![];

        let mut assembly_code_one = AssemblyCode::new(LabelRecog::new());

        let operation_vec_one = assembly_code_one.get_text_refmut();
        operation_vec_one.push(Operation::LablInstr(Cow::from("SEHR_SCHOEN"), Instruction::Add(Reg::G0, Reg::G0, Reg::G1)));
        operation_vec_one.push(Operation::Instr(Instruction::Lb(Reg::G11, Reg::G12, 56)));
        operation_vec_one.push(Operation::Instr(Instruction::Addi(Reg::G1, Reg::G11, 0)));

        let label_recog_one = assembly_code_one.get_labels_refmut();

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

        parsed_vector.push(assembly_code_one);

        let mut assembly_code_two = AssemblyCode::new(LabelRecog::new());

        let operation_vec_two = assembly_code_two.get_text_refmut();

        operation_vec_two.push(Operation::LablInstr(Cow::from("SEHR_SCHOEN"), Instruction::Sub(Reg::G0, Reg::G0, Reg::G0)));
        operation_vec_two.push(Operation::Macro(MacroInstr::Jal(Reg::G1, "SEHR_SCHOEN".to_string())));

        let label_recog_two = assembly_code_two.get_labels_refmut();

        label = LabelElem::new_refd("SEHR_SCHOEN".to_string());
        label.set_scope(true);
        label.set_def(0);
        let _ = label_recog_two.insert_label(label.clone());

        /*
            Assembly file two:

            SEHR_SCHOEN: Subn $0, $0, $0
            jmp SEHR_SCHOEN
        */

        parsed_vector.push(assembly_code_two);

        // ####################################################################

        /*
            Resulting assembly file:

            SEHR_SCHOEN: Add $0, $0, $1 # !!
            Ld $11, 56
            Mov $1, $11
            SEHR_SCHOEN: Subn $0, $0, $0 # !!
            jmp SEHR_SCHOEN
        */

        let _result = link(parsed_vector);

        assert!(matches!(Err::<LinkError, LinkError>(LinkError::InsertRecog(CommonError::MultipleGlobalDefined(label))), _result));
    }

    #[test]
    fn test_no_defined_labels() {
        let mut parsed_vector: Vec<AssemblyCode<LabelRecog>> = vec![];

        let mut assembly_code_one = AssemblyCode::new(LabelRecog::new());

        let operation_vec_one = assembly_code_one.get_text_refmut();
        operation_vec_one.push(Operation::Instr(Instruction::Add(Reg::G0, Reg::G1, Reg::G12)));
        operation_vec_one.push(Operation::Instr(Instruction::Or(Reg::G2, Reg::G12, Reg::G12)));
        operation_vec_one.push(Operation::Instr(Instruction::Sub(Reg::G2, Reg::G12, Reg::G12)));

        /*
            Assembly file one:

            Add $0, $1, $12
            Or $2, $12, $12
            Sub $3, $15, $15
        */

        parsed_vector.push(assembly_code_one);

        let mut assembly_code_two = AssemblyCode::new(LabelRecog::new());

        let operation_vec_two = assembly_code_two.get_text_refmut();

        operation_vec_two.push(Operation::Instr(Instruction::Add(Reg::G0, Reg::G1, Reg::G12)));
        operation_vec_two.push(Operation::Instr(Instruction::Or(Reg::G2, Reg::G12, Reg::G12)));
        operation_vec_two.push(Operation::Instr(Instruction::Sub(Reg::G2, Reg::G12, Reg::G12)));

        /*
            Assembly file two:

            Add $0, $1, $12
            Or $2, $12, $12
            Sub $3, $15, $15
        */

        parsed_vector.push(assembly_code_two);

        // ####################################################################

        let label_recog_ver1 = LabelRecog::new();
        let mut label_recog_ver2 = LabelRecog::new();

        label_recog_ver2.add_offset(3, LabelType::Address);

        let mut assembly_code_ver = AssemblyCode::new(Namespaces::new());

        let namespace_ver = assembly_code_ver.get_labels_refmut();

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

        let operation_vec_ver = assembly_code_ver.get_text_refmut();

        operation_vec_ver.push(Operation::Namespace(0));
        operation_vec_ver.push(Operation::Instr(Instruction::Add(Reg::G0, Reg::G1, Reg::G12)));
        operation_vec_ver.push(Operation::Instr(Instruction::Or(Reg::G2, Reg::G12, Reg::G12)));
        operation_vec_ver.push(Operation::Instr(Instruction::Sub(Reg::G2, Reg::G12, Reg::G12)));
        operation_vec_ver.push(Operation::Namespace(1));
        operation_vec_ver.push(Operation::Instr(Instruction::Add(Reg::G0, Reg::G1, Reg::G12)));
        operation_vec_ver.push(Operation::Instr(Instruction::Or(Reg::G2, Reg::G12, Reg::G12)));
        operation_vec_ver.push(Operation::Instr(Instruction::Sub(Reg::G2, Reg::G12, Reg::G12)));

        assert_eq!(assembly_code_ver, link(parsed_vector).unwrap());
    }

    #[test]
    fn test_single_link() {
        let mut assembly_code = AssemblyCode::new(LabelRecog::new());

        let operation_vec = assembly_code.get_text_refmut();
        operation_vec.push(Operation::Instr(Instruction::Add(Reg::G0, Reg::G1, Reg::G12)));
        operation_vec.push(Operation::Instr(Instruction::Or(Reg::G2, Reg::G12, Reg::G12)));
        operation_vec.push(Operation::Instr(Instruction::Sub(Reg::G2, Reg::G12, Reg::G12)));
        operation_vec.push(Operation::Instr(Instruction::Add(Reg::G0, Reg::G1, Reg::G12)));
        operation_vec.push(Operation::Instr(Instruction::Or(Reg::G2, Reg::G12, Reg::G12)));
        operation_vec.push(Operation::Instr(Instruction::Sub(Reg::G2, Reg::G12, Reg::G12)));

        /*
            Assembly file:

            Add $0, $1, $12
            Or $2, $12, $12
            Sub $3, $15, $15
            Add $0, $1, $12
            Or $2, $12, $12
            Sub $3, $15, $15
        */

        let mut assembly_code_ver = AssemblyCode::new(Namespaces::new());
        let _ = assembly_code_ver.get_labels_refmut().insert_recog(LabelRecog::new());

        let operation_vec_ver = assembly_code_ver.get_text_refmut();

        operation_vec_ver.push(Operation::Namespace(0));
        operation_vec_ver.push(Operation::Instr(Instruction::Add(Reg::G0, Reg::G1, Reg::G12)));
        operation_vec_ver.push(Operation::Instr(Instruction::Or(Reg::G2, Reg::G12, Reg::G12)));
        operation_vec_ver.push(Operation::Instr(Instruction::Sub(Reg::G2, Reg::G12, Reg::G12)));
        operation_vec_ver.push(Operation::Instr(Instruction::Add(Reg::G0, Reg::G1, Reg::G12)));
        operation_vec_ver.push(Operation::Instr(Instruction::Or(Reg::G2, Reg::G12, Reg::G12)));
        operation_vec_ver.push(Operation::Instr(Instruction::Sub(Reg::G2, Reg::G12, Reg::G12)));

        assert_eq!(assembly_code_ver, link(Vec::from([assembly_code])).unwrap());
    }
}
