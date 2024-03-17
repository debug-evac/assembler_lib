/*
 * Copyright (c) 2023 Steven Becker
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

//
// This module defines the data structures for labels.

use std::collections::HashMap;
use std::fmt::Display;

use super::errors::CommonError;

#[derive(Debug, Clone, PartialEq)]
pub enum LabelType {
    Data,
    Address,
    Uninit
}

#[derive(Debug, Clone, PartialEq)]
// Scope for locality: true for global; false for local
pub struct LabelElem {
    name: smartstring::alias::String,
    definition: i128,
    ltype: LabelType,
    scope: bool,
    referenced: bool
}

impl LabelElem {
    pub fn new() -> LabelElem {
        let name = smartstring::alias::String::new();
        let definition: i128 = 0;
        let ltype = LabelType::Uninit;
        let scope = false;
        let referenced = false;

        LabelElem {
            name,
            definition,
            ltype,
            scope, 
            referenced 
        }
    }

    #[allow(dead_code)]
    pub fn new_def(name: smartstring::alias::String, definition: i128) -> LabelElem {
        let ltype = LabelType::Uninit;
        let referenced = false;

        LabelElem {
            scope: !name.starts_with('.'),
            name,
            definition,
            ltype,
            referenced 
        }
    }

    pub fn new_refd(name: smartstring::alias::String) -> LabelElem {
        let definition: i128 = 0;
        let ltype = LabelType::Uninit;

        LabelElem {
            scope: !name.starts_with('.'), 
            name,
            definition,
            ltype,
            referenced: true
        }
    }

    pub fn combine(&mut self, other: &LabelElem) -> Result<&str, CommonError> {
        if self.name.ne(&other.name) || self.scope != other.scope {
            return Err(CommonError::LabelsNameNotEqual(self.clone(), other.clone()));
        }

        if self.ltype != LabelType::Uninit && other.ltype != LabelType::Uninit {
            return Err(CommonError::MultipleGlobalDefined(self.clone()));
        } else if self.ltype == LabelType::Uninit && other.ltype != LabelType::Uninit {
            self.definition = other.definition;
            self.ltype = other.ltype.clone();
        }

        if self.referenced || other.referenced {
            self.referenced = true;
        }

        Ok("Labels combined!")
    }

    pub fn set_name(&mut self, name: smartstring::alias::String) {
        self.name = name;
    }

    pub fn get_name(&self) -> &smartstring::alias::String {
        &self.name
    }

    pub fn set_scope(&mut self, scope: bool) {
        self.scope = scope;
    }

    pub fn get_scope(&self) -> bool {
        self.scope
    }

    pub fn set_def(&mut self, definition: i128) {
        self.definition = definition;
    }

    pub fn add_def(&mut self, offset: i128) {
        self.definition += offset;
    }

    pub fn get_def(&self) -> &i128 {
        &self.definition
    }

    pub fn set_refd(&mut self) {
        self.referenced = true;
    }

    pub fn set_type(&mut self, ltype: LabelType) {
        self.ltype = ltype;
    }

    #[allow(dead_code)]
    pub fn get_type(&mut self) -> &LabelType {
        &self.ltype
    }
}

impl Default for LabelElem {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct LabelRecog {
    label_map: HashMap<smartstring::alias::String, usize>,
    label_list: Vec<LabelElem>,
}

impl LabelRecog {
    pub fn new() -> LabelRecog {
        let label_list: Vec<LabelElem> = vec![];
        let label_map: HashMap<smartstring::alias::String, usize> = HashMap::new();

        LabelRecog {
            label_list,
            label_map,
        }
    }

    pub fn insert_label(&mut self, label: LabelElem) -> Result<&str, &str> {
        if self.label_map.contains_key(label.get_name()) {
            Err("Label already exists!")
        } else {
            let elem = self.label_list.len();
            self.label_map.insert(label.get_name().clone(), elem);
            self.label_list.push(label);
            Ok("Added label!")
        }
    }

    pub fn get_label(&mut self, label_str: &smartstring::alias::String) -> Option<&mut LabelElem> {
        match self.label_map.get(label_str) {
            Some(val) => self.label_list.get_mut(*val),
            None => None,
        }
    }

    pub fn crt_or_def_label(&mut self, label_str: &smartstring::alias::String, scope: bool, ltype: LabelType, definition: i128) -> Result<(), CommonError> {
        match self.get_label(label_str) {
            Some(label) => {
                if *label.get_type() != LabelType::Uninit {
                    return Err(CommonError::LabelAlreadyDefined(label.clone()))
                }
                label.set_def(definition);
                label.set_type(ltype);
                label.set_scope(scope);
            },
            None => {
                let mut label = LabelElem::new();
                label.set_name(label_str.clone());
                label.set_def(definition);
                label.set_type(ltype);
                label.set_scope(scope);
                let _ = self.insert_label(label);
            },
        }
        Ok(())
    }

    // Creates a label, if it does not exist already with the name label_str, scope and the reference.
    // Returns true, if there is already a definition, else false.
    pub fn crt_or_ref_label(&mut self, label_str: &smartstring::alias::String) {
        match self.get_label(label_str) {
            Some(label) => label.set_refd(),
            None => {
                let mut label = LabelElem::new_refd(label_str.clone());
                label.set_scope(!label_str.starts_with('.'));
                let _ = self.insert_label(label);
            },
        }
    }

    pub fn crt_def_ref(&mut self, label_str: &smartstring::alias::String, scope: bool, ltype: LabelType, definition: i128) {
        if self.get_label(label_str).is_none() {
            let mut label = LabelElem::new();
            label.set_name(label_str.clone());
            label.set_def(definition);
            label.set_type(ltype);
            label.set_refd();
            label.set_scope(scope);
            let _ = self.insert_label(label);
        }
    }

    pub fn set_refd_label(&mut self, label_str: &smartstring::alias::String) {
        if let Some(label) = self.get_label(label_str) {
            label.set_refd();
        }
    }

    #[allow(dead_code)]
    pub fn get_local_labels(&self) -> Vec<&LabelElem> {
        let mut local_labels: Vec<&LabelElem> = vec![];
        for label in self.label_list.iter() {
            if !label.get_scope() {
                local_labels.push(label);
            }
        }
        local_labels
    }

    pub fn get_global_labels(&self) -> Vec<&LabelElem> {
        let mut global_labels: Vec<&LabelElem> = vec![];
        for label in self.label_list.iter() {
            if label.get_scope() {
                global_labels.push(label);
            }
        }
        global_labels
    }

    pub fn add_offset(&mut self, offset: i128, ltype: LabelType) {
        if ltype == LabelType::Uninit {
            return;
        }
        for lblelm in self.label_list.iter_mut().filter(|e| e.ltype == ltype) {
            lblelm.add_def(offset);
        }
    }
}

impl Display for LabelRecog {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "LabelRecog {{ label_list: {:?} }}", self.label_list)
    }
}

impl Default for LabelRecog {
    fn default() -> Self {
        Self::new()
    }
}