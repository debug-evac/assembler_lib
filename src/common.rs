/*
 * Copyright (c) 2023 Steven Becker
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

//
// This module defines types, functions, ... that are commonly used in
// other modules. This module was created since the parser module is 
// quite big.

mod directives;
mod instructions;
mod labels;
mod macros;
mod registers;
pub mod errors;

use std::fmt::Display;

//pub use asm_core_lib::common::*;
pub use self::directives::*;
pub use self::labels::*;
pub use self::instructions::*;
pub use self::macros::*;
pub use self::registers::*;

use crate::linker::Namespaces;
use crate::parser::Subroutines;

pub trait AssemblyCode {
    type Labels;

    fn new(labels: Self::Labels) -> Self;
    fn get_labels_refmut(&mut self) -> &mut Self::Labels;
    fn get_data_refmut(&mut self) -> &mut Vec<MemData>;
    fn get_text_refmut(&mut self) -> &mut Vec<Operation>;
    fn get_all_refmut(&mut self) -> (&mut Self::Labels, &mut Vec<Operation>, &mut Vec<MemData>);
}

macro_rules! impl_assembly_code {
    ($([$( $der:ident ),+])? $struct:ident { labels:$ltype:ty, $( $field:ident:$type:ty ),*}) => {
        $(#[derive($( $der ),+)])?
        pub struct $struct {
            labels: $ltype,
            data: Vec<MemData>,
            text: Vec<Operation>,
            $(
                $field: $type,
            )*
        }

        impl AssemblyCode for $struct {
            type Labels = $ltype;
        
            fn new(labels: Self::Labels) -> Self {
                $struct { labels, data: vec![], text: vec![], $( $field: <$type>::new() )* }
            }
        
            fn get_labels_refmut(&mut self) -> &mut Self::Labels {
                &mut self.labels
            }
        
            fn get_data_refmut(&mut self) -> &mut Vec<MemData> {
                &mut self.data
            }
        
            fn get_text_refmut(&mut self) -> &mut Vec<Operation> {
                &mut self.text
            }
        
            fn get_all_refmut(&mut self) -> (&mut Self::Labels, &mut Vec<Operation>, &mut Vec<MemData>) {
                (&mut self.labels, &mut self.text, &mut self.data)
            }
        }
    };
}

impl_assembly_code!(
    AssemblyCodeRecog { 
        labels: LabelRecog, 
        subroutine: Subroutines
    }
);

impl AssemblyCodeRecog {
    pub fn set_text(&mut self, other: Vec<Operation>) {
        self.text = other
    }

    pub fn set_data(&mut self, other: Vec<MemData>) {
        self.data = other
    }

    pub fn get_subroutine_refmut(&mut self) -> &mut Subroutines {
        &mut self.subroutine
    }
}

impl_assembly_code!(
    [Debug, PartialEq]
    AssemblyCodeNamespaces { 
        labels: Namespaces,
    }
);

pub type Imm = i32; // always less than 32

#[derive(Debug, Clone, PartialEq)]
pub enum Operation {
    Namespace(usize),
    Instr(Instruction),
    Macro(MacroInstr),
    LablMacro(smartstring::alias::String, MacroInstr),
    LablInstr(smartstring::alias::String, Instruction),
    Labl(smartstring::alias::String)
}

impl Display for Operation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Operation::Namespace(space) => write!(f, "Namespace({space})"),
            Operation::Instr(instr) => write!(f, "{instr}"),
            Operation::Macro(macro_in) => write!(f, "{macro_in}"),
            Operation::LablMacro(label, macro_in) =>  write!(f, "{label}: {macro_in}"),
            Operation::LablInstr(label, instr) => write!(f, "{label}: {instr}"),
            Operation::Labl(label) => write!(f, "{label}:")
        }
    }
}

impl From<Instruction> for Operation {
    fn from(item: Instruction) -> Self {
        Operation::Instr(item)
    }
}

impl From<MacroInstr> for Operation {
    fn from(item: MacroInstr) -> Self {
        Operation::Macro(item)
    }
}

#[derive(Debug, PartialEq)]
pub struct TranslatableCode {
    data: Vec<MemData>,
    text: Vec<Instruction>
}

impl TranslatableCode {
    #[allow(dead_code)]
    pub fn new() -> Self {
        TranslatableCode { data: vec![], text: vec![] }
    }

    pub fn new_with_data(data: Vec<MemData>) -> Self {
        TranslatableCode { data, text: vec![] }
    }

    #[allow(dead_code)]
    pub fn get_data_refmut(&mut self) -> &mut Vec<MemData> {
        &mut self.data
    }

    pub fn get_text_refmut(&mut self) -> &mut Vec<Instruction> {
        &mut self.text
    }

    pub fn get_all_ref(&self) -> (&Vec<Instruction>, &Vec<MemData>) {
        (&self.text, &self.data)
    }
}

impl Default for TranslatableCode {
    fn default() -> Self {
        Self::new()
    }
}