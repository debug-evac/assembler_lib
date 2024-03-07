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

pub mod errors;

pub use asm_core_lib::common::*;

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
