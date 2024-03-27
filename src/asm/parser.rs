/*
 * Copyright (c) 2023 Steven Becker
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

mod instructions;
mod literals;
mod text;
mod data;
mod symbols;

use log::debug;
use winnow::{
    combinator::{opt, terminated}, token::{literal, take_until}, PResult, Parser
};
use std::{collections::HashSet, str::FromStr};

use crate::{common::*, asm::parser::symbols::Symbols};

use self::errors::{CommonError, LibraryError};

pub(crate) use self::text::parse as text_parse;

pub struct Subroutines {
    code_str_vec: HashSet<String>
}

impl Subroutines {
    pub fn new() -> Self {
        let code_str_vec = HashSet::new();

        Subroutines{ code_str_vec }
    }

    pub fn get_code(&self) -> Vec<String> {
        self.code_str_vec.iter().cloned().collect()
    }

    pub fn merge(&mut self, other: &Subroutines) {
        for elem in other.code_str_vec.iter() {
            self.code_str_vec.insert(elem.to_owned());
        }
    }
}

impl Default for Subroutines {
    fn default() -> Self {
        Self::new()
    }
}

#[inline]
fn handle_label_defs(label: &str, symbol_map: &mut LabelRecog, ltype: LabelType, instr_counter: usize) -> Result<(), CommonError> {
    symbol_map.crt_or_def_label(&label.into(), !label.starts_with('.'), ltype, instr_counter.try_into()?)
}

fn parse(input: &mut &str) -> PResult<AssemblyCodeRecog> {
    let mut assembly: AssemblyCodeRecog = AssemblyCode::new(LabelRecog::new());
    
    Symbols::symbols_clear();

    let parsed = opt(terminated(take_until(0.., ".data"), literal(".data"))).parse_next(input)?;
    if parsed.is_some() {
        let parsed = data::parse(input, assembly.get_labels_refmut())?;
        assembly.set_data(parsed);
    } else {
        let _ = opt(terminated(take_until(0.., ".text"), literal(".text"))).parse_next(input)?;
    }

    {
        let (labels, subroutine) = assembly.get_parse_refmut();
        let vec_ops = text::parse(input, &mut Some(subroutine), labels)?;
        assembly.set_text(vec_ops);
    }

    debug!("Finished parser step");

    Ok(assembly)
}

impl FromStr for AssemblyCodeRecog {
    type Err = LibraryError;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        parse
            .parse(input)
            .map_err(|e| (0, e).into())
    }
}