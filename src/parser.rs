/*
 * Copyright (c) 2023 Steven Becker
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

mod instructions;
mod literals;
mod code;
mod text;

use nom::{
    bytes::complete::escaped, 
    character::complete::{multispace1, not_line_ending}, 
    combinator::opt, 
    multi::many0, 
    sequence::tuple, 
    IResult
};
use std::collections::HashSet;

use crate::common::*;

use self::literals::{parse_data_segment_id, parse_text_segment_id};

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
}

fn handle_label_defs(label: &str, symbol_map: &mut LabelRecog, instr_counter: usize) {
    let (label_string, scope) = match label.strip_prefix('.') {
        Some(label) => {
            // Local label; Track definitions and references!
            (label.to_string(), false)
        },
        None => {
            // Global label; Do not track definitions and references!
            (label.to_string(), true)
        },
    };
    symbol_map.crt_or_def_label(&label_string, scope, instr_counter.try_into().unwrap());
}

fn parse_multiline_comments(input: &str) -> IResult<&str, bool> {
    let (rest, parsed) = opt(
        many0(
            escaped(multispace1, ';', not_line_ending)
        )
    )(input)?;
    if parsed.is_none() {
        // is only None at EOF
        let (rest, _) = nom::bytes::complete::take::<usize, &str, nom::error::Error<&str>>(rest.len())(rest)?;
        return Ok((rest, true))
    }
    Ok((rest, false))
}

pub fn parse<'a>(input: &'a str, subroutines: &mut Option<&mut Subroutines>) -> IResult<&'a str, (LabelRecog, Vec<Operation<'a>>)> {
    let mut symbol_map = LabelRecog::new();

    let (mut rest, parsed) = tuple((parse_multiline_comments, opt(parse_data_segment_id), parse_multiline_comments))(input)?;
    if parsed.1.is_some() {
        let parsed = text::parse(rest, &mut symbol_map)?;
        rest = parsed.0;
        // TODO: Handle Vec<MemData>
    } else {
        let (rested, _) = tuple((parse_multiline_comments, opt(parse_text_segment_id), parse_multiline_comments))(rest)?;
        rest = rested;
    }

    let (rest, vec_ops) = code::parse(rest, subroutines, &mut symbol_map)?;

    Ok((rest, (symbol_map, vec_ops)))
}

// Konstanten:
// .data
// [LABEL] .[ASSEMBLER INSTRUCTION] [IMM]
// .text
// CODE
