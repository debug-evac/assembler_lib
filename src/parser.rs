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

use log::{debug, warn};
use winnow::ascii::space1;
use winnow::combinator::{alt, eof, repeat, terminated};
use winnow::{
    ascii::{till_line_ending, line_ending, escaped},
    combinator::opt,
    Parser,
    PResult
};
use std::collections::HashSet;

use crate::{common::*, parser::symbols::Symbols};

use self::{errors::CommonError, literals::{parse_data_segment_id, parse_text_segment_id}};

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

impl Default for Subroutines {
    fn default() -> Self {
        Self::new()
    }
}

#[inline]
fn handle_label_defs(label: &str, symbol_map: &mut LabelRecog, ltype: LabelType, instr_counter: usize) -> Result<(), CommonError> {
    symbol_map.crt_or_def_label(&label.into(), !label.starts_with('.'), ltype, instr_counter.try_into()?)
}

/*fn escaped_internal<'a, I: 'a, Error, F, G, O1, O2>(
    input: I,
    normal: &mut F,
    control_char: char,
    escapable: &mut G,
) //-> IResult<I, <I as Stream>::Slice, Error>
-> PResult<<I as Stream>::Slice, Error>
where
    I: Stream + Offset,
    <I as Stream>::Token: winnow::stream::AsChar,
    F: Parser<I, O1, Error>,
    G: Parser<I, O2, Error>,
    Error: ParserError<I>,
{
    use winnow::stream::AsChar;

    //let mut i = input.clone();

    while input.eof_offset() > 0 {
        let current_len = input.eof_offset();

        match normal.parse_next(&mut input) {
            Ok(_) => {
                // return if we consumed everything or if the normal parser
                // does not consume anything
                if input.eof_offset() == 0 {
                    return Ok(input.next_slice(input.eof_offset()));
                } else if input.eof_offset() == current_len {
                    let offset = input.offset_to(&input);
                    return Ok(input.next_slice(offset));
                } else {
                    i = i2;
                }
            }
            Err(ErrMode::Backtrack(_)) => {
                if i.next_token().expect("eof_offset > 0").1.as_char() == control_char {
                    let next = control_char.len_utf8();
                    if next >= i.eof_offset() {
                        return Err(ErrMode::from_error_kind(input, ErrorKind::Fail));
                    } else {
                        match escapable.parse_next(i.next_slice(next).0) {
                            Ok((i2, _)) => {
                                if i2.eof_offset() == 0 {
                                    return Ok(input.next_slice(input.eof_offset()));
                                } else {
                                    i = i2;
                                }
                            }
                            Err(e) => return Err(e),
                        }
                    }
                } else {
                    let offset = input.offset_to(&i);
                    if offset == 0 {
                        return Err(ErrMode::from_error_kind(input, ErrorKind::Fail));
                    }
                    return Ok(input.next_slice(offset));
                }
            }
            Err(e) => {
                return Err(e);
            }
        }
    }

    Ok(input.next_slice(input.eof_offset()))
}

fn escaped<'a, I: 'a, Error, F, G, O1, O2>(
    mut normal: F,
    control_char: char,
    mut escapable: G,
) -> impl Parser<I, <I as Stream>::Slice, Error>
where
    I: StreamIsPartial,
    I: Stream + Offset,
    <I as Stream>::Token: winnow::stream::AsChar,
    F: Parser<I, O1, Error>,
    G: Parser<I, O2, Error>,
    Error: ParserError<I>,
{
    trace("escaped", move |input: I| {
        if input.is_partial() {
            return fail(&mut input)
        } else {
            escaped_internal(
                input,
                &mut normal,
                control_char,
                &mut escapable,
            )
        }
    })
}

fn many_m_n<I, O, C, E, F>(
    min: usize,
    max: usize,
    mut parse: F,
) -> impl Parser<I, C, E>
where
    I: Stream,
    C: Accumulate<O>,
    F: Parser<I, O, E>,
    E: ParserError<I>,
{
    trace("many_m_n", move |mut input: I| {
        if min > max {
            return Err(ErrMode::Cut(E::from_error_kind(&input, ErrorKind::Many)));
        }

        let mut res = C::initial(Some(min));
        for count in 0..max {
            let len = input.eof_offset();
            match parse.parse_next(&mut input) {
                Ok(value) => {
                    // infinite loop check: the parser must always consume
                    if input.eof_offset() == len {
                        return Err(ErrMode::from_error_kind(&input, ErrorKind::Many));
                    }

                    res.accumulate(value);
                }
                Err(ErrMode::Backtrack(e)) => {
                    if count < min {
                        return Err(ErrMode::Backtrack(e.append(&input, ErrorKind::Many)));
                    } else {
                        return Ok((input, res));
                    }
                }
                Err(e) => {
                    return Err(e);
                }
            }
        }

        Ok((input, res))
    })
}*/

fn parse_multiline_comments(input: &mut &str) -> PResult<bool> {
    //let parsed: Option<Vec<&str>> = opt(
    let _parsed: Vec<&str> =
        /*many_m_n(
            0,
            1000000000,*/
            //escaped(multispace1, ';', not_line_ending)
        repeat(
            0..,
            terminated(
                escaped(space1, ';', till_line_ending),
                alt((line_ending.void(), eof.void()))
            )
            //escaped(multispace1, ';', until_newline)
        ).parse_next(input)?;
    //).parse_next(input)?;
    /*if parsed.is_none() {
        // is only None at EOF
        let _ = winnow::token::take(input.len()).parse_next(input)?;
        return Ok(true)
    }*/
    Ok(false)
}

pub fn parse(input: &mut &str, subroutines: &mut Option<&mut Subroutines>, sp_init: bool) -> PResult<AssemblyCodeRecog> {
    let mut assembly: AssemblyCodeRecog = AssemblyCode::new(LabelRecog::new());
    
    Symbols::symbols_clear();

    let parsed = (parse_multiline_comments, opt(parse_data_segment_id), parse_multiline_comments).parse_next(input)?;
    if parsed.1.is_some() {
        warn!("Experimental: Data sections have not been tested rigorously! Expect bugs and errors!");
        let parsed = data::parse(input, assembly.get_labels_refmut())?;
        assembly.set_data(parsed);
    } else {
        let _ = (parse_multiline_comments, opt(parse_text_segment_id), parse_multiline_comments).parse_next(input)?;
    }

    let vec_ops = text::parse(input, subroutines, assembly.get_labels_refmut(), sp_init)?;
    assembly.set_text(vec_ops);

    debug!("Finished parser step");

    Ok(assembly)
}
