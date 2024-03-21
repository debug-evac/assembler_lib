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
use winnow::stream::Offset;
use winnow::trace::trace;
use winnow::{
    ascii::{multispace1, not_line_ending},
    combinator::{fail, opt},
    error::{ErrMode, ErrorKind, ParseError},
    stream::{Accumulate, Stream, StreamIsPartial},
    IResult,
    Parser
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

fn escaped_internal<'a, I: 'a, Error, F, G, O1, O2>(
    input: I,
    normal: &mut F,
    control_char: char,
    escapable: &mut G,
) -> IResult<I, <I as Stream>::Slice, Error>
where
    I: Stream + Offset,
    <I as Stream>::Token: winnow::stream::AsChar,
    F: Parser<I, O1, Error>,
    G: Parser<I, O2, Error>,
    Error: ParseError<I>,
{
    use winnow::stream::AsChar;

    let mut i = input.clone();

    while i.eof_offset() > 0 {
        let current_len = i.eof_offset();

        match normal.parse_next(i.clone()) {
            Ok((i2, _)) => {
                // return if we consumed everything or if the normal parser
                // does not consume anything
                if i2.eof_offset() == 0 {
                    return Ok(input.next_slice(input.eof_offset()));
                } else if i2.eof_offset() == current_len {
                    let offset = input.offset_to(&i2);
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
    Error: ParseError<I>,
{
    trace("escaped", move |input: I| {
        if input.is_partial() {
            return fail(input)
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
    E: ParseError<I>,
{
    trace("many_m_n", move |mut input: I| {
        if min > max {
            return Err(ErrMode::Cut(E::from_error_kind(input, ErrorKind::Many)));
        }

        let mut res = C::initial(Some(min));
        for count in 0..max {
            let len = input.eof_offset();
            match parse.parse_next(input.clone()) {
                Ok((tail, value)) => {
                    // infinite loop check: the parser must always consume
                    if tail.eof_offset() == len {
                        return Err(ErrMode::from_error_kind(input, ErrorKind::Many));
                    }

                    res.accumulate(value);
                    input = tail;
                }
                Err(ErrMode::Backtrack(e)) => {
                    if count < min {
                        return Err(ErrMode::Backtrack(e.append(input, ErrorKind::Many)));
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
}

fn parse_multiline_comments(input: &str) -> IResult<&str, bool> {
    // many_m_n since winnow detects a chance for infinite loops (bogus)
    let (rest, parsed): (&str, Option<Vec<&str>>) = opt(
        many_m_n(
            0,
            1000000000,
            escaped(multispace1, ';', not_line_ending)
        )
    ).parse_next(input)?;
    if parsed.is_none() {
        // is only None at EOF
        let (rest, _) = winnow::bytes::take::<usize, &str, winnow::error::Error<&str>>(rest.len()).parse_next(rest)?;
        return Ok((rest, true))
    }
    Ok((rest, false))
}

pub fn parse<'a>(input: &'a str, subroutines: &mut Option<&mut Subroutines>, sp_init: bool) -> IResult<&'a str, AssemblyCodeRecog> {
    let mut assembly: AssemblyCodeRecog = AssemblyCode::new(LabelRecog::new());
    
    Symbols::symbols_clear();

    let (mut rest, parsed) = (parse_multiline_comments, opt(parse_data_segment_id), parse_multiline_comments).parse_next(input)?;
    if parsed.1.is_some() {
        warn!("Experimental: Data sections have not been tested rigorously! Expect bugs and errors!");
        let parsed = data::parse(rest, assembly.get_labels_refmut())?;
        rest = parsed.0;
        assembly.set_data(parsed.1);
    } else {
        let (rested, _) = (parse_multiline_comments, opt(parse_text_segment_id), parse_multiline_comments).parse_next(rest)?;
        rest = rested;
    }

    let (rest, vec_ops) = text::parse(rest, subroutines, assembly.get_labels_refmut(), sp_init)?;
    assembly.set_text(vec_ops);

    debug!("Finished parser step");

    Ok((rest, assembly))
}
