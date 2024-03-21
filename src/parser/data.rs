/*
 * Copyright (c) 2023 Steven Becker
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

use std::{any::Any, fmt::Display};

use log::{debug, error};
use winnow::{
    Parser,
    branch::alt,
    bytes::{tag, take_till1},
    character::{digit1, space1},
    combinator::{fail, opt, success},
    multi::separated1,
    sequence::{delimited, separated_pair},
    IResult
};

use super::{
    errors::ParserError, handle_label_defs, instructions::parse_seper, literals::{
        parse_bigimm, 
        parse_imm, 
        parse_label_definition, 
        parse_label_name, parse_text_segment_id
    }, parse_multiline_comments, symbols::Symbols, ByteData, DWordData, HalfData, LabelRecog, LabelType, MemData, WordData
};

#[derive(Clone, Debug, PartialEq)]
enum Directive {
    Data(MemData),
    EqvLabel(smartstring::alias::String, i128)
}

impl From<MemData> for Directive {
    fn from(value: MemData) -> Self {
        Directive::Data(value)
    }
}

impl Display for Directive {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Directive::Data(memdata) => write!(f, "{memdata}"),
            Directive::EqvLabel(label, imm) => write!(f, ".eqv {label}, {imm}"),
        }
    }
}

fn parse_byte(input: &str) -> IResult<&str, MemData> {
    separated1(
        alt((
            parse_imm.map(|imm| imm.into()),
            parse_label_name.map(|label| {
                let labl = smartstring::alias::String::from(label);
                match Symbols::symbols_read(&labl) {
                    Some(val) => ByteData::Byte(val as i16),
                    None => ByteData::String(labl),
                }
            })
        )),
        parse_seper
    ).map(
        |data| MemData::Bytes(data, false)
    ).parse_next(input)
}

fn parse_half(input: &str) -> IResult<&str, MemData> {
    separated1(
        alt((
            parse_imm.map(|imm| imm.into()),
            parse_label_name.map(|label| {
                let labl = smartstring::alias::String::from(label);
                match Symbols::symbols_read(&labl) {
                    Some(val) => HalfData::Half(val as i32),
                    None => HalfData::String(labl),
                }
            })
        )),
        parse_seper
    ).map(
        MemData::Halfs
    ).parse_next(input)
}

fn parse_word(input: &str) -> IResult<&str, MemData> {
    separated1(
        alt((
            parse_bigimm.map(|imm| imm.into()),
            parse_label_name.map(|label| {
                let labl = smartstring::alias::String::from(label);
                match Symbols::symbols_read(&labl) {
                    Some(val) => WordData::Word(val as i64),
                    None => WordData::String(labl),
                }
            })
        )),
        parse_seper
    ).map(
        MemData::Words
    ).parse_next(input)
}

fn parse_dword(input: &str) -> IResult<&str, MemData> {
    separated1(
        alt((
            parse_bigimm.map(|imm| imm.into()),
            parse_label_name.map(|label| {
                let labl = smartstring::alias::String::from(label);
                match Symbols::symbols_read(&labl) {
                    Some(val) => DWordData::DWord(val),
                    None => DWordData::String(labl),
                }
            })
        )),
        parse_seper
    ).map(
        MemData::DWords
    ).parse_next(input)
}

fn parse_eqv(input: &str) -> IResult<&str, Directive> {
    separated_pair(
        parse_label_name, 
        parse_seper, 
        parse_bigimm
    ).map(
        |(label, def)| Directive::EqvLabel(label.into(), def)
    ).parse_next(input)
}

fn string_to_le_words(input: String) -> MemData {
    let mut vec_data = vec![];
    let iter = input.as_bytes().chunks_exact(4);
    for word in iter.clone() {
        vec_data.push(ByteData::Byte(word[3] as i16));
        vec_data.push(ByteData::Byte(word[2] as i16));
        vec_data.push(ByteData::Byte(word[1] as i16));
        vec_data.push(ByteData::Byte(word[0] as i16));
    };
    let last_values = iter.remainder();
    match last_values.len() {
        0 => (),
        1 => vec_data.push(ByteData::Byte(last_values[0] as i16)),
        2 => {
            vec_data.push(ByteData::Byte(last_values[1] as i16));
            vec_data.push(ByteData::Byte(last_values[0] as i16));
        },
        3 => {
            vec_data.push(ByteData::Byte(last_values[2] as i16));
            vec_data.push(ByteData::Byte(last_values[1] as i16));
            vec_data.push(ByteData::Byte(last_values[0] as i16));
        },
        _ => unreachable!(),
    };
    MemData::Bytes(vec_data, true)
}

fn parse_directive(input: &str) -> IResult<&str, Directive> {
    let (rest, (_, directive)) = alt((
        separated_pair(tag(".byte"), space1, parse_byte.map(Directive::Data)),
        separated_pair(tag(".half"), space1, parse_half.map(Directive::Data)),
        separated_pair(tag(".word"), space1, parse_word.map(Directive::Data)),
        separated_pair(tag(".dword"), space1, parse_dword.map(Directive::Data)),
        separated_pair(tag(".space"), space1,
            digit1.map_res(|num| {
                let mut vec_data = vec![];
                for _ in 0..str::parse(num)? {
                    vec_data.push(ByteData::Byte(0));
                }
                Ok::<Directive, <usize as core::str::FromStr>::Err>(MemData::Bytes(vec_data, true).into())
            }
        )),
        separated_pair(tag(".ascii"), space1,
            delimited('"', take_till1("\n\";"), '"').map(
            |ascii_str: &str| string_to_le_words(ascii_str.to_string()).into()
        )),
        separated_pair(tag(".asciz"), space1,
            delimited('"', take_till1("\n\";"), '"').map(
            |ascii_str: &str| {
                let mut ascii_string = ascii_str.to_string(); 
                ascii_string.push('\0');
                string_to_le_words(ascii_string).into()
            } 
        )),
        separated_pair(tag(".string"), space1,
            delimited('"', take_till1("\n\";"), '"').map(
            |ascii_str: &str| {
                let mut ascii_string = ascii_str.to_string(); 
                ascii_string.push('\0');
                string_to_le_words(ascii_string).into()
            }
        )),
        separated_pair(tag(".eqv"), space1, parse_eqv)
    )).parse_next(input)?;

    Ok((rest, directive))
}

fn parse_line(input: &str) -> IResult<&str, Box<dyn LineHandle>> {
    let (rest, early) = parse_multiline_comments(input)?;
    if early {
        return Ok((rest, Box::from(NoData { })))
    }
    alt((
        separated_pair(
            parse_label_definition.map(Some),
            parse_multiline_comments,
            parse_directive.map(Some),
        ),
        (
            parse_label_definition.map(Some), 
            success(None)
        ),
        (
            success(None),
            parse_directive.map(Some)
        ),
    ))
    .output_into()
    .parse_next(rest)
}

fn handle_label_refs_count(direct: &MemData, symbol_map: &mut LabelRecog) -> usize {
    match direct {
        MemData::Bytes(data, contains_no_labels) => {
            if !contains_no_labels {
                for byte in data.iter() {
                    match byte {
                        ByteData::Byte(_) => (),
                        ByteData::String(label) => symbol_map.crt_or_ref_label(label),
                    }
                }
            }
            data.len()
        },
        MemData::Halfs(data) => {
            for half in data.iter() {
                match half {
                    HalfData::Half(_) => (),
                    HalfData::String(label) => symbol_map.crt_or_ref_label(label),
                }
            }
            data.len() * 2
        },
        MemData::Words(data) => {
            for word in data.iter() {
                match word {
                    WordData::Word(_) => (),
                    WordData::String(label) => symbol_map.crt_or_ref_label(label),
                }
            }
            data.len() * 4
        },
        MemData::DWords(data) => {
            for dword in data.iter() {
                match dword {
                    DWordData::DWord(_) => (),
                    DWordData::String(label) => symbol_map.crt_or_ref_label(label),
                }
            }
            data.len() * 8
        },
        _ => unreachable!(),
    }
}

fn align_data(direct: &MemData, next_free_ptr: &mut usize, dir_list: &mut [MemData]) {
    match direct {
        MemData::Bytes(_, _) => (),
        MemData::Halfs(_) => {
            if *next_free_ptr % 2 != 0 {
                if let MemData::Bytes(byte_data, _) = dir_list.last_mut().unwrap() {
                    byte_data.push(ByteData::Byte(0));
                }
                *next_free_ptr += 2;
            }
        },
        MemData::Words(_) | MemData::DWords(_) => {
            let free_bytes = *next_free_ptr % 4;
            if free_bytes != 0 {
                match dir_list.last_mut().unwrap() {
                    MemData::Bytes(byte_data, _) => {
                        for _ in 0..free_bytes + 2 {
                            byte_data.push(ByteData::Byte(0));
                        }
                        *next_free_ptr += free_bytes + 2;
                    },
                    MemData::Halfs(half_data) => {
                        half_data.push(HalfData::Half(0));
                        *next_free_ptr += 1;
                    },
                    MemData::Words(_) |
                    MemData::DWords(_) |
                    MemData::Namespace(_) => (),
                }
            }
        },
        MemData::Namespace(_) => unreachable!(),
    }
}

#[derive(Debug, PartialEq)]
struct NoData {}
#[derive(Debug, PartialEq)]
struct LabelDef {
    label: smartstring::alias::String
}
#[derive(Debug, PartialEq)]
struct DirectiveData {
    directive: Directive
}
#[derive(Debug, PartialEq)]
struct LabelDirectiveData {
    label: smartstring::alias::String, 
    directive: Directive
}

trait LineHandle {
    fn as_any(&self) -> &dyn Any;
    fn handle(&self, next_free_ptr: &mut usize, dir_list: &mut Vec<MemData>, symbol_map: &mut LabelRecog) -> Result<(), ParserError>;
}

impl From<(Option<&str>, Option<Directive>)> for Box<dyn LineHandle> {
    fn from(value: (Option<&str>, Option<Directive>)) -> Self {
        match value {
            (Some(labl), Some(direct)) => Box::from(LabelDirectiveData { label: labl.into(), directive: direct }),
            (None, Some(direct)) => Box::from(DirectiveData { directive: direct }),
            (Some(labl), None) => Box::from(LabelDef { label: labl.into() }),
            (None, None) => Box::from(NoData {}),
        }
    }
}

impl LineHandle for DirectiveData {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn handle(&self, next_free_ptr: &mut usize, dir_list: &mut Vec<MemData>, symbol_map: &mut LabelRecog) -> Result<(), ParserError> {
        let direct = self.directive.clone();
        debug!("Parsed data '{direct}'");
        match direct {
            Directive::Data(data) => {
                align_data(&data, next_free_ptr, dir_list);
                *next_free_ptr += handle_label_refs_count(&data, symbol_map);
                dir_list.push(data);
            },
            Directive::EqvLabel(label, def) => Symbols::symbols_write(label, def),
        }
        Ok(())
    }
}

impl LineHandle for LabelDef {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn handle(&self, next_free_ptr: &mut usize, _dir_list: &mut Vec<MemData>, symbol_map: &mut LabelRecog) -> Result<(), ParserError> {
        let label = &self.label;
        debug!("Parsed label '{label}'");
        handle_label_defs(label, symbol_map, LabelType::Data, *next_free_ptr)?;
        Ok(())
    }
}

impl LineHandle for LabelDirectiveData {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn handle(&self, next_free_ptr: &mut usize, dir_list: &mut Vec<MemData>, symbol_map: &mut LabelRecog) -> Result<(), ParserError> {
        let direct = self.directive.clone();
        let label = &self.label;
        debug!("Parsed label '{label}' and data '{direct}'");
        match direct {
            Directive::Data(data) => {
                align_data(&data, next_free_ptr, dir_list);
                handle_label_defs(label, symbol_map, LabelType::Data, *next_free_ptr)?;
                *next_free_ptr += handle_label_refs_count(&data, symbol_map);
                dir_list.push(data);
            },
            Directive::EqvLabel(label, def) => Symbols::symbols_write(label, def),
        }
        Ok(())
    }
}

impl LineHandle for NoData {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn handle(&self, _next_free_ptr: &mut usize, _dir_list: &mut Vec<MemData>, _symbol_map: &mut LabelRecog) -> Result<(), ParserError> {
        Err(ParserError::NoTextSection)
    }
}

pub fn parse<'a>(input: &'a str, symbol_map: &mut LabelRecog) -> IResult<&'a str, Vec<MemData>> {
    let mut dir_list: Vec<MemData> = vec![];

    let mut rest = input;
    let mut next_free_ptr = 0;

    loop {
        let (rest_line, parsed) = parse_line(rest)?;
        rest = rest_line;

        if let Err(e) = parsed.handle(&mut next_free_ptr, &mut dir_list, symbol_map) {
            error!("{e}");
            return fail.context(e.get_nom_err_text()).parse_next(rest)
        }

        let (rested, breakout) = delimited(
            parse_multiline_comments,
            opt(parse_text_segment_id),
            parse_multiline_comments
        ).parse_next(rest)?;
        if breakout.is_some() {
            debug!("Finished data parsing sub step");
            rest = rested;
            break
        }
    }

    Ok((rest, dir_list))
}

#[cfg(test)]
mod tests {
    use crate::common::LabelElem;

    use super::*;

    // TODO: Better test cases

    #[test]
    fn test_parse_byte() {
        assert_eq!(parse_byte("15, 16, 10, 15"), Ok(("", MemData::Bytes(Vec::from([
            ByteData::Byte(15), 
            ByteData::Byte(16), 
            ByteData::Byte(10), 
            ByteData::Byte(15)
        ]), false))));
        assert_eq!(parse_byte("16, ligma, 201"), Ok(("", MemData::Bytes(Vec::from([
            ByteData::Byte(16), 
            ByteData::String("ligma".into()),
            ByteData::Byte(201)
        ]), false))));
        assert_eq!(parse_byte(".awldldaw"), Ok(("", MemData::Bytes(Vec::from([
            ByteData::String(".awldldaw".into())
        ]), false))));
    }

    #[test]
    fn test_parse_half() {
        assert_eq!(parse_half("15, 16, 10, 15"), Ok(("", MemData::Halfs(Vec::from([
            HalfData::Half(15), 
            HalfData::Half(16), 
            HalfData::Half(10), 
            HalfData::Half(15)
        ])))));
        assert_eq!(parse_half("16, ligma, 201"), Ok(("", MemData::Halfs(Vec::from([
            HalfData::Half(16), 
            HalfData::String("ligma".into()),
            HalfData::Half(201)
        ])))));
        assert_eq!(parse_half(".awldldaw"), Ok(("", MemData::Halfs(Vec::from([
            HalfData::String(".awldldaw".into())
        ])))));
    }

    #[test]
    fn test_parse_word() {
        assert_eq!(parse_word("15, 16, 10, 15"), Ok(("", MemData::Words(Vec::from([
            WordData::Word(15), 
            WordData::Word(16), 
            WordData::Word(10), 
            WordData::Word(15)
        ])))));
        assert_eq!(parse_word("16, ligma, 201"), Ok(("", MemData::Words(Vec::from([
            WordData::Word(16), 
            WordData::String("ligma".into()),
            WordData::Word(201)
        ])))));
        assert_eq!(parse_word(".awldldaw"), Ok(("", MemData::Words(Vec::from([
            WordData::String(".awldldaw".into())
        ])))));
    }

    #[test]
    fn test_parse_dword() {
        assert_eq!(parse_dword("15, 16, 10, 15"), Ok(("", MemData::DWords(Vec::from([
            DWordData::DWord(15), 
            DWordData::DWord(16), 
            DWordData::DWord(10), 
            DWordData::DWord(15)
        ])))));
        assert_eq!(parse_dword("16, ligma, 201"), Ok(("", MemData::DWords(Vec::from([
            DWordData::DWord(16), 
            DWordData::String("ligma".into()),
            DWordData::DWord(201)
        ])))));
        assert_eq!(parse_dword(".awldldaw"), Ok(("", MemData::DWords(Vec::from([
            DWordData::String(".awldldaw".into())
        ])))));
    }

    #[test]
    fn test_parse_eqv() {
        assert_eq!(parse_eqv("test, 120"), Ok(("", Directive::EqvLabel("test".into(), 120))));
        assert_eq!(parse_eqv("testing,0b10"), Ok(("", Directive::EqvLabel("testing".into(), 2))));
        assert_ne!(parse_eqv("test"), Ok(("", Directive::EqvLabel("test".into(), 0))));
    }

    #[test]
    fn test_string_to_le_words() {
        let mut words_vec = Vec::from([
            ByteData::Byte(0x20), ByteData::Byte(0x65), ByteData::Byte(0x68), ByteData::Byte(0x74),
            ByteData::Byte(0x63), ByteData::Byte(0x69), ByteData::Byte(0x75), ByteData::Byte(0x71),
            ByteData::Byte(0x72), ByteData::Byte(0x62), ByteData::Byte(0x20), ByteData::Byte(0x6b),
            ByteData::Byte(0x20), ByteData::Byte(0x6e), ByteData::Byte(0x77), ByteData::Byte(0x6f),
            ByteData::Byte(0x20), ByteData::Byte(0x78), ByteData::Byte(0x6f), ByteData::Byte(0x66),
            ByteData::Byte(0x70), ByteData::Byte(0x6d), ByteData::Byte(0x75), ByteData::Byte(0x6a),
            ByteData::Byte(0x76), ByteData::Byte(0x6f), ByteData::Byte(0x20), ByteData::Byte(0x73),
            ByteData::Byte(0x74), ByteData::Byte(0x20), ByteData::Byte(0x72), ByteData::Byte(0x65),
            ByteData::Byte(0x6c), ByteData::Byte(0x20), ByteData::Byte(0x65), ByteData::Byte(0x68),
            ByteData::Byte(0x20), ByteData::Byte(0x79), ByteData::Byte(0x7a), ByteData::Byte(0x61),
            ByteData::Byte(0x67), ByteData::Byte(0x6f), ByteData::Byte(0x64),
        ]);

        // last has only 3
        assert_eq!(string_to_le_words("the quick brown fox jumps over the lazy dog".to_string()),
            MemData::Bytes(words_vec.clone(), true)
        );

        words_vec.insert(40, ByteData::Byte(0x31));

        // last has all
        assert_eq!(string_to_le_words("the quick brown fox jumps over the lazy dog1".to_string()),
            MemData::Bytes(words_vec.clone(), true)
        );

        words_vec.remove(40);
        words_vec.remove(40);

        // last has only 2
        assert_eq!(string_to_le_words("the quick brown fox jumps over the lazy do".to_string()),
            MemData::Bytes(words_vec.clone(), true)
        );

        words_vec.remove(40);

        // last has only 1
        assert_eq!(string_to_le_words("the quick brown fox jumps over the lazy d".to_string()),
            MemData::Bytes(words_vec.clone(), true)
        );

        words_vec.insert(40, ByteData::Byte(0x6f));
        words_vec.insert(40, ByteData::Byte(0x67));
        words_vec.insert(40, ByteData::Byte(0x31));
        words_vec.push(ByteData::Byte(0));

        // last has 4 and null byte
        assert_eq!(string_to_le_words("the quick brown fox jumps over the lazy dog1\0".to_string()),
            MemData::Bytes(words_vec.clone(), true)
        );
    }

    #[test]
    fn test_parse_directive() {
        assert_eq!(parse_directive(".word 2000, 1510"), Ok(("", MemData::Words(Vec::from([
            WordData::Word(2000),
            WordData::Word(1510)
        ])).into())));

        assert_eq!(parse_directive(".word       lolgetit,12002, 5195"), Ok(("", MemData::Words(Vec::from([
            WordData::String("lolgetit".into()),
            WordData::Word(12002),
            WordData::Word(5195)
        ])).into())));

        assert_ne!(parse_directive(".space 20,1"), Ok(("", MemData::Bytes(Vec::from([
            ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0),
            ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0),
            ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0),
            ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0),
        ]), true).into())));

        assert_eq!(parse_directive(".space 20"), Ok(("", MemData::Bytes(Vec::from([
            ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0),
            ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0),
            ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0),
            ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0),
        ]), true).into())));

        assert_eq!(parse_directive(".space      13"), Ok(("", MemData::Bytes(Vec::from([
            ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0),
            ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0),
            ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0)
        ]), true).into())));

        let mut words_vec = Vec::from([
            ByteData::Byte(0x20), ByteData::Byte(0x65), ByteData::Byte(0x68), ByteData::Byte(0x74),
            ByteData::Byte(0x63), ByteData::Byte(0x69), ByteData::Byte(0x75), ByteData::Byte(0x71),
            ByteData::Byte(0x72), ByteData::Byte(0x62), ByteData::Byte(0x20), ByteData::Byte(0x6b),
            ByteData::Byte(0x20), ByteData::Byte(0x6e), ByteData::Byte(0x77), ByteData::Byte(0x6f),
            ByteData::Byte(0x20), ByteData::Byte(0x78), ByteData::Byte(0x6f), ByteData::Byte(0x66),
            ByteData::Byte(0x70), ByteData::Byte(0x6d), ByteData::Byte(0x75), ByteData::Byte(0x6a),
            ByteData::Byte(0x76), ByteData::Byte(0x6f), ByteData::Byte(0x20), ByteData::Byte(0x73),
            ByteData::Byte(0x74), ByteData::Byte(0x20), ByteData::Byte(0x72), ByteData::Byte(0x65),
            ByteData::Byte(0x6c), ByteData::Byte(0x20), ByteData::Byte(0x65), ByteData::Byte(0x68),
            ByteData::Byte(0x20), ByteData::Byte(0x79), ByteData::Byte(0x7a), ByteData::Byte(0x61),
            ByteData::Byte(0x67), ByteData::Byte(0x6f), ByteData::Byte(0x64),
        ]);

        assert_eq!(parse_directive(".ascii  \"the quick brown fox jumps over the lazy dog\""),
            Ok(("", MemData::Bytes(words_vec.clone(), true).into()))
        );

        words_vec.insert(40, ByteData::Byte(0));

        assert_eq!(parse_directive(".string  \"the quick brown fox jumps over the lazy dog\""),
            Ok(("", MemData::Bytes(words_vec.clone(), true).into()))
        );

        assert_eq!(parse_directive(".asciz  \"the quick brown fox jumps over the lazy dog\""),
            Ok(("", MemData::Bytes(words_vec.clone(), true).into()))
        );

        assert_eq!(parse_directive(".eqv  test,1505"),
            Ok(("", Directive::EqvLabel("test".into(), 1505)))
        );
    }

    macro_rules! assert_equ_line {
        ($pars_line:literal, $rest:literal, $struct:ident { $( $field:ident: $val:expr ),*}) => {
            let (rest, parsed) = parse_line($pars_line).unwrap();
            assert_eq!(rest, $rest);
            assert_eq!(parsed.as_any().downcast_ref::<$struct>().unwrap(), &$struct { $( $field: $val ),* });
        };
    }

    #[test]
    fn test_parse_line() -> Result<(), Box<dyn std::error::Error>> {
        assert_equ_line!("label:   .word 30,51", "", LabelDirectiveData { label: "label".into(), directive: MemData::Words(Vec::from([
            WordData::Word(30),
            WordData::Word(51)
        ])).into() });
        assert_equ_line!("\ntest:\n\n.string   \"HANS!\"", "", LabelDirectiveData { label: "test".into(), directive: MemData::Bytes(Vec::from([
            ByteData::Byte('S' as i16), ByteData::Byte('N' as i16), ByteData::Byte('A' as i16), ByteData::Byte('H' as i16),
            ByteData::Byte(0), ByteData::Byte('!' as i16)
        ]), true).into() });
        assert_equ_line!("\n\n\n.space     12\n", "\n", DirectiveData { directive: MemData::Bytes(Vec::from([
            ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0),
            ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0),
            ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0),
        ]), true).into() });
        assert_equ_line!("\n\ntest:   \n\n.text", "   \n\n.text", LabelDef { label: "test".into() });
        assert_equ_line!("label:\n.half    105, testing, 120", "", LabelDirectiveData { label: "label".into(), directive: MemData::Halfs(Vec::from([
            HalfData::Half(105),
            HalfData::String("testing".into()),
            HalfData::Half(120)
        ])).into() });
        assert_equ_line!("label:\n.ascii \"SToP\"", "", LabelDirectiveData { label: "label".into(), directive: MemData::Bytes(Vec::from([
            ByteData::Byte('P' as i16), ByteData::Byte('o' as i16), ByteData::Byte('T' as i16), ByteData::Byte('S' as i16)
        ]), true).into() });
        Ok(())
    }

    #[test]
    fn test_parse() {
        let data_code = r#"
HelloKitty:
    .asciz      "MIAO!"                 ; Very nice kitten - 6
    .byte       10, 10, HelloKitty      ; 12
VeryGood:                               ; 13
    .word       1250, 1250, 1250        ; 25
NeedSomeSpaceGotIt:                     ; 26
    .space      20                      ; Space for super secret data struct!!!

.text
    "#;

        let mut symbol_map = LabelRecog::new();

        let mut symbols = LabelRecog::new();
        let mut label = LabelElem::new_refd("HelloKitty".into());
        label.set_type(LabelType::Data);
        label.set_scope(true);
        label.set_refd();
        label.set_def(0);
        let _ = symbols.insert_label(label);

        label = LabelElem::new();
        label.set_name("VeryGood".into());
        label.set_type(LabelType::Data);
        label.set_scope(true);
        label.set_def(12);
        let _ = symbols.insert_label(label);

        label = LabelElem::new();
        label.set_name("NeedSomeSpaceGotIt".into());
        label.set_type(LabelType::Data);
        label.set_scope(true);
        label.set_def(24);
        let _ = symbols.insert_label(label);

        let correct_vec: Vec<MemData> = vec![
                                                MemData::Bytes(Vec::from([
                                                    ByteData::Byte('O' as i16), ByteData::Byte('A' as i16), ByteData::Byte('I' as i16), ByteData::Byte('M' as i16), 
                                                    ByteData::Byte(0), ByteData::Byte('!' as i16)
                                                ]), true),
                                                MemData::Bytes(Vec::from([
                                                    ByteData::Byte(10), ByteData::Byte(10), ByteData::String("HelloKitty".into()), ByteData::Byte(0),
                                                    ByteData::Byte(0), ByteData::Byte(0)
                                                ]), false),
                                                MemData::Words(Vec::from([
                                                    WordData::Word(1250), WordData::Word(1250), WordData::Word(1250)
                                                ])),
                                                MemData::Bytes(Vec::from([
                                                    ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0),
                                                    ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0),
                                                    ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0),
                                                    ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0),
                                                ]), true)
                                            ];

        assert_eq!(parse(data_code, &mut symbol_map),
                   Ok(("", correct_vec)));
        assert_eq!(symbol_map, symbols);
    }
}
