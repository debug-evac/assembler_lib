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
    ascii::{digit1, escaped, multispace1, space0, space1, till_line_ending}, combinator::{alt, delimited, empty, fail, not, opt, preceded, separated, separated_pair, terminated}, dispatch, error::StrContext, stream::AsChar, token::{none_of, take_till}, PResult, Parser
};

use super::{
    errors::ParserError, handle_label_defs, instructions::parse_seper, literals::{
        parse_bigimm, 
        parse_imm, 
        parse_label_definition, 
        parse_label_name, parse_text_segment_id
    }, symbols::Symbols, ByteData, DWordData, HalfData, LabelRecog, LabelType, MemData, WordData
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

fn parse_byte(input: &mut &str) -> PResult<MemData> {
    separated(
        1..,
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

fn parse_half(input: &mut &str) -> PResult<MemData> {
    separated(
        1..,
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

fn parse_word(input: &mut &str) -> PResult<MemData> {
    separated(
        1..,
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

fn parse_dword(input: &mut &str) -> PResult<MemData> {
    separated(
        1..,
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

fn parse_eqv(input: &mut &str) -> PResult<Directive> {
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

fn parse_directive(input: &mut &str) -> PResult<Directive> {
    dispatch!{terminated(take_till(1.., AsChar::is_space), space1);
        ".byte" => parse_byte.map(Directive::Data),
        ".half" => parse_half.map(Directive::Data),
        ".word" => parse_word.map(Directive::Data),
        ".dword" => parse_dword.map(Directive::Data),
        ".space" => digit1.try_map(|num| {
            let mut vec_data = vec![];
            for _ in 0..str::parse(num)? {
                vec_data.push(ByteData::Byte(0));
            }
            Ok::<Directive, <usize as core::str::FromStr>::Err>(MemData::Bytes(vec_data, true).into())
        }),
        ".ascii" => delimited('"', take_till(1.., ['\n', '\"', ';']), '"').map(
            |ascii_str: &str| string_to_le_words(ascii_str.to_string()).into()
        ),
        ".asciz" => delimited('"', take_till(1.., ['\n', '\"', ';']), '"').map(
            |ascii_str: &str| {
                let mut ascii_string = ascii_str.to_string(); 
                ascii_string.push('\0');
                string_to_le_words(ascii_string).into()
        }),
        ".string" => delimited('"', take_till(1.., ['\n', '\"', ';']), '"').map(
            |ascii_str: &str| {
                let mut ascii_string = ascii_str.to_string(); 
                ascii_string.push('\0');
                string_to_le_words(ascii_string).into()
        }),
        ".eqv" => parse_eqv,
        _ => fail.context(StrContext::Label("unknown directive")),
    }.parse_next(input)
}

fn real_parse_line(input: &mut &str) -> PResult<Box<dyn LineHandle>> {
    alt((
        separated_pair(
            parse_label_definition.map(Some),
            space1,
            parse_directive.map(Some)
        ),
        (
            parse_label_definition.map(Some), 
            empty.value(None)
        ),
        (
            empty.value(None),
            parse_directive.map(Some)
        ),
        (
            empty.value(None),
            not(none_of(('\n', ';', '\r'))).value(None)
        )
    ))
    .output_into()
    .parse_next(input)
}

fn parse_line(input: &mut &str) -> PResult<Box<dyn LineHandle>> {
    escaped(none_of(('\n', ';', '\r')), ';', till_line_ending)
    .and_then(delimited(space0, real_parse_line, space0))
    .parse_next(input)
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

fn align_data(direct: &MemData, next_free_ptr: &mut usize, dir_list: &mut [MemData]) -> usize {
    match direct {
        MemData::Bytes(_, _) => 0,
        MemData::Halfs(_) => {
            if *next_free_ptr % 2 != 0 {
                if let MemData::Bytes(byte_data, _) = dir_list.last_mut().unwrap() {
                    byte_data.push(ByteData::Byte(0));
                }
                *next_free_ptr += 1;
                1
            } else {
                0
            }
        },
        MemData::Words(_) | MemData::DWords(_) => {
            let free_bytes = *next_free_ptr % 4;
            if free_bytes != 0 {
                match dir_list.last_mut().unwrap() {
                    MemData::Bytes(byte_data, _) => {
                        let real_bytes = 4 - free_bytes;
                        for _ in 0..real_bytes {
                            byte_data.push(ByteData::Byte(0));
                        }
                        let num = real_bytes;
                        *next_free_ptr += num;
                        num
                    },
                    MemData::Halfs(half_data) => {
                        half_data.push(HalfData::Half(0));
                        let num = 2;
                        *next_free_ptr += num;
                        num
                    },
                    MemData::Words(_) |
                    MemData::DWords(_) |
                    MemData::Namespace(_) => 0,
                }
            } else {
                0
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
    fn handle(&self,
        next_free_ptr: &mut usize,
        dir_list: &mut Vec<MemData>,
        symbol_map: &mut LabelRecog,
        unaligned_labels: &mut Vec<smartstring::alias::String>
    ) -> Result<(), ParserError>;
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

    fn handle(&self, next_free_ptr: &mut usize, dir_list: &mut Vec<MemData>, symbol_map: &mut LabelRecog, unaligned_labels: &mut Vec<smartstring::alias::String>) -> Result<(), ParserError> {
        let direct = self.directive.clone();
        debug!("Parsed data '{direct}'");
        match direct {
            Directive::Data(data) => {
                let dif = align_data(&data, next_free_ptr, dir_list);
                while let Some(labl) = unaligned_labels.pop() {
                    if let Some(label) = symbol_map.get_label(&labl) {
                        label.add_def(dif as i128);
                    }
                }
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

    fn handle(&self, next_free_ptr: &mut usize, _dir_list: &mut Vec<MemData>, symbol_map: &mut LabelRecog, unaligned_labels: &mut Vec<smartstring::alias::String>) -> Result<(), ParserError> {
        let label = &self.label;
        debug!("Parsed label '{label}'");
        handle_label_defs(label, symbol_map, LabelType::Data, *next_free_ptr)?;
        unaligned_labels.push(label.clone());
        Ok(())
    }
}

impl LineHandle for LabelDirectiveData {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn handle(&self, next_free_ptr: &mut usize, dir_list: &mut Vec<MemData>, symbol_map: &mut LabelRecog, unaligned_labels: &mut Vec<smartstring::alias::String>) -> Result<(), ParserError> {
        let direct = self.directive.clone();
        let label = &self.label;
        debug!("Parsed label '{label}' and data '{direct}'");
        match direct {
            Directive::Data(data) => {
                let dif = align_data(&data, next_free_ptr, dir_list);
                while let Some(labl) = unaligned_labels.pop() {
                    if let Some(label) = symbol_map.get_label(&labl) {
                        label.add_def(dif as i128);
                    }
                }
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

    fn handle(&self, _next_free_ptr: &mut usize, _dir_list: &mut Vec<MemData>, _symbol_map: &mut LabelRecog, _unaligned_labels: &mut Vec<smartstring::alias::String>) -> Result<(), ParserError> {
        Err(ParserError::NoTextSection)
    }
}

pub fn parse(input: &mut &str, symbol_map: &mut LabelRecog) -> PResult<Vec<MemData>> {
    let mut dir_list: Vec<MemData> = vec![];

    let mut unaligned_labels: Vec<smartstring::alias::String> = Vec::new();

    let mut next_free_ptr = 0;

    loop {
        let parsed = preceded('\n', parse_line).parse_next(input)?;

        if let Err(e) = parsed.handle(&mut next_free_ptr, &mut dir_list, symbol_map, &mut unaligned_labels) {
            error!("{e}");
            return fail.context(StrContext::Label(e.get_nom_err_text())).parse_next(input)
        }

        let breakout = opt(delimited(multispace1, parse_text_segment_id, multispace1)).parse_next(input)?;
        if breakout.is_some() {
            debug!("Finished data parsing sub step");
            break
        }
    }

    Ok(dir_list)
}

#[cfg(test)]
mod tests {
    use crate::common::LabelElem;

    use super::*;

    // TODO: Better test cases

    #[test]
    fn test_parse_byte() {
        assert_eq!(parse_byte(&mut "15, 16, 10, 15"), Ok(MemData::Bytes(Vec::from([
            ByteData::Byte(15), 
            ByteData::Byte(16), 
            ByteData::Byte(10), 
            ByteData::Byte(15)
        ]), false)));
        assert_eq!(parse_byte(&mut "16, ligma, 201"), Ok(MemData::Bytes(Vec::from([
            ByteData::Byte(16), 
            ByteData::String("ligma".into()),
            ByteData::Byte(201)
        ]), false)));
        assert_eq!(parse_byte(&mut ".awldldaw"), Ok(MemData::Bytes(Vec::from([
            ByteData::String(".awldldaw".into())
        ]), false)));
    }

    #[test]
    fn test_parse_half() {
        assert_eq!(parse_half(&mut "15, 16, 10, 15"), Ok(MemData::Halfs(Vec::from([
            HalfData::Half(15), 
            HalfData::Half(16), 
            HalfData::Half(10), 
            HalfData::Half(15)
        ]))));
        assert_eq!(parse_half(&mut "16, ligma, 201"), Ok(MemData::Halfs(Vec::from([
            HalfData::Half(16), 
            HalfData::String("ligma".into()),
            HalfData::Half(201)
        ]))));
        assert_eq!(parse_half(&mut ".awldldaw"), Ok(MemData::Halfs(Vec::from([
            HalfData::String(".awldldaw".into())
        ]))));
    }

    #[test]
    fn test_parse_word() {
        assert_eq!(parse_word(&mut "15, 16, 10, 15"), Ok(MemData::Words(Vec::from([
            WordData::Word(15), 
            WordData::Word(16), 
            WordData::Word(10), 
            WordData::Word(15)
        ]))));
        assert_eq!(parse_word(&mut "16, ligma, 201"), Ok(MemData::Words(Vec::from([
            WordData::Word(16), 
            WordData::String("ligma".into()),
            WordData::Word(201)
        ]))));
        assert_eq!(parse_word(&mut ".awldldaw"), Ok(MemData::Words(Vec::from([
            WordData::String(".awldldaw".into())
        ]))));
    }

    #[test]
    fn test_parse_dword() {
        assert_eq!(parse_dword(&mut "15, 16, 10, 15"), Ok(MemData::DWords(Vec::from([
            DWordData::DWord(15), 
            DWordData::DWord(16), 
            DWordData::DWord(10), 
            DWordData::DWord(15)
        ]))));
        assert_eq!(parse_dword(&mut "16, ligma, 201"), Ok( MemData::DWords(Vec::from([
            DWordData::DWord(16), 
            DWordData::String("ligma".into()),
            DWordData::DWord(201)
        ]))));
        assert_eq!(parse_dword(&mut ".awldldaw"), Ok(MemData::DWords(Vec::from([
            DWordData::String(".awldldaw".into())
        ]))));
    }

    #[test]
    fn test_parse_eqv() {
        assert_eq!(parse_eqv(&mut "test, 120"), Ok(Directive::EqvLabel("test".into(), 120)));
        assert_eq!(parse_eqv(&mut "testing,0b10"), Ok(Directive::EqvLabel("testing".into(), 2)));
        assert_ne!(parse_eqv(&mut "test"), Ok(Directive::EqvLabel("test".into(), 0)));
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
        assert_eq!(parse_directive(&mut ".word 2000, 1510"), Ok(MemData::Words(Vec::from([
            WordData::Word(2000),
            WordData::Word(1510)
        ])).into()));

        assert_eq!(parse_directive(&mut ".word       lolgetit,12002, 5195"), Ok(MemData::Words(Vec::from([
            WordData::String("lolgetit".into()),
            WordData::Word(12002),
            WordData::Word(5195)
        ])).into()));

        /*assert_ne!(parse_directive(&mut ".space 20,1"), Ok(MemData::Bytes(Vec::from([
            ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0),
            ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0),
            ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0),
            ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0),
        ]), true).into()));*/

        assert_eq!(parse_directive(&mut ".space 20"), Ok(MemData::Bytes(Vec::from([
            ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0),
            ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0),
            ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0),
            ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0),
        ]), true).into()));

        assert_eq!(parse_directive(&mut ".space      13"), Ok(MemData::Bytes(Vec::from([
            ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0),
            ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0),
            ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0)
        ]), true).into()));

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

        assert_eq!(parse_directive(&mut ".ascii  \"the quick brown fox jumps over the lazy dog\""),
            Ok(MemData::Bytes(words_vec.clone(), true).into())
        );

        words_vec.insert(40, ByteData::Byte(0));

        assert_eq!(parse_directive(&mut ".string  \"the quick brown fox jumps over the lazy dog\""),
            Ok(MemData::Bytes(words_vec.clone(), true).into())
        );

        assert_eq!(parse_directive(&mut ".asciz  \"the quick brown fox jumps over the lazy dog\""),
            Ok(MemData::Bytes(words_vec.clone(), true).into())
        );

        assert_eq!(parse_directive(&mut ".eqv  test,1505"),
            Ok(Directive::EqvLabel("test".into(), 1505))
        );
    }

    macro_rules! assert_equ_line {
        ($pars_line:literal, $rest:literal, $struct:ident { $( $field:ident: $val:expr ),*}) => {
            let parsed = parse_line(&mut $pars_line).unwrap();
            //assert_eq!(&mut &$pars_line, &mut &$rest);
            assert_eq!(parsed.as_any().downcast_ref::<$struct>().unwrap(), &$struct { $( $field: $val ),* });
        };
    }

    #[test]
    fn test_parse_line() -> Result<(), Box<dyn std::error::Error>> {
        assert_equ_line!("label:   .word 30,51", "", LabelDirectiveData { label: "label".into(), directive: MemData::Words(Vec::from([
            WordData::Word(30),
            WordData::Word(51)
        ])).into() });
        assert_equ_line!("test:     .string   \"HANS!\"", "", LabelDirectiveData { label: "test".into(), directive: MemData::Bytes(Vec::from([
            ByteData::Byte('S' as i16), ByteData::Byte('N' as i16), ByteData::Byte('A' as i16), ByteData::Byte('H' as i16),
            ByteData::Byte(0), ByteData::Byte('!' as i16)
        ]), true).into() });
        assert_equ_line!(".space     12\n", "\n", DirectiveData { directive: MemData::Bytes(Vec::from([
            ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0),
            ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0),
            ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0),
        ]), true).into() });
        assert_equ_line!("   test:   \n\n.text", "   \n\n.text", LabelDef { label: "test".into() });
        assert_equ_line!(" label:   .half    105, testing, 120", "", LabelDirectiveData { label: "label".into(), directive: MemData::Halfs(Vec::from([
            HalfData::Half(105),
            HalfData::String("testing".into()),
            HalfData::Half(120)
        ])).into() });
        assert_equ_line!("label: .ascii \"SToP\"", "", LabelDirectiveData { label: "label".into(), directive: MemData::Bytes(Vec::from([
            ByteData::Byte('P' as i16), ByteData::Byte('o' as i16), ByteData::Byte('T' as i16), ByteData::Byte('S' as i16)
        ]), true).into() });
        Ok(())
    }

    #[test]
    fn test_parse_o() {
        let mut data_code = r#"
HelloKitty:
    .asciz      "MIAO!"                 ; Very nice kitten - 6
    .byte       10, 10, HelloKitty      ; 12
VeryGood:                               ; 13
    .word       1250, 1250, 1250        ; 23
NeedSomeSpaceGotIt:                     ; 24
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

        assert_eq!(parse(&mut data_code, &mut symbol_map),
                   Ok(correct_vec));
        assert_eq!(symbol_map, symbols);
    }

    #[test]
    fn test_align_data() -> Result<(), Box<dyn std::error::Error>> {
        let mut dir_list = Vec::new();
        let next_free_ptr = 1;
        dir_list.push(MemData::Bytes(Vec::from([ByteData::Byte(1)]), false));

        {
            let mut dir_list = dir_list.clone();
            let mut next_free_ptr = next_free_ptr;
            let direct = MemData::Halfs(Vec::from([HalfData::Half(1)]));
            assert_eq!(align_data(&direct, &mut next_free_ptr, &mut dir_list), 1);
            assert_eq!(dir_list[0], MemData::Bytes(Vec::from([ByteData::Byte(1),ByteData::Byte(0)]), false));
            assert_eq!(next_free_ptr, 2);
        }
        {
            let mut dir_list = dir_list.clone();
            let mut next_free_ptr = next_free_ptr;
            let direct = MemData::Words(Vec::from([WordData::Word(1)]));
            assert_eq!(align_data(&direct, &mut next_free_ptr, &mut dir_list), 3);
            assert_eq!(dir_list[0], MemData::Bytes(Vec::from([ByteData::Byte(1),ByteData::Byte(0),ByteData::Byte(0),ByteData::Byte(0)]), false));
            assert_eq!(next_free_ptr, 4);
        }
        {
            let mut dir_list = dir_list.clone();
            let mut next_free_ptr = next_free_ptr;
            let direct = MemData::DWords(Vec::from([DWordData::DWord(1)]));
            assert_eq!(align_data(&direct, &mut next_free_ptr, &mut dir_list), 3);
            assert_eq!(dir_list[0], MemData::Bytes(Vec::from([ByteData::Byte(1),ByteData::Byte(0),ByteData::Byte(0),ByteData::Byte(0)]), false));
            assert_eq!(next_free_ptr, 4);
        }

        let next_free_ptr = 2;
        dir_list.clear();
        dir_list.push(MemData::Bytes(Vec::from([ByteData::Byte(1),ByteData::Byte(2)]), false));

        {
            let mut dir_list = dir_list.clone();
            let mut next_free_ptr = next_free_ptr;
            let direct = MemData::Words(Vec::from([WordData::Word(1)]));
            assert_eq!(align_data(&direct, &mut next_free_ptr, &mut dir_list), 2);
            assert_eq!(dir_list[0], MemData::Bytes(Vec::from([ByteData::Byte(1),ByteData::Byte(2),ByteData::Byte(0),ByteData::Byte(0)]), false));
            assert_eq!(next_free_ptr, 4);
        }
        {
            let mut dir_list = dir_list.clone();
            let mut next_free_ptr = next_free_ptr;
            let direct = MemData::DWords(Vec::from([DWordData::DWord(1)]));
            assert_eq!(align_data(&direct, &mut next_free_ptr, &mut dir_list), 2);
            assert_eq!(dir_list[0], MemData::Bytes(Vec::from([ByteData::Byte(1),ByteData::Byte(2),ByteData::Byte(0),ByteData::Byte(0)]), false));
            assert_eq!(next_free_ptr, 4);
        }

        let next_free_ptr = 3;
        dir_list.clear();
        dir_list.push(MemData::Bytes(Vec::from([ByteData::Byte(1),ByteData::Byte(2),ByteData::Byte(3)]), false));

        {
            let mut dir_list = dir_list.clone();
            let mut next_free_ptr = next_free_ptr;
            let direct = MemData::Words(Vec::from([WordData::Word(1)]));
            assert_eq!(align_data(&direct, &mut next_free_ptr, &mut dir_list), 1);
            assert_eq!(dir_list[0], MemData::Bytes(Vec::from([ByteData::Byte(1),ByteData::Byte(2),ByteData::Byte(3),ByteData::Byte(0)]), false));
            assert_eq!(next_free_ptr, 4);
        }
        {
            let mut dir_list = dir_list.clone();
            let mut next_free_ptr = next_free_ptr;
            let direct = MemData::DWords(Vec::from([DWordData::DWord(1)]));
            assert_eq!(align_data(&direct, &mut next_free_ptr, &mut dir_list), 1);
            assert_eq!(dir_list[0], MemData::Bytes(Vec::from([ByteData::Byte(1),ByteData::Byte(2),ByteData::Byte(3),ByteData::Byte(0)]), false));
            assert_eq!(next_free_ptr, 4);
        }

        let next_free_ptr = 2;
        dir_list.clear();
        dir_list.push(MemData::Halfs(Vec::from([HalfData::Half(1)])));

        {
            let mut dir_list = dir_list.clone();
            let mut next_free_ptr = next_free_ptr;
            let direct = MemData::Words(Vec::from([WordData::Word(1)]));
            assert_eq!(align_data(&direct, &mut next_free_ptr, &mut dir_list), 2);
            assert_eq!(dir_list[0], MemData::Halfs(Vec::from([HalfData::Half(1),HalfData::Half(0)])));
            assert_eq!(next_free_ptr, 4);
        }
        {
            let mut dir_list = dir_list.clone();
            let mut next_free_ptr = next_free_ptr;
            let direct = MemData::DWords(Vec::from([DWordData::DWord(1)]));
            assert_eq!(align_data(&direct, &mut next_free_ptr, &mut dir_list), 2);
            assert_eq!(dir_list[0], MemData::Halfs(Vec::from([HalfData::Half(1),HalfData::Half(0)])));
            assert_eq!(next_free_ptr, 4);
        }

        Ok(())
    }
}
