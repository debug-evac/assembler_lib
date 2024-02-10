/*
 * Copyright (c) 2023 Steven Becker
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

use log::error;
use nom::{
    branch::alt, 
    bytes::complete::{is_not, tag}, 
    character::complete::{digit1, space1}, 
    combinator::{map, opt, success},
    multi::separated_list1, 
    sequence::{delimited, pair, separated_pair}, 
    IResult
};

use super::{
    handle_label_defs, instructions::parse_seper, literals::{
        parse_bigimm, 
        parse_imm, 
        parse_label_definition, 
        parse_label_name, parse_text_segment_id
    }, parse_multiline_comments, ByteData, DWordData, HalfData, LabelRecog, LabelType, MemData, WordData
};

fn parse_byte(input: &str) -> IResult<&str, MemData> {
    map(
        separated_list1(
            parse_seper,
            alt((
                map(parse_imm, |imm| imm.into()),
                map(parse_label_name, |label| ByteData::String(label.to_string()))
            ))
        ),
        |data| MemData::Bytes(data, false)
    )(input)
}

fn parse_half(input: &str) -> IResult<&str, MemData> {
    map(
        separated_list1(
            parse_seper,
            alt((
                map(parse_imm, |imm| imm.into()),
                map(parse_label_name, |label| HalfData::String(label.to_string()))
            ))
        ),
        MemData::Halfs
    )(input)
}

fn parse_word(input: &str) -> IResult<&str, MemData> {
    map(
        separated_list1(
            parse_seper,
            alt((
                map(parse_bigimm, |imm| imm.into()),
                map(parse_label_name, |label| WordData::String(label.to_string()))
            ))
        ),
        MemData::Words
    )(input)
}

fn parse_dword(input: &str) -> IResult<&str, MemData> {
    map(
        separated_list1(
            parse_seper,
            alt((
                map(parse_bigimm, |imm| imm.into()),
                map(parse_label_name, |label| DWordData::String(label.to_string()))
            ))
        ),
        MemData::DWords
    )(input)
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

fn parse_directive(input: &str) -> IResult<&str, MemData> {
    let (rest, (_, directive)) = alt((
        separated_pair(tag(".byte"), space1, parse_byte),
        separated_pair(tag(".half"), space1, parse_half),
        separated_pair(tag(".word"), space1, parse_word),
        separated_pair(tag(".dword"), space1, parse_dword),
        separated_pair(tag(".space"), space1, map(
            digit1, |num| {
                let mut vec_data = vec![];
                for _ in 0..str::parse(num).unwrap() {
                    vec_data.push(ByteData::Byte(0));
                }
                MemData::Bytes(vec_data, true) 
            }
        )),
        separated_pair(tag(".ascii"), space1, map(
            delimited(nom::character::complete::char('"'), is_not("\n\";"), nom::character::complete::char('"')),
            |ascii_str: &str| string_to_le_words(ascii_str.to_string())
        )),
        separated_pair(tag(".asciz"), space1, map(
            delimited(nom::character::complete::char('"'), is_not("\n\";"), nom::character::complete::char('"')),
            |ascii_str: &str| {
                let mut ascii_string = ascii_str.to_string(); 
                ascii_string.push('\0');
                string_to_le_words(ascii_string)
            } 
        )),
        separated_pair(tag(".string"), space1, map(
            delimited(nom::character::complete::char('"'), is_not("\n\";"), nom::character::complete::char('"')),
            |ascii_str: &str| {
                let mut ascii_string = ascii_str.to_string(); 
                ascii_string.push('\0');
                string_to_le_words(ascii_string)
            }
        )),
    ))(input)?;

    Ok((rest, directive))
}

#[allow(clippy::type_complexity)]
fn parse_line(input: &str) -> IResult<&str, (Option<&str>, Option<MemData>)> {
    let (rest, early) = parse_multiline_comments(input)?;
    if early {
        return Ok((rest, (None, None)))
    }
    alt((
        separated_pair(
            map(parse_label_definition, Some),
            parse_multiline_comments,
            map(
                parse_directive,
                Some
            )
        ),
        pair(
            map(parse_label_definition, Some), 
            success(None)
        ),
        pair(
            success(None),
            map(
                parse_directive,
                Some
            )
        ),
    ))(rest)
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
            data.len() + 1
        },
        MemData::Halfs(data) => {
            for half in data.iter() {
                match half {
                    HalfData::Half(_) => (),
                    HalfData::String(label) => symbol_map.crt_or_ref_label(label),
                }
            }
            data.len() * 2 + 1
        },
        MemData::Words(data) => {
            for word in data.iter() {
                match word {
                    WordData::Word(_) => (),
                    WordData::String(label) => symbol_map.crt_or_ref_label(label),
                }
            }
            data.len() * 4 + 1
        },
        MemData::DWords(data) => {
            for dword in data.iter() {
                match dword {
                    DWordData::DWord(_) => (),
                    DWordData::String(label) => symbol_map.crt_or_ref_label(label),
                }
            }
            data.len() * 8 + 1
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
                *next_free_ptr += 1;
            }
        },
        MemData::Words(_) | MemData::DWords(_) => {
            let free_bytes = *next_free_ptr % 4;
            if free_bytes != 0 {
                match dir_list.last_mut().unwrap() {
                    MemData::Bytes(byte_data, _) => {
                        for _ in 0..free_bytes {
                            byte_data.push(ByteData::Byte(0));
                        }
                        *next_free_ptr += free_bytes;
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

pub fn parse<'a>(input: &'a str, symbol_map: &mut LabelRecog) -> IResult<&'a str, Vec<MemData>> {
    let mut dir_list: Vec<MemData> = vec![];

    let mut rest = input;
    let mut next_free_ptr = 0;

    loop {
        let parsed = match parse_line(rest) {
            Ok(line) => {
                rest = line.0;
                line.1
            },
            Err(e) => return Err(e),
        };

        match parsed {
            (None, Some(direct)) => {
                align_data(&direct, &mut next_free_ptr, &mut dir_list);
                next_free_ptr += handle_label_refs_count(&direct, symbol_map);
                dir_list.push(direct);
            },
            (Some(label), None) => {
                let free_ptr = if next_free_ptr > 0 {
                    next_free_ptr - 1
                } else {
                    0
                };
                if let Err(e) = handle_label_defs(label, symbol_map, LabelType::Data, free_ptr) {
                    error!("{e}");
                    std::process::exit(1)
                };
            },
            (Some(label), Some(direct)) => {
                align_data(&direct, &mut next_free_ptr, &mut dir_list);
                let free_ptr = if next_free_ptr > 0 {
                    next_free_ptr - 1
                } else {
                    0
                };
                if let Err(e) = handle_label_defs(label, symbol_map, LabelType::Data, free_ptr) {
                    error!("{e}");
                    std::process::exit(1)
                };
                next_free_ptr += handle_label_refs_count(&direct, symbol_map);
                dir_list.push(direct);
            },
            (None, None) => {
                error!("Specified .data section without .text section!");
                std::process::exit(1);
            },
        }

        let (rested, breakout) = delimited(parse_multiline_comments, opt(parse_text_segment_id), parse_multiline_comments)(rest)?;
        if breakout.is_some() {
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
            ByteData::String("ligma".to_string()), 
            ByteData::Byte(201)
        ]), false))));
        assert_ne!(parse_byte(".awldldaw"), Ok(("", MemData::Bytes(Vec::from([
            ByteData::String(".awldldaw".to_string())
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
            HalfData::String("ligma".to_string()), 
            HalfData::Half(201)
        ])))));
        assert_ne!(parse_half(".awldldaw"), Ok(("", MemData::Halfs(Vec::from([
            HalfData::String(".awldldaw".to_string())
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
            WordData::String("ligma".to_string()), 
            WordData::Word(201)
        ])))));
        assert_ne!(parse_word(".awldldaw"), Ok(("", MemData::Words(Vec::from([
            WordData::String(".awldldaw".to_string())
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
            DWordData::String("ligma".to_string()), 
            DWordData::DWord(201)
        ])))));
        assert_ne!(parse_dword(".awldldaw"), Ok(("", MemData::DWords(Vec::from([
            DWordData::String(".awldldaw".to_string())
        ])))));
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
        ])))));

        assert_eq!(parse_directive(".word       lolgetit,12002, 5195"), Ok(("", MemData::Words(Vec::from([
            WordData::String("lolgetit".to_string()),
            WordData::Word(12002),
            WordData::Word(5195)
        ])))));

        assert_ne!(parse_directive(".space 20,1"), Ok(("", MemData::Bytes(Vec::from([
            ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0),
            ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0),
            ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0),
            ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0),
        ]), true))));

        assert_eq!(parse_directive(".space 20"), Ok(("", MemData::Bytes(Vec::from([
            ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0),
            ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0),
            ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0),
            ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0),
        ]), true))));

        assert_eq!(parse_directive(".space      13"), Ok(("", MemData::Bytes(Vec::from([
            ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0),
            ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0),
            ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0)
        ]), true))));

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
            Ok(("", MemData::Bytes(words_vec.clone(), true)))
        );

        words_vec.insert(40, ByteData::Byte(0));

        assert_eq!(parse_directive(".string  \"the quick brown fox jumps over the lazy dog\""),
            Ok(("", MemData::Bytes(words_vec.clone(), true)))
        );

        assert_eq!(parse_directive(".asciz  \"the quick brown fox jumps over the lazy dog\""),
            Ok(("", MemData::Bytes(words_vec.clone(), true)))
        );
    }

    #[test]
    fn test_parse_line() {
        assert_eq!(parse_line("label:   .word 30,51"),
                   Ok(("", (Some("label"), Some(MemData::Words(Vec::from([
                        WordData::Word(30),
                        WordData::Word(51)
                    ])))))));
        assert_eq!(parse_line("\ntest:\n\n.string   \"HANS!\""),
                   Ok(("", (Some("test"), Some(MemData::Bytes(Vec::from([
                        ByteData::Byte('S' as i16), ByteData::Byte('N' as i16), ByteData::Byte('A' as i16), ByteData::Byte('H' as i16),
                        ByteData::Byte(0), ByteData::Byte('!' as i16)
                   ]), true))))));
        assert_eq!(parse_line("\n\n\n.space     12\n"),
                   Ok(("\n", (None, Some(MemData::Bytes(Vec::from([
                        ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0),
                        ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0),
                        ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0), ByteData::Byte(0),
                   ]), true))))));
        assert_eq!(parse_line("\n\ntest:   \n\n.text"),
                   Ok(("   \n\n.text", (Some("test"), None))));
        assert_eq!(parse_line("label:\n.half    105, testing, 120"),
                   Ok(("", (Some("label"), Some(MemData::Halfs(Vec::from([
                        HalfData::Half(105),
                        HalfData::String("testing".to_string()),
                        HalfData::Half(120)
                   ])))))));
        assert_eq!(parse_line("label:\n.ascii \"SToP\""),
                   Ok(("", (Some("label"), Some(MemData::Bytes(Vec::from([
                        ByteData::Byte('P' as i16), ByteData::Byte('o' as i16), ByteData::Byte('T' as i16), ByteData::Byte('S' as i16)
                   ]), true))))));
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
        let mut label = LabelElem::new_refd("HelloKitty".to_string());
        label.set_type(LabelType::Data);
        label.set_scope(true);
        label.set_refd();
        label.set_def(0);
        let _ = symbols.insert_label(label);

        label = LabelElem::new();
        label.set_name("VeryGood".to_string());
        label.set_type(LabelType::Data);
        label.set_scope(true);
        label.set_def(13);
        let _ = symbols.insert_label(label);

        label = LabelElem::new();
        label.set_name("NeedSomeSpaceGotIt".to_string());
        label.set_type(LabelType::Data);
        label.set_scope(true);
        label.set_def(26);
        let _ = symbols.insert_label(label);

        let correct_vec: Vec<MemData> = vec![
                                                MemData::Bytes(Vec::from([
                                                    ByteData::Byte('O' as i16), ByteData::Byte('A' as i16), ByteData::Byte('I' as i16), ByteData::Byte('M' as i16), 
                                                    ByteData::Byte(0), ByteData::Byte('!' as i16)
                                                ]), true),
                                                MemData::Bytes(Vec::from([
                                                    ByteData::Byte(10), ByteData::Byte(10), ByteData::String("HelloKitty".to_string()), ByteData::Byte(0),
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
