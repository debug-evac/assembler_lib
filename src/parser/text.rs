/*
 * Copyright (c) 2023 Steven Becker
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

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
        MemData::Bytes
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
        |data| MemData::Words(data, false)
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
        vec_data.push(WordData::Word(((word[3] as i64) << 24) + ((word[2] as i64) << 16) + ((word[1] as i64) << 8) + word[0] as i64));
    };
    let last_values = iter.remainder();
    match last_values.len() {
        0 => (),
        1 => vec_data.push(WordData::Word(last_values[0] as i64)),
        2 => vec_data.push(WordData::Word(((last_values[1] as i64) << 8) + last_values[0] as i64)),
        3 => vec_data.push(WordData::Word(((last_values[2] as i64) << 16) + ((last_values[1] as i64) << 8) + last_values[0] as i64)),
        _ => unreachable!(),
    };
    MemData::Words(vec_data, true)
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
                    vec_data.push(WordData::Word(0));
                }
                MemData::Words(vec_data, true) 
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
        MemData::Bytes(data) => {
            for byte in data.iter() {
                match byte {
                    ByteData::Byte(_) => (),
                    ByteData::String(label) => symbol_map.crt_or_ref_label(label),
                }
            }
            data.len() / 4
        },
        MemData::Halfs(data) => {
            for half in data.iter() {
                match half {
                    HalfData::Half(_) => (),
                    HalfData::String(label) => symbol_map.crt_or_ref_label(label),
                }
            }
            data.len() / 2
        },
        MemData::Words(data, contains_no_labels) => {
            if !contains_no_labels {
                for word in data.iter() {
                    match word {
                        WordData::Word(_) => (),
                        WordData::String(label) => symbol_map.crt_or_ref_label(label),
                    }
                }
            }
            data.len()
        },
        MemData::DWords(data) => {
            for dword in data.iter() {
                match dword {
                    DWordData::DWord(_) => (),
                    DWordData::String(label) => symbol_map.crt_or_ref_label(label),
                }
            }
            data.len() * 2
        },
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
                next_free_ptr += handle_label_refs_count(&direct, symbol_map);
                dir_list.push(direct);
            },
            (Some(label), None) => {
                handle_label_defs(label, symbol_map, LabelType::Data, next_free_ptr);
            },
            (Some(label), Some(direct)) => {
                handle_label_defs(label, symbol_map, LabelType::Data, next_free_ptr);
                next_free_ptr += handle_label_refs_count(&direct, symbol_map);
                dir_list.push(direct);
            },
            (None, None) => {
                todo!("Implement error! This can only occur when .text is not specified!");
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
    use super::*;

    // TODO: Better test cases

    #[test]
    fn test_parse_byte() {
        assert_eq!(parse_byte("15, 16, 10, 15"), Ok(("", MemData::Bytes(Vec::from([
            ByteData::Byte(15), 
            ByteData::Byte(16), 
            ByteData::Byte(10), 
            ByteData::Byte(15)
        ])))));
        assert_eq!(parse_byte("16, ligma, 201"), Ok(("", MemData::Bytes(Vec::from([
            ByteData::Byte(16), 
            ByteData::String("ligma".to_string()), 
            ByteData::Byte(201)
        ])))));
        assert_ne!(parse_byte(".awldldaw"), Ok(("", MemData::Bytes(Vec::from([
            ByteData::String(".awldldaw".to_string())
        ])))));
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
        ]), false))));
        assert_eq!(parse_word("16, ligma, 201"), Ok(("", MemData::Words(Vec::from([
            WordData::Word(16), 
            WordData::String("ligma".to_string()), 
            WordData::Word(201)
        ]), false))));
        assert_ne!(parse_word(".awldldaw"), Ok(("", MemData::Words(Vec::from([
            WordData::String(".awldldaw".to_string())
        ]), false))));
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
            WordData::Word(0x20656874),
            WordData::Word(0x63697571),
            WordData::Word(0x7262206b),
            WordData::Word(0x206e776f),
            WordData::Word(0x20786f66),
            WordData::Word(0x706d756a),
            WordData::Word(0x766f2073),
            WordData::Word(0x74207265),
            WordData::Word(0x6c206568),
            WordData::Word(0x20797a61),
            WordData::Word(0x00676f64)
        ]);

        // last has only 3
        assert_eq!(string_to_le_words("the quick brown fox jumps over the lazy dog".to_string()),
            MemData::Words(words_vec.clone(), true)
        );

        words_vec[10] = WordData::Word(0x31676f64);

        // last has all
        assert_eq!(string_to_le_words("the quick brown fox jumps over the lazy dog1".to_string()),
            MemData::Words(words_vec.clone(), true)
        );

        words_vec[10] = WordData::Word(0x00006f64);

        // last has only 2
        assert_eq!(string_to_le_words("the quick brown fox jumps over the lazy do".to_string()),
            MemData::Words(words_vec.clone(), true)
        );

        words_vec[10] = WordData::Word(0x00000064);

        // last has only 1
        assert_eq!(string_to_le_words("the quick brown fox jumps over the lazy d".to_string()),
            MemData::Words(words_vec.clone(), true)
        );

        words_vec[10] = WordData::Word(0x31676f64);
        words_vec.push(WordData::Word(0x0));

        // last has 4 and null byte
        assert_eq!(string_to_le_words("the quick brown fox jumps over the lazy dog1\0".to_string()),
            MemData::Words(words_vec.clone(), true)
        );
    }

    #[test]
    fn test_parse_directive() {
        assert_eq!(parse_directive(".word 2000, 1510"), Ok(("", MemData::Words(Vec::from([
            WordData::Word(2000),
            WordData::Word(1510)
        ]), false))));

        assert_eq!(parse_directive(".word       lolgetit,12002, 5195"), Ok(("", MemData::Words(Vec::from([
            WordData::String("lolgetit".to_string()),
            WordData::Word(12002),
            WordData::Word(5195)
        ]), false))));

        assert_ne!(parse_directive(".space 20,1"), Ok(("", MemData::Words(Vec::from([
            WordData::Word(0), WordData::Word(0), WordData::Word(0), WordData::Word(0), WordData::Word(0),
            WordData::Word(0), WordData::Word(0), WordData::Word(0), WordData::Word(0), WordData::Word(0),
            WordData::Word(0), WordData::Word(0), WordData::Word(0), WordData::Word(0), WordData::Word(0),
            WordData::Word(0), WordData::Word(0), WordData::Word(0), WordData::Word(0), WordData::Word(0),
        ]), true))));

        assert_eq!(parse_directive(".space 20"), Ok(("", MemData::Words(Vec::from([
            WordData::Word(0), WordData::Word(0), WordData::Word(0), WordData::Word(0), WordData::Word(0),
            WordData::Word(0), WordData::Word(0), WordData::Word(0), WordData::Word(0), WordData::Word(0),
            WordData::Word(0), WordData::Word(0), WordData::Word(0), WordData::Word(0), WordData::Word(0),
            WordData::Word(0), WordData::Word(0), WordData::Word(0), WordData::Word(0), WordData::Word(0),
        ]), true))));

        assert_eq!(parse_directive(".space      13"), Ok(("", MemData::Words(Vec::from([
            WordData::Word(0), WordData::Word(0), WordData::Word(0), WordData::Word(0), WordData::Word(0),
            WordData::Word(0), WordData::Word(0), WordData::Word(0), WordData::Word(0), WordData::Word(0),
            WordData::Word(0), WordData::Word(0), WordData::Word(0)
        ]), true))));

        let words_vec = Vec::from([
            WordData::Word(0x20656874),
            WordData::Word(0x63697571),
            WordData::Word(0x7262206b),
            WordData::Word(0x206e776f),
            WordData::Word(0x20786f66),
            WordData::Word(0x706d756a),
            WordData::Word(0x766f2073),
            WordData::Word(0x74207265),
            WordData::Word(0x6c206568),
            WordData::Word(0x20797a61),
            WordData::Word(0x00676f64)
        ]);

        assert_eq!(parse_directive(".ascii  \"the quick brown fox jumps over the lazy dog\""),
            Ok(("", MemData::Words(words_vec.clone(), true)))
        );

        assert_eq!(parse_directive(".string  \"the quick brown fox jumps over the lazy dog\""),
            Ok(("", MemData::Words(words_vec.clone(), true)))
        );

        assert_eq!(parse_directive(".asciz  \"the quick brown fox jumps over the lazy dog\""),
            Ok(("", MemData::Words(words_vec.clone(), true)))
        );
    }

    #[ignore = "not implemented yet"]
    #[test]
    fn test_parse_line() {

    }

    #[ignore = "not implemented yet"]
    #[test]
    fn test_parse() {

    }
}
