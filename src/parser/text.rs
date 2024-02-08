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
    combinator::{map, success},
    multi::many1, 
    sequence::{delimited, pair, separated_pair}, 
    IResult
};

use super::{
    literals::{
        parse_bigimm, 
        parse_imm, 
        parse_label_definition, 
        parse_label_name
    }, 
    parse_multiline_comments, 
    ByteData, DWordData, HalfData, LabelRecog, MemData, WordData
};

fn parse_byte(input: &str) -> IResult<&str, MemData> {
    map(
        many1(
            alt((
                map(parse_imm, |imm| imm.into()),
                map(parse_label_name, |label| ByteData::String(label.to_string()))
            ))
        ),
        |data_vec| MemData::Bytes(data_vec)
    )(input)
}

fn parse_half(input: &str) -> IResult<&str, MemData> {
    map(
        many1(
            alt((
                map(parse_imm, |imm| imm.into()),
                map(parse_label_name, |label| HalfData::String(label.to_string()))
            ))
        ),
        |data_vec| MemData::Halfs(data_vec)
    )(input)
}

fn parse_word(input: &str) -> IResult<&str, MemData> {
    map(
        many1(
            alt((
                map(parse_bigimm, |imm| imm.into()),
                map(parse_label_name, |label| WordData::String(label.to_string()))
            ))
        ),
        |data_vec| MemData::Words(data_vec)
    )(input)
}

fn parse_dword(input: &str) -> IResult<&str, MemData> {
    map(
        many1(
            alt((
                map(parse_bigimm, |imm| imm.into()),
                map(parse_label_name, |label| DWordData::String(label.to_string()))
            ))
        ),
        |data_vec| MemData::DWords(data_vec)
    )(input)
}

fn string_to_le_words(input: String) -> MemData {
    let mut vec_data = vec![];
    for word in input.as_bytes().chunks_exact(4) {
        vec_data.push(WordData::Word((0 as i64) + ((word[3] as i64) << 24) + ((word[2] as i64) << 16) + ((word[1] as i64) << 8) + word[0] as i64));
    };
    MemData::Words(vec_data)
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
                MemData::Words(vec_data) 
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
                ascii_string.push_str("\0");
                string_to_le_words(ascii_string)
            } 
        )),
        separated_pair(tag(".string"), space1, map(
            delimited(nom::character::complete::char('"'), is_not("\n\";"), nom::character::complete::char('"')),
            |ascii_str: &str| {
                let mut ascii_string = ascii_str.to_string(); 
                ascii_string.push_str("\0");
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

pub fn parse<'a>(input: &'a str, symbol_map: &mut LabelRecog) -> IResult<&'a str, Vec<MemData>> {

    let mut rest = input;
    let mut next_free_ptr = 0;

    Ok((input, vec![]))
}