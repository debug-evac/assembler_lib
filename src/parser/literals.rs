/*
 * Copyright (c) 2023 Steven Becker
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

use nom::{
    IResult,
    bytes::complete::{
        is_a,
        tag,
        tag_no_case
    },
    branch::alt,
    combinator::{
        opt,
        map_res,
        recognize,
        success,
    },
    character::complete::{
        alpha1,
        digit1,
        hex_digit1,
        alphanumeric0,
        alphanumeric1,
        one_of
    },
    sequence::{
        tuple,
        pair
    }
};

use crate::common::{Imm, Reg};

pub fn parse_label_name(input: &str) -> IResult<&str, &str> {
    recognize(
        pair(alpha1, alphanumeric0)
    )(input)
}

pub fn parse_label_definition(input: &str) -> IResult<&str, &str> {
    let (rest, parsed) = pair(
        recognize(
            pair(
                opt(nom::character::complete::char('.')),
                parse_label_name
        )),
        nom::character::complete::char(':')
    )(input)?;

    Ok((rest, parsed.0))
}

pub fn parse_label_definition_priv(input: &str) -> IResult<&str, &str> {
    let (rest, parsed) = pair(
        recognize(
            pair(
                nom::character::complete::char('_'),
                parse_label_name
        )),
        nom::character::complete::char(':')
    )(input)?;

    Ok((rest, parsed.0))
}

fn from_hex(input: &str) -> Result<Imm, std::num::ParseIntError> {
    let num_str = input.to_lowercase();
    if let Some(number) = num_str.strip_suffix('u') {
        Imm::from_str_radix(number, 16)
    } else if let Some(number) = num_str.strip_suffix('s') {
        match Imm::from_str_radix(number, 16) {
            Ok(num) => {
                let num_zero = num.leading_zeros();
                let or_num = -1 << (i32::BITS - num_zero);
                Ok(num | or_num)
            },
            Err(e) => Err(e),
        }
    } else {
        Imm::from_str_radix(input, 16)
    }
}

fn from_binary(input: &str) -> Result<Imm, std::num::ParseIntError> {
    let num_str = input.to_lowercase();
    if let Some(number) = num_str.strip_suffix('u') {
        Imm::from_str_radix(number, 2)
    } else if let Some(number) = num_str.strip_suffix('s') {
        match Imm::from_str_radix(number, 2) {
            Ok(num) => {
                let num_zero = num.leading_zeros();
                let or_num = -1 << (i32::BITS - num_zero);
                Ok(num | or_num)
            },
            Err(e) => Err(e),
        }
    } else {
        Imm::from_str_radix(input, 2)
    }
}

pub fn parse_imm(input: &str) -> IResult<&str, Imm> {
    if let Ok((rest, Some(_))) = opt(tag_no_case::<&str, &str, nom::error::Error<&str>>("0x"))(input) {
        // Hexadecimal
        map_res(
            recognize(tuple((hex_digit1, opt(one_of("suSU"))))), 
            from_hex
        )(rest)
    } else if let Ok((rest, Some(_))) = opt(tag_no_case::<&str, &str, nom::error::Error<&str>>("0b"))(input) {
        // Binary
        map_res(
            recognize(tuple((is_a("01"), opt(one_of("suSU"))))), 
            from_binary
        )(rest)
    } else {
        // Decimal
        map_res(
            recognize(tuple((opt(tag("-")), digit1))),
            str::parse
        )(input)
    }
}

pub fn parse_reg(input: &str) -> IResult<&str, Reg> {
    let (rest, reg) = alt((
        pair(
            nom::character::complete::char('x'), 
            map_res(map_res(digit1, str::parse::<u8>), Reg::num_to_enum)
        ),
        pair(
            success('n'),
            map_res(alphanumeric1, Reg::str_to_enum)
        )
    ))(input)?;
    Ok((rest, reg.1))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_label() {
        assert_ne!(parse_label_definition("invalid"), Ok(("", "invalid")));
        assert_eq!(parse_label_definition("valid0:"), Ok(("", "valid0")));
        assert_ne!(parse_label_definition("invalid :"), Ok(("", "invalid")));
        assert_ne!(parse_label_definition(" "), Ok(("", "")));
        assert_eq!(parse_label_definition("valid:"), Ok(("", "valid")));
        assert_ne!(parse_label_definition("0invalid:"), Ok(("", "0invalid")));
        assert_eq!(parse_label_definition("v415alid:"), Ok(("", "v415alid")));
        assert_eq!(parse_label_definition(".veryvalid:"), Ok(("", ".veryvalid")));
    }

    #[test]
    fn test_parse_label_privileged() {
        assert_ne!(parse_label_definition_priv("invalid"), Ok(("", "invalid")));
        assert_ne!(parse_label_definition_priv("invalid0:"), Ok(("", "invalid0")));
        assert_ne!(parse_label_definition_priv("invalid :"), Ok(("", "invalid")));
        assert_ne!(parse_label_definition_priv(" "), Ok(("", "")));
        assert_eq!(parse_label_definition_priv("_valid:"), Ok(("", "_valid")));
        assert_ne!(parse_label_definition_priv("0invalid:"), Ok(("", "0invalid")));
        assert_eq!(parse_label_definition_priv("_v415alid:"), Ok(("", "_v415alid")));
        assert_eq!(parse_label_definition_priv("_veryvalid:"), Ok(("", "_veryvalid")));
    }

    #[test]
    fn test_parse_imm() {
        assert_ne!(parse_imm("invalid"), Ok(("", 0)));
        assert_ne!(parse_imm(" "), Ok(("", 0)));
        assert_eq!(parse_imm("10"), Ok(("", 10)));
        assert_eq!(parse_imm("0xA"), Ok(("", 10)));
        assert_eq!(parse_imm("-10"), Ok(("", -10)));
        assert_eq!(parse_imm("0xAAs"), Ok(("", -86)));
        assert_eq!(parse_imm("0xAAS"), Ok(("", -86)));
        assert_eq!(parse_imm("0b1111u"), Ok(("", 15)));
        assert_eq!(parse_imm("0b1100s"), Ok(("", -4)));
    }

    #[test]
    fn test_parse_reg() {
        assert_ne!(parse_reg("invalid"), Ok(("", Reg::NA)));
        assert_ne!(parse_reg(" "), Ok(("", Reg::NA)));
        assert_ne!(parse_reg("  "), Ok(("", Reg::NA)));
        assert_eq!(parse_reg("x3"), Ok(("", Reg::G3)));
        assert_eq!(parse_reg("s1"), Ok(("", Reg::G9)));
        assert_eq!(parse_reg("zero"), Ok(("", Reg::G0)));
        assert_eq!(parse_reg("t3"), Ok(("", Reg::G28)));
    }
}