/*
 * Copyright (c) 2023 Steven Becker
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

use winnow::{
    ascii::{alpha1, alphanumeric0, alphanumeric1, digit1, hex_digit1},
    combinator::{alt, opt, success, terminated, preceded},
    token::{one_of, tag, tag_no_case, take_while},
    PResult,
    Parser
};

use crate::common::{Imm, Reg};

pub fn parse_data_segment_id<'a>(input: &mut &'a str) -> PResult<&'a str> {
    ('.', tag("data"))
    .recognize()
    .parse_next(input)
}

pub fn parse_text_segment_id<'a>(input: &mut &'a str) -> PResult<&'a str> {
    ('.', tag("text"))
    .recognize()
    .parse_next(input)
}

macro_rules! label_name {
    ($($ins:expr)?) => {
        (
            opt('.'),
            $($ins,)?
            alpha1,
            alphanumeric0
        )
        .recognize()
    };
}

pub fn parse_label_name<'a>(input: &mut &'a str) -> PResult<&'a str> {
    label_name!().parse_next(input)
}

pub fn parse_label_definition<'a>(input: &mut &'a str) -> PResult<&'a str> {
    terminated(
        parse_label_name,
        ':'
    ).parse_next(input)
}

pub fn parse_label_definition_priv<'a>(input: &mut &'a str) -> PResult<&'a str> {
    terminated(
        label_name!('_'),
        ':'
    ).parse_next(input)
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

fn from_bighex(input: &str) -> Result<i128, std::num::ParseIntError> {
    let num_str = input.to_lowercase();
    if let Some(number) = num_str.strip_suffix('u') {
        i128::from_str_radix(number, 16)
    } else if let Some(number) = num_str.strip_suffix('s') {
        match i128::from_str_radix(number, 16) {
            Ok(num) => {
                let num_zero = num.leading_zeros();
                let or_num = -1 << (i32::BITS - num_zero);
                Ok(num | or_num)
            },
            Err(e) => Err(e),
        }
    } else {
        i128::from_str_radix(input, 16)
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

fn from_bigbinary(input: &str) -> Result<i128, std::num::ParseIntError> {
    let num_str = input.to_lowercase();
    if let Some(number) = num_str.strip_suffix('u') {
        i128::from_str_radix(number, 2)
    } else if let Some(number) = num_str.strip_suffix('s') {
        match i128::from_str_radix(number, 2) {
            Ok(num) => {
                let num_zero = num.leading_zeros();
                let or_num = -1 << (i32::BITS - num_zero);
                Ok(num | or_num)
            },
            Err(e) => Err(e),
        }
    } else {
        i128::from_str_radix(input, 2)
    }
}

pub fn parse_bigimm(input: &mut &str) -> PResult<i128> {
    if let Ok(Some(_)) = opt(tag_no_case::<&str, &str, winnow::error::InputError<&str>>("0x")).parse_next(input) {
        // Hexadecimal
        (hex_digit1, opt(one_of(['s', 'u', 'S', 'U']))).recognize()
        .try_map(from_bighex)
        .parse_next(input)
    } else if let Ok(Some(_)) = opt(tag_no_case::<&str, &str, winnow::error::InputError<&str>>("0b")).parse_next(input) {
        // Binary
        (take_while(1.., ['0', '1']), opt(one_of(['s', 'u', 'S', 'U']))).recognize()
        .try_map(from_bigbinary)
        .parse_next(input)
    } else {
        // Decimal
        (opt(tag("-")), digit1).recognize()
        .try_map(str::parse)
        .parse_next(input)
    }
}

pub fn parse_imm(input: &mut &str) -> PResult<Imm> {
    if let Ok(Some(_)) = opt(tag_no_case::<&str, &str, winnow::error::InputError<&str>>("0x")).parse_next(input) {
        // Hexadecimal
        (hex_digit1, opt(one_of(['s', 'u', 'S', 'U']))).recognize()
        .try_map(from_hex)
        .parse_next(input)
    } else if let Ok(Some(_)) = opt(tag_no_case::<&str, &str, winnow::error::InputError<&str>>("0b")).parse_next(input) {
        // Binary
        (take_while(1.., ['0', '1']), opt(one_of(['s', 'u', 'S', 'U']))).recognize()
        .try_map(from_binary)
        .parse_next(input)
    } else {
        parse_decimal(input)
    }
}

pub fn parse_decimal(input: &mut &str) -> PResult<Imm> {
    (opt(tag("-")), digit1).recognize()
    .try_map(str::parse)
    .parse_next(input)
}

pub fn parse_reg(input: &mut &str) -> PResult<Reg> {
    alt((
        preceded(
            'x', 
            digit1.try_map(|x: &str| {
                let num = str::parse::<u8>(x).unwrap_or(50);
                Reg::num_to_enum(num)
            })
        ),
        preceded(
            success('n'),
            alphanumeric1.try_map(Reg::str_to_enum)
        )
    )).parse_next(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_label() {
        assert_ne!(parse_label_definition(&mut "invalid"), Ok("invalid"));
        assert_eq!(parse_label_definition(&mut "valid0:"), Ok("valid0"));
        assert_ne!(parse_label_definition(&mut "invalid :"), Ok("invalid"));
        assert_ne!(parse_label_definition(&mut " "), Ok(""));
        assert_eq!(parse_label_definition(&mut "valid:"), Ok("valid"));
        assert_ne!(parse_label_definition(&mut "0invalid:"), Ok( "0invalid"));
        assert_eq!(parse_label_definition(&mut "v415alid:"), Ok("v415alid"));
        assert_eq!(parse_label_definition(&mut ".veryvalid:"), Ok(".veryvalid"));
    }

    #[test]
    fn test_parse_label_privileged() {
        assert_ne!(parse_label_definition_priv(&mut "invalid"), Ok("invalid"));
        assert_ne!(parse_label_definition_priv(&mut "invalid0:"), Ok("invalid0"));
        assert_ne!(parse_label_definition_priv(&mut "invalid :"), Ok("invalid"));
        assert_ne!(parse_label_definition_priv(&mut " "), Ok(""));
        assert_eq!(parse_label_definition_priv(&mut "_valid:"), Ok("_valid"));
        assert_ne!(parse_label_definition_priv(&mut "0invalid:"), Ok("0invalid"));
        assert_eq!(parse_label_definition_priv(&mut "_v415alid:"), Ok("_v415alid"));
        assert_eq!(parse_label_definition_priv(&mut "_veryvalid:"), Ok("_veryvalid"));
    }

    #[test]
    fn test_parse_imm() {
        assert_ne!(parse_imm(&mut "invalid"), Ok(0));
        assert_ne!(parse_imm(&mut " "), Ok(0));
        assert_eq!(parse_imm(&mut "10"), Ok(10));
        assert_eq!(parse_imm(&mut "0xA"), Ok(10));
        assert_eq!(parse_imm(&mut "-10"), Ok(-10));
        assert_eq!(parse_imm(&mut "0xAAs"), Ok(-86));
        assert_eq!(parse_imm(&mut "0xAAS"), Ok(-86));
        assert_eq!(parse_imm(&mut "0b1111u"), Ok(15));
        assert_eq!(parse_imm(&mut "0b1100s"), Ok(-4));
    }

    #[test]
    fn test_parse_reg() {
        assert_ne!(parse_reg(&mut "invalid"), Ok(Reg::G0));
        assert_ne!(parse_reg(&mut " "), Ok(Reg::G0));
        assert_ne!(parse_reg(&mut "  "), Ok(Reg::G0));
        assert_eq!(parse_reg(&mut "x3"), Ok(Reg::G3));
        assert_eq!(parse_reg(&mut "s1"), Ok(Reg::G9));
        assert_eq!(parse_reg(&mut "zero"), Ok(Reg::G0));
        assert_eq!(parse_reg(&mut "t3"), Ok(Reg::G28));
    }
}