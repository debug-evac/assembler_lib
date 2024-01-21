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
        value,
        map_res,
        recognize
    },
    character::complete::{
        alpha1,
        digit1,
        hex_digit1,
        multispace0,
        multispace1, 
        alphanumeric0,
        alphanumeric1,
        one_of
    },
    sequence::{
        tuple,
        separated_pair,
    }, multi::separated_list1,
};
use std::cmp::Ordering;
use std::collections::{HashSet, BTreeMap};
use std::borrow::Cow;

use crate::common::*;

#[derive(Clone, Debug)]
enum IntermediateOp {
    Call,
    Tail,
    Jal,
    J,
    Jr,
    Jalr,
    Lui,
    Auipc,
    Li,
    Mv,
    Addi,
    Slti,
    Sltiu,
    Xori,
    Ori,
    Andi,
    Srr,
    Slr,
    Add,
    Sub,
    Xor,
    Or,
    And,
    Slt,
    Sltu,
    Sll,
    Srl,
    Sra,
    Div,
    Mul,
    Remu,
    Xnor,
    Nor,
    Equal,
    Push,
    Pop,
    La
}

pub struct Subroutines {
    code_str_vec: HashSet<String>
}

impl Subroutines {
    const MUL_SUB: &'static str = r#"
_MUL:
    addi a7, zero, 0
    addi a6, zero, 1
    mv a2, a0
    mv a3, a1
    mv a0, zero
    mv a1, zero
    blt a2, a3, 32
    and a4, a6, a3
    beq a4, zero, 12
    sll a5, a2, a7
    add a0, a0, a5
    addi a7, a7, 1
    slli a6, a6, 1
    bge a3, a6, -24
    ret
    and a4, a6, a2
    beq a4, zero, 12
    sll a5, a3, a7
    add a0, a0, a5
    addi a7, a7, 1
    slli a6, a6, 1
    bge a2, a6, -24
    ret
"#;
    const DIV_SUB: &'static str = r#"
_DIV:
    addi a7, zero, 1
    mv a2, a0
    mv a3, a1
    mv a0, zero
    mv a1, zero
    bne a2, a3, 16
    slli a3, a3, 1
    sub a2, a2, a3
    add a0, a0, a7
    blt a2, a3, 40
    slli a3, a3, 1
    slli a7, a7, 1
    blt a3, a2, -8
    srli a3, a3, 1
    srli a7, a7, 1
    sub a2, a2, a3
    add a0, a0, a7
    bne a2, zero, -32
    beq zero, zero, 44
    srli a3, a3, 1
    srli a7, a7, 1
    blt a2, a3, -8
    slli a3, a3, 1
    slli a7, a7, 1
    beq a2, a3, 8
    srli a3, a3, 1
    srli a7, a7, 1
    sub a2, a2, a3
    add a0, a0, a7
    bne a2, zero, -80
    ret
"#;

//just one direction
    const REMU_SUB: &'static str = r#"
_REMU:
    addi a7, zero, 1
    mv a2, a0
    mv a3, a1
    mv a0, zero
    mv a1, zero
    bne a2, a3, 16
    slli a3, a3, 1
    sub a2, a2, a3
    add a0, a0, a7
    blt a2, a3, 40
    slli a3, a3, 1
    slli a7, a7, 1
    blt a3, a2, -8
    srli a3, a3, 1
    srli a7, a7, 1
    sub a2, a2, a3
    add a0, a0, a7
    bne a2, zero, -32
    beq zero, zero, 48
    srli a3, a3, 1
    srli a7, a7, 1
    blt a2, a3, -8
    slli a3, a3, 1
    slli a7, a7, 1
    beq a2, a3, 8
    srli a3, a3, 1
    srli a7, a7, 1
    sub a2, a2, a3
    add a0, a0, a7
    blt a2, a3, 8
    bne a2, zero, -84
    mv a0, a2
    ret
"#;

    const SRR_SUB: &'static str = r#"
_SRR:
    sub a4, zero, a1
    srl a2, a0, a1
    sll a3, a0, a4
    or a0, a2, a3
    ret
"#;
    const SLR_SUB: &'static str = r#"
_SLR:
    sub a4, zero, a1
    sll a2, a0, a1
    srl a3, a0, a4
    or a0, a2, a3
    ret
"#;

    pub fn new() -> Self {
        let code_str_vec = HashSet::new();

        Subroutines{ code_str_vec }
    }

    pub fn mul_defined(&mut self) {
        self.code_str_vec.insert(Self::MUL_SUB.to_string());
    }

    pub fn div_defined(&mut self) {
        self.code_str_vec.insert(Self::DIV_SUB.to_string());
    }

    pub fn remu_defined(&mut self) {
        self.code_str_vec.insert(Self::REMU_SUB.to_string());
    }

    pub fn srr_defined(&mut self) {
        self.code_str_vec.insert(Self::SRR_SUB.to_string());
    }

    pub fn slr_defined(&mut self) {
        self.code_str_vec.insert(Self::SLR_SUB.to_string());
    }

    pub fn get_code(&self) -> Vec<String> {
        self.code_str_vec.iter().cloned().collect()
    }
}

#[derive(Debug, Clone)]
pub struct LabelInsertError {
    label: String,
}

impl LabelInsertError {
    #[allow(dead_code)]
    pub fn new(label: String) -> LabelInsertError {
        LabelInsertError { label }
    }
}

impl std::fmt::Display for LabelInsertError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{} already exists!", self.label)
    }
}

impl <'a> From<Instruction> for Operation <'a> {
    fn from(item: Instruction) -> Self {
        Operation::Instr(item)
    }
}

impl <'a> From<MacroInstr> for Operation <'a> {
    fn from(item: MacroInstr) -> Self {
        Operation::Macro(item)
    }
}

fn parse_label_name(input: &str) -> IResult<&str, Cow<str>> {
    let (rest, parsed) = alpha1(input)?;
    let (rest, parsed_l) = alphanumeric0(rest)?;

    Ok((rest, Cow::from(format!("{}{}", parsed, parsed_l))))
}

fn parse_label_definition(input: &str) -> IResult<&str, Cow<str>> {
    let (rest, scope) = opt(tag("."))(input)?;
    let (rest, parsed) = parse_label_name(rest)?;
    let (rest, _) = tag(":")(rest)?;

    match scope {
        Some(local) => Ok((rest, Cow::from(local) + parsed)),
        None => Ok((rest, parsed))
    }
}

fn parse_label_definition_priv(input: &str) -> IResult<&str, Cow<str>> {
    let (rest, req_underscore) = tag("_")(input)?;
    let (rest, parsed) = parse_label_name(rest)?;
    let (rest, _) = tag(":")(rest)?;

    Ok((rest, Cow::from(req_underscore) + parsed))
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

fn parse_imm(input: &str) -> IResult<&str, Imm> {
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

fn parse_reg(input: &str) -> IResult<&str, Reg> {
    let (rest, abs_reg) = opt(tag("x"))(input)?;

    match abs_reg {
        Some(_) => {
            let (rest, reg) = map_res(digit1, str::parse)(rest)?;
            let real_reg = Reg::num_to_enum(&reg);

            if real_reg == Reg::NA {
                println!("WARNING! Reg::NA RECEIVED! Rest = {}", rest);
                todo!("Implement own error!");
            } else {
                Ok((rest, real_reg))
            }
        },
        None => {
            let (rest, reg) = map_res(alphanumeric1, Reg::str_to_enum)(rest)?;

            Ok((rest, reg))
        },
    }
}

// ld x1,0x01 OR ld x1, 0x01
fn parse_seper(input: &str) -> IResult<&str, &str> {
    let (rest, not_needed) = tag(",")(input)?;
    let (rest, _) = opt(tag(" "))(rest)?;

    Ok((rest, not_needed))
}

fn parse_instr_args_seper(input: &str) -> IResult<&str, &str> {
    is_a(" \t")(input)
}

fn parse_macro_noparm(input: &str) -> IResult<&str, Operation> {
    let (rest, instr) = alt((
        value(Operation::Instr(Instruction::Addi(Reg::G0, Reg::G0, 0)), tag("nop")),
        value(Operation::Instr(Instruction::Jalr(Reg::G0, Reg::G1, 0)), tag("ret")),
    ))(input)?;

    Ok((rest, instr))
}

fn parse_macro_1labl(input: &str) -> IResult<&str, Operation> {
    let (rest, macro_in) = alt((
        value(IntermediateOp::Call, tag("call")),
        value(IntermediateOp::Tail, tag("tail")),

        value(IntermediateOp::Jal, tag("jal")),
        value(IntermediateOp::J, tag("j")),
    ))(input)?;
    let (rest, _) = parse_instr_args_seper(rest)?;
    let (rest, labl) = parse_label_name(rest)?;

    let instr = match macro_in {
        IntermediateOp::Call => MacroInstr::CallLabl(labl.to_string()).into(),
        IntermediateOp::Tail => MacroInstr::TailLabl(labl.to_string()).into(),
        IntermediateOp::J => MacroInstr::Jal(Reg::G0, labl.to_string()).into(),
        IntermediateOp::Jal => MacroInstr::Jal(Reg::G1, labl.to_string()).into(),

        op => panic!("[Error] Could not map parsed instruction to internal data structure: {:?}", op),
    };

    Ok((rest, instr))
}

fn parse_macro_1imm(input: &str) -> IResult<&str, Operation> {
    let (rest, macro_in) = alt((
        value(IntermediateOp::Call, tag("call")),
        value(IntermediateOp::Tail, tag("tail")),

        value(IntermediateOp::Jal, tag("jal")),
        value(IntermediateOp::J, tag("j")),
    ))(input)?;
    let (rest, _) = parse_instr_args_seper(rest)?;
    let (rest, imm) = parse_imm(rest)?;

    let instr = match macro_in {
        IntermediateOp::Call => MacroInstr::CallImm(imm).into(),
        IntermediateOp::Tail => MacroInstr::TailImm(imm).into(),
        IntermediateOp::J => Instruction::Jal(Reg::G0, imm).into(),
        IntermediateOp::Jal => Instruction::Jal(Reg::G1, imm).into(),
        op => panic!("[Error] Could not map parsed instruction to internal data structure: {:?}", op),
    };

    Ok((rest, instr))
}

fn parse_macro_1reg(input: &str) -> IResult<&str, Operation> {
    let (rest, macro_in) = alt((
        value(IntermediateOp::Jr, tag("jr")),
        value(IntermediateOp::Jalr, tag("jalr")),
    ))(input)?;
    let (rest, _) = parse_instr_args_seper(rest)?;
    let (rest, register) = parse_reg(rest)?;

    let instr = match macro_in {
        IntermediateOp::Jr => Instruction::Jalr(Reg::G0, register, 0),
        IntermediateOp::Jalr => Instruction::Jalr(Reg::G1, register, 0),
        op => panic!("[Error] Could not map parsed instruction to internal data structure: {:?}", op),
    };

    Ok((rest, instr.into()))
}

fn parse_macro_1labl1reg(input: &str) -> IResult<&str, Operation> {
    let (rest, inter) = alt((
        value(IntermediateOp::Lui, tag("lui")),
        value(IntermediateOp::Auipc, tag("auipc")),
        value(IntermediateOp::Jal, tag("jal")),
        value(IntermediateOp::La, tag("la")),
    ))(input)?;
    let (rest, _) = parse_instr_args_seper(rest)?;
    let (rest, args) = separated_pair(parse_reg, parse_seper, parse_label_name)(rest)?;

    let instr = match inter {
        IntermediateOp::Lui => MacroInstr::Lui(args.0, args.1.to_string()).into(),
        IntermediateOp::Auipc => MacroInstr::Auipc(args.0, args.1.to_string(), Part::None).into(),
        IntermediateOp::Jal => MacroInstr::Jal(args.0, args.1.to_string()).into(),
        IntermediateOp::La => MacroInstr::LaLabl(args.0, args.1.to_string()).into(),
        op => panic!("[Error] Could not map parsed instruction to internal data structure: {:?}", op),
    };

    Ok((rest, instr))
}

// ld x3, 0x30
fn parse_inst_1imm1reg(input: &str) -> IResult<&str, Operation> {
    let (rest, instr) = alt((
        value(IntermediateOp::Lui, tag("lui")),
        value(IntermediateOp::Auipc, tag("auipc")),
        value(IntermediateOp::Jal, tag("jal")),

        value(IntermediateOp::Li, tag("li")),
        value(IntermediateOp::La, tag("la")),
    ))(input)?;
    let (rest, _) = parse_instr_args_seper(rest)?;
    let (rest, args) = separated_pair(parse_reg, parse_seper, parse_imm)(rest)?;

    let instr = match instr {
        IntermediateOp::Lui => Instruction::Lui(args.0, args.1).into(),
        IntermediateOp::Auipc => Instruction::Auipc(args.0, args.1).into(),
        IntermediateOp::Jal => Instruction::Jal(args.0, args.1).into(),

        IntermediateOp::Li => MacroInstr::Li(args.0, args.1).into(),
        IntermediateOp::La => MacroInstr::LaImm(args.0, args.1).into(),

        op => panic!("[Error] Could not map parsed instruction to internal data structure: {:?}", op),
    };

    Ok((rest, instr))
}

fn parse_macro_2reg(input: &str) -> IResult<&str, Operation> {
    let (rest, instr) = (
        value(IntermediateOp::Mv, tag("mv"))
    )(input)?;
    let (rest, _) = parse_instr_args_seper(rest)?;
    let (rest, args) = separated_pair(parse_reg, parse_seper, parse_reg)(rest)?;

    let instr = match instr {
        IntermediateOp::Mv => Instruction::Addi(args.0, args.1, 0),
        op  => panic!("[Error] Could not map parsed instruction to internal data structure: {:?}", op),
    };

    Ok((rest, instr.into()))
}

fn parse_macro_1labl2reg(input: &str) -> IResult<&str, Operation> {
    let (rest, instr) = alt((
        value(MacroInstr::Beq(Reg::NA, Reg::NA, String::new()), tag("beq")),
        value(MacroInstr::Bne(Reg::NA, Reg::NA, String::new()), tag("bne")),
        value(MacroInstr::Bltu(Reg::NA, Reg::NA, String::new()), tag("bltu")),
        value(MacroInstr::Bgeu(Reg::NA, Reg::NA, String::new()), tag("bgeu")),
        value(MacroInstr::Blt(Reg::NA, Reg::NA, String::new()), tag("blt")),
        value(MacroInstr::Bge(Reg::NA, Reg::NA, String::new()), tag("bge")),

        value(MacroInstr::Jalr(Reg::NA, Reg::NA, String::new(), Part::None), tag("jalr")),

        value(MacroInstr::Slli(Reg::NA, Reg::NA, String::new()), tag("slli")),
        value(MacroInstr::Srli(Reg::NA, Reg::NA, String::new()), tag("srli")),
        value(MacroInstr::Srai(Reg::NA, Reg::NA, String::new()), tag("srai")),

        value(MacroInstr::Sb(Reg::NA, Reg::NA, String::new(), Part::None), tag("sb")),
        value(MacroInstr::Sh(Reg::NA, Reg::NA, String::new(), Part::None), tag("sh")),
        value(MacroInstr::Sw(Reg::NA, Reg::NA, String::new(), Part::None), tag("sw")),

        value(MacroInstr::Lbu(Reg::NA, Reg::NA, String::new()), tag("lbu")),
        value(MacroInstr::Lhu(Reg::NA, Reg::NA, String::new()), tag("lhu")),
        value(MacroInstr::Lb(Reg::NA, Reg::NA, String::new(), Part::None), tag("lb")),
        value(MacroInstr::Lh(Reg::NA, Reg::NA, String::new(), Part::None), tag("lh")),
        value(MacroInstr::Lw(Reg::NA, Reg::NA, String::new(), Part::None), tag("lw")),
    ))(input)?;
    let (rest, _) = parse_instr_args_seper(rest)?;
    let (rest, args) = tuple((parse_reg, parse_seper, parse_reg, parse_seper, parse_label_name))(rest)?;

    let instr = match instr {
        MacroInstr::Beq(_, _, _) => MacroInstr::Beq(args.0, args.2, args.4.to_string()),
        MacroInstr::Bne(_, _, _) => MacroInstr::Bne(args.0, args.2, args.4.to_string()),
        MacroInstr::Blt(_, _, _) => MacroInstr::Blt(args.0, args.2, args.4.to_string()),
        MacroInstr::Bltu(_, _, _) => MacroInstr::Bltu(args.0, args.2, args.4.to_string()),
        MacroInstr::Bge(_, _, _) => MacroInstr::Bge(args.0, args.2, args.4.to_string()),
        MacroInstr::Bgeu(_, _, _) => MacroInstr::Bgeu(args.0, args.2, args.4.to_string()),

        MacroInstr::Jalr(_, _, _, ir) => MacroInstr::Jalr(args.0, args.2, args.4.to_string(), ir),

        MacroInstr::Slli(_, _, _) => MacroInstr::Slli(args.0, args.2, args.4.to_string()),
        MacroInstr::Srli(_, _, _) => MacroInstr::Srli(args.0, args.2, args.4.to_string()),
        MacroInstr::Srai(_, _, _) => MacroInstr::Srai(args.0, args.2, args.4.to_string()),

        MacroInstr::Sb(_, _, _, ir) => MacroInstr::Sb(args.0, args.2, args.4.to_string(), ir),
        MacroInstr::Sh(_, _, _, ir) => MacroInstr::Sh(args.0, args.2, args.4.to_string(), ir),
        MacroInstr::Sw(_, _, _, ir) => MacroInstr::Sw(args.0, args.2, args.4.to_string(), ir),

        MacroInstr::Lb(_, _, _, ir) => MacroInstr::Lb(args.0, args.2, args.4.to_string(), ir),
        MacroInstr::Lbu(_, _, _) => MacroInstr::Lbu(args.0, args.2, args.4.to_string()),
        MacroInstr::Lh(_, _, _, ir) => MacroInstr::Lh(args.0, args.2, args.4.to_string(), ir),
        MacroInstr::Lhu(_, _, _) => MacroInstr::Lhu(args.0, args.2, args.4.to_string()),
        MacroInstr::Lw(_, _, _, ir) => MacroInstr::Lw(args.0, args.2, args.4.to_string(), ir),

        op => panic!("[Error] Could not map parsed instruction to internal data structure: {:?}", op),
    };

    Ok((rest, instr.into()))
}

fn parse_inst_1imm2reg_lw(input: &str) -> IResult<&str, Operation> {
    let (rest, instr) = alt((
        value(Instruction::Beq(Reg::NA, Reg::NA, 0), tag("beq")),
        value(Instruction::Bne(Reg::NA, Reg::NA, 0), tag("bne")),
        value(Instruction::Blt(Reg::NA, Reg::NA, 0), tag("blt")),
        value(Instruction::Bltu(Reg::NA, Reg::NA, 0), tag("bltu")),
        value(Instruction::Bge(Reg::NA, Reg::NA, 0), tag("bge")),
        value(Instruction::Bgeu(Reg::NA, Reg::NA, 0), tag("bgeu")),

        value(Instruction::Slli(Reg::NA, Reg::NA, 0), tag("slli")),
        value(Instruction::Srli(Reg::NA, Reg::NA, 0), tag("srli")),
        value(Instruction::Srai(Reg::NA, Reg::NA, 0), tag("srai")),

        value(Instruction::Sb(Reg::NA, Reg::NA, 0), tag("sb")),
        value(Instruction::Sh(Reg::NA, Reg::NA, 0), tag("sh")),
        value(Instruction::Sw(Reg::NA, Reg::NA, 0), tag("sw")),

        value(Instruction::Lb(Reg::NA, Reg::NA, 0), tag("lb")),
        value(Instruction::Lbu(Reg::NA, Reg::NA, 0), tag("lbu")),
        value(Instruction::Lh(Reg::NA, Reg::NA, 0), tag("lh")),
        value(Instruction::Lhu(Reg::NA, Reg::NA, 0), tag("lhu")),
        value(Instruction::Lw(Reg::NA, Reg::NA, 0), tag("lw")),
    ))(input)?;
    let (rest, _) = parse_instr_args_seper(rest)?;
    let (rest, args) = tuple((parse_reg, parse_seper, parse_reg, parse_seper, parse_imm))(rest)?;

    let instr = match instr {
        Instruction::Beq(_, _, _) => Instruction::Beq(args.0, args.2, args.4),
        Instruction::Bne(_, _, _) => Instruction::Bne(args.0, args.2, args.4),
        Instruction::Blt(_, _, _) => Instruction::Blt(args.0, args.2, args.4),
        Instruction::Bltu(_, _, _) => Instruction::Bltu(args.0, args.2, args.4),
        Instruction::Bge(_, _, _) => Instruction::Bge(args.0, args.2, args.4),
        Instruction::Bgeu(_, _, _) => Instruction::Bgeu(args.0, args.2, args.4),

        Instruction::Slli(_, _, _) => Instruction::Slli(args.0, args.2, args.4),
        Instruction::Srli(_, _, _) => Instruction::Srli(args.0, args.2, args.4),
        Instruction::Srai(_, _, _) => Instruction::Srai(args.0, args.2, args.4),

        Instruction::Sb(_, _, _) => Instruction::Sb(args.0, args.2, args.4),
        Instruction::Sh(_, _, _) => Instruction::Sh(args.0, args.2, args.4),
        Instruction::Sw(_, _, _) => Instruction::Sw(args.0, args.2, args.4),

        Instruction::Lb(_, _, _) => Instruction::Lb(args.0, args.2, args.4),
        Instruction::Lbu(_, _, _) => Instruction::Lbu(args.0, args.2, args.4),
        Instruction::Lh(_, _, _) => Instruction::Lh(args.0, args.2, args.4),
        Instruction::Lhu(_, _, _) => Instruction::Lhu(args.0, args.2, args.4),
        Instruction::Lw(_, _, _) => Instruction::Lw(args.0, args.2, args.4),

        _ => Instruction::NA
    };

    Ok((rest, instr.into()))
}

fn parse_inst_1imm2reg_up(input: &str) -> IResult<&str, Operation> {
    let (rest, instr) = alt((
        value(IntermediateOp::Addi, tag("addi")),

        value(IntermediateOp::Slti, tag("slti")),
        value(IntermediateOp::Sltiu, tag("sltiu")),

        value(IntermediateOp::Xori, tag("xori")),
        value(IntermediateOp::Ori, tag("ori")),
        value(IntermediateOp::Andi, tag("andi")),

        value(IntermediateOp::Jalr, tag("jalr")),

        value(IntermediateOp::Srr, tag("srr")),
        value(IntermediateOp::Slr, tag("slr"))
    ))(input)?;
    let (rest, _) = parse_instr_args_seper(rest)?;
    let (rest, args) = tuple((parse_reg, parse_seper, parse_reg, parse_seper, parse_imm))(rest)?;

    let instr = match instr {
        IntermediateOp::Addi => Instruction::Addi(args.0, args.2, args.4).into(),

        IntermediateOp::Slti => Instruction::Slti(args.0, args.2, args.4).into(),
        IntermediateOp::Sltiu => Instruction::Sltiu(args.0, args.2, args.4).into(),
        IntermediateOp::Xori => Instruction::Xori(args.0, args.2, args.4).into(),
        IntermediateOp::Ori => Instruction::Ori(args.0, args.2, args.4).into(),
        IntermediateOp::Andi => Instruction::Andi(args.0, args.2, args.4).into(),

        IntermediateOp::Jalr => Instruction::Jalr(args.0, args.2, args.4).into(),

        IntermediateOp::Srr => MacroInstr::Srr(args.0, args.2, args.4).into(),
        IntermediateOp::Slr => MacroInstr::Slr(args.0, args.2, args.4).into(),

        op => panic!("[Error] Could not map parsed instruction to internal data structure: {:?}", op),
    };

    Ok((rest, instr))
}

fn parse_inst_3reg(input: &str) -> IResult<&str, Operation> {
    let (rest, instr) = alt((
        value(IntermediateOp::Add, tag("add")),
        value(IntermediateOp::Sub, tag("sub")),

        value(IntermediateOp::Xor, tag("xor")),
        value(IntermediateOp::Or, tag("or")),
        value(IntermediateOp::And, tag("and")),

        value(IntermediateOp::Slt,tag("slt")),
        value(IntermediateOp::Sltu,tag("sltu")),

        value(IntermediateOp::Sll, tag("sll")),
        value(IntermediateOp::Srl, tag("srl")),
        value(IntermediateOp::Sra, tag("sra")),

        value(IntermediateOp::Div, tag("div")),
        value(IntermediateOp::Mul, tag("mul")),
        value(IntermediateOp::Remu, tag("remu")),

        value(IntermediateOp::Xnor, tag("xnor")),
        value(IntermediateOp::Equal, tag("eq")),
        value(IntermediateOp::Nor, tag("nor")),
    ))(input)?;
    let (rest, _) = parse_instr_args_seper(rest)?;
    let (rest, args) = tuple((parse_reg, parse_seper, parse_reg, parse_seper, parse_reg))(rest)?;

    let instr = match instr {
        IntermediateOp::Add => Instruction::Addn(args.0, args.2, args.4).into(),
        IntermediateOp::Sub => Instruction::Subn(args.0, args.2, args.4).into(),

        IntermediateOp::Xor => Instruction::Xor(args.0, args.2, args.4).into(),
        IntermediateOp::Or => Instruction::Or(args.0, args.2, args.4).into(),
        IntermediateOp::And => Instruction::And(args.0, args.2, args.4).into(),

        IntermediateOp::Slt => Instruction::Slt(args.0, args.2, args.4).into(),
        IntermediateOp::Sltu => Instruction::Sltu(args.0, args.2, args.4).into(),

        IntermediateOp::Sll => Instruction::Sll(args.0, args.2, args.4).into(),
        IntermediateOp::Srl => Instruction::Srl(args.0, args.2, args.4).into(),
        IntermediateOp::Sra => Instruction::Sra(args.0, args.2, args.4).into(),

        IntermediateOp::Div => MacroInstr::Divn(args.0, args.2, args.4).into(),
        IntermediateOp::Mul => MacroInstr::Muln(args.0, args.2, args.4).into(),
        IntermediateOp::Remu => MacroInstr::Remu(args.0, args.2, args.4).into(),

        IntermediateOp::Equal => Instruction::Equal(args.0, args.2, args.4).into(),
        IntermediateOp::Xnor => Instruction::Xnor(args.0, args.2, args.4).into(),
        IntermediateOp::Nor => todo!("Not implemented yet!"),

        op => panic!("[Error] Could not map parsed instruction to internal data structure: {:?}", op),
    };

    Ok((rest, instr))
}

fn parse_macro_multiarg(input: &str) -> IResult<&str, Operation> {
    let (rest, instr) = alt((
        value(IntermediateOp::Push, tag("push")),
        value(IntermediateOp::Pop, tag("pop")),
    ))(input)?;
    let (rest, _) = parse_instr_args_seper(rest)?;
    let (rest, args) = separated_list1(parse_seper, parse_reg)(rest)?;

    let instr = match instr {
        IntermediateOp::Push => MacroInstr::Push(args).into(),
        IntermediateOp::Pop => MacroInstr::Pop(args).into(),

        op => panic!("[Error] Could not map parsed instruction to internal data structure: {:?}", op),
    };

    Ok((rest, instr))
}

fn parse_instruction(input: &str) -> IResult<&str, Operation> {
    let (rest, op) = alt((
        parse_macro_noparm,
        parse_macro_1reg,
        parse_macro_2reg,
        parse_macro_1labl2reg,
        parse_inst_1imm2reg_lw,
        parse_macro_1labl,
        parse_macro_1imm,
        parse_macro_1labl1reg,
        parse_inst_1imm1reg,
        parse_inst_1imm2reg_up,
        parse_inst_3reg,
        parse_macro_multiarg
    ))(input)?;

    Ok((rest, op))
}

// TODO: Incoporate
/*let (rest, res) = alt((
    tuple((
        map(parse_label_definition, |s| Some(s)),
        multispace1,
        map(
            alt((
            parse_instruction,
            parse_multiline_macro
        )),
        |s| Some(s)
        )
    )),
    tuple((
        map(parse_label_definition, |s| Some(s)), 
        success(""), 
        success(None)
    )),
    tuple((
        success(None),
        success(""),
        map(
            alt((
            parse_instruction,
            parse_multiline_macro
        )),
        |s| Some(s)
        )
    )),
))(rest)?;
Ok((rest, (res.0, res.2)))*/
#[allow(clippy::type_complexity)]
fn parse_line(input: &str) -> IResult<&str, (Option<Cow<str>>, Option<Operation>)> {
    let (rest, _) = multispace0(input)?;
    let (rest, label) = opt(parse_label_definition)(rest)?;
    if label.is_some() {
        let (rest, _) = multispace1(rest)?;
        let (rest, instr) = opt(parse_instruction)(rest)?;
        Ok((rest, (label, instr)))
    } else {
        let (rest, instr) = parse_instruction(rest)?;
        Ok((rest, (label, Some(instr))))
    }
}

#[allow(clippy::type_complexity)]
fn parse_line_priv(input: &str) -> IResult<&str, (Option<Cow<str>>, Option<Operation>)> {
    let (rest, _) = multispace0(input)?;
    let (rest, label) = opt(parse_label_definition_priv)(rest)?;
    if label.is_some() {
        let (rest, _) = multispace1(rest)?;
        let (rest, instr) = opt(parse_instruction)(rest)?;
        Ok((rest, (label, instr)))
    } else {
        let (rest, instr) = parse_instruction(rest)?;
        Ok((rest, (label, Some(instr))))
    }
}

fn handle_label_defs(label: &mut Cow<str>, symbol_map: &mut LabelRecog, local_ref_set: &mut HashSet<String>, instr_counter: usize) {
    match label.strip_prefix('.') {
        Some(label) => {
            // Local label; Track definitions and references!
            let label_string = &label.to_string();
            // TODO: Evaluate if .unwrap is appropriate!
            symbol_map.crt_or_def_label(label_string, false, instr_counter.try_into().unwrap());
            local_ref_set.remove(label_string);
        },
        None => {
            // Global label; Do not track definitions and references!
            // TODO: Evaluate if .unwrap is appropriate!
            symbol_map.crt_or_def_label(&label.to_string(), true, instr_counter.try_into().unwrap())
        },
    };
}

fn handle_label_refs(macro_in: &MacroInstr, subroutines: &mut Option<&mut Subroutines>, symbol_map: &mut LabelRecog, local_ref_set: &mut HashSet<String>) {
    #[allow(unreachable_patterns)]
    match macro_in {
        MacroInstr::Addi(_, _, labl, _) |

        MacroInstr::Beq(_, _, labl) | 
        MacroInstr::Bne(_, _, labl) |
        MacroInstr::Blt(_, _, labl) |
        MacroInstr::Bltu(_, _, labl) |
        MacroInstr::Bge(_, _, labl) |
        MacroInstr::Bgeu(_, _, labl) |

        MacroInstr::Jal(_, labl) |
        MacroInstr::Jalr(_, _, labl, _) |

        MacroInstr::Lui(_, labl) |
        MacroInstr::Auipc(_, labl, _) |

        MacroInstr::Slli(_, _, labl) |
        MacroInstr::Srli(_, _, labl) |
        MacroInstr::Srai(_, _, labl) |

        MacroInstr::Lb(_, _, labl, _) |
        MacroInstr::Lh(_, _, labl, _) |
        MacroInstr::Lw(_, _, labl, _) |

        MacroInstr::Lbu(_, _, labl) |
        MacroInstr::Lhu(_, _, labl) |

        MacroInstr::Sb(_, _, labl, _) |
        MacroInstr::Sh(_, _, labl, _) |
        MacroInstr::Sw(_, _, labl, _) | 
        
        MacroInstr::Addi(_, _, labl, _) |
        MacroInstr::CallLabl(labl) |
        MacroInstr::TailLabl(labl) |
        MacroInstr::LaLabl(_, labl) => {
            if !symbol_map.crt_or_ref_label(labl, false) {
                local_ref_set.insert(labl.clone());
            }
        },

        MacroInstr::Muln(_, _, _) => {
            if let Some(subs) = subroutines {
                subs.mul_defined();
            };
            static LABEL: &str = "_MUL";
            if !symbol_map.crt_or_ref_label(&LABEL.to_string(), true) {
                local_ref_set.insert(LABEL.to_string());
            };
        },
        MacroInstr::Divn(_, _, _) => {
            if let Some(subs) = subroutines {
                subs.div_defined();
            };
            static LABEL: &str = "_DIV";
            if !symbol_map.crt_or_ref_label(&LABEL.to_string(), true) {
                local_ref_set.insert(LABEL.to_string());
            };
        },
        MacroInstr::Remu(_, _, _) => {
            if let Some(subs) = subroutines {
                subs.remu_defined();
            };
            static LABEL: &str = "_REMU";
            if !symbol_map.crt_or_ref_label(&LABEL.to_string(), true) {
                local_ref_set.insert(LABEL.to_string());
            };
        },
        MacroInstr::Srr(_, _, _) => {
            if let Some(subs) = subroutines {
                subs.srr_defined();
            };
            static LABEL: &str = "_SRR";
            if !symbol_map.crt_or_ref_label(&LABEL.to_string(), true) {
                local_ref_set.insert(LABEL.to_string());
            };
        },
        MacroInstr::Slr(_, _, _) => {
            if let Some(subs) = subroutines {
                subs.slr_defined();
            };
            static LABEL: &str = "_SLR";
            if !symbol_map.crt_or_ref_label(&LABEL.to_string(), true) {
                local_ref_set.insert(LABEL.to_string());
            };
        },

        _ => (),
    }
}

fn handle_abs_addr_label_conv<'b>(
    instr_counter: usize,
    abs_to_label_queue: &mut BTreeMap<usize, Vec<usize>>, 
    instr_list: &'b mut [Operation],
    symbol_map: &mut LabelRecog,
    imm: &Imm
) -> Option<Cow<'b, str>> {
    let mut jump_line: usize = match instr_counter as i128 + (*imm / 4) as i128 {
        x if x < 0 => 0,
        x => x.try_into().unwrap()
    };

    match imm.cmp(&0) {
        Ordering::Greater => {
            // cannot look ahead, delegate to later
            jump_line += 1;
            match abs_to_label_queue.get_mut(&jump_line) {
                Some(list) => list.push(instr_counter),
                None => {
                    abs_to_label_queue.insert(jump_line, Vec::from([instr_counter]));
                },
            }
            None
        },
        Ordering::Less => {
            // looking back
            let jump_label: Cow<'_, str>;
            match &instr_list[jump_line] {
                Operation::Instr(instr) => {
                    jump_label = Cow::from("__".to_string() + &jump_line.to_string());
                    symbol_map.crt_def_ref(&jump_label.to_string(), false, jump_line as i128);
                    instr_list[jump_line] = Operation::LablInstr(jump_label.clone(), instr.to_owned());
                },
                Operation::Macro(macro_in) => {
                    jump_label = Cow::from("__".to_string() + &jump_line.to_string());
                    symbol_map.crt_def_ref(&jump_label.to_string(), false, jump_line as i128);
                    instr_list[jump_line] = Operation::LablMacro(jump_label.clone(), macro_in.to_owned());
                },
                Operation::LablInstr(labl, _) |
                Operation::LablMacro(labl, _) |
                Operation::Labl(labl) => {
                    jump_label = labl.clone();
                    symbol_map.set_refd_label(&labl.to_string());
                },
                Operation::Namespace(_) => unreachable!(),
            };
            Some(jump_label)
        },
        Ordering::Equal => None,
    }
}

fn handle_instr_substitution(instr_list: &mut [Operation], elem: &[usize], jump_label: &str) {
    for origin in elem.iter() {
        match &instr_list[*origin] {
            Operation::Instr(instr) => {
                match instr {
                    Instruction::Beq(reg1, reg2, _) => instr_list[*origin] = Operation::Macro(MacroInstr::Beq(reg1.to_owned(), reg2.to_owned(), jump_label.to_string())),
                    Instruction::Bne(reg1, reg2, _) => instr_list[*origin] = Operation::Macro(MacroInstr::Bne(reg1.to_owned(), reg2.to_owned(), jump_label.to_string())),
                    Instruction::Blt(reg1, reg2, _) => instr_list[*origin] = Operation::Macro(MacroInstr::Blt(reg1.to_owned(), reg2.to_owned(), jump_label.to_string())),
                    Instruction::Bltu(reg1, reg2, _) => instr_list[*origin] = Operation::Macro(MacroInstr::Bltu(reg1.to_owned(), reg2.to_owned(), jump_label.to_string())),
                    Instruction::Bge(reg1, reg2, _) => instr_list[*origin] = Operation::Macro(MacroInstr::Bge(reg1.to_owned(), reg2.to_owned(), jump_label.to_string())),
                    Instruction::Bgeu(reg1, reg2, _) => instr_list[*origin] = Operation::Macro(MacroInstr::Bgeu(reg1.to_owned(), reg2.to_owned(), jump_label.to_string())),
                    Instruction::Jal(reg, _) => instr_list[*origin] = Operation::Macro(MacroInstr::Jal(reg.to_owned(), jump_label.to_string())),
                    Instruction::Jalr(reg1, reg2, _) => instr_list[*origin] = Operation::Macro(MacroInstr::Jalr(reg1.to_owned(), reg2.to_owned(), jump_label.to_string(), Part::None)),
                    op => {
                        println!("Matched instr: {:?}", op);
                        unreachable!()
                    },
                }
            },
            Operation::LablInstr(labl, instr) => {
                match instr {
                    Instruction::Beq(reg1, reg2, _) => instr_list[*origin] = Operation::LablMacro(labl.clone(), MacroInstr::Beq(reg1.to_owned(), reg2.to_owned(), jump_label.to_string())),
                    Instruction::Bne(reg1, reg2, _) => instr_list[*origin] = Operation::LablMacro(labl.clone(), MacroInstr::Bne(reg1.to_owned(), reg2.to_owned(), jump_label.to_string())),
                    Instruction::Blt(reg1, reg2, _) => instr_list[*origin] = Operation::LablMacro(labl.clone(), MacroInstr::Blt(reg1.to_owned(), reg2.to_owned(), jump_label.to_string())),
                    Instruction::Bltu(reg1, reg2, _) => instr_list[*origin] = Operation::LablMacro(labl.clone(), MacroInstr::Bltu(reg1.to_owned(), reg2.to_owned(), jump_label.to_string())),
                    Instruction::Bge(reg1, reg2, _) => instr_list[*origin] = Operation::LablMacro(labl.clone(), MacroInstr::Bge(reg1.to_owned(), reg2.to_owned(), jump_label.to_string())),
                    Instruction::Bgeu(reg1, reg2, _) => instr_list[*origin] = Operation::LablMacro(labl.clone(), MacroInstr::Bgeu(reg1.to_owned(), reg2.to_owned(), jump_label.to_string())),
                    Instruction::Jal(reg, _) => instr_list[*origin] = Operation::LablMacro(labl.clone(), MacroInstr::Jal(reg.to_owned(), jump_label.to_string())),
                    Instruction::Jalr(reg1, reg2, _) => instr_list[*origin] = Operation::LablMacro(labl.clone(), MacroInstr::Jalr(reg1.to_owned(), reg2.to_owned(), jump_label.to_string(), Part::None)),
                    op => {
                        println!("Matched labl: {}, matched instr: {:?}", labl, op);
                        unreachable!()
                    },
                }
            },
            op => {
                println!("Matched operation: {:?}", op);
                unreachable!()
            },
        }
    }
}

fn translate_macros<'a>(
    macro_in: &MacroInstr,
    instr_list: &mut Vec<Operation<'a>>,
    accumulator: &mut i128,
    pointer: &mut usize,
    label: Option<Cow<'a, str>>
) {
    match &macro_in {
        MacroInstr::Divn(reg1, reg2, reg3) => {
            let mut right_list = instr_list.split_off(*pointer);
            right_list.remove(0);
            let mut mid_list: Vec<Operation> = vec![];

            if *reg2 != Reg::G10 {
                mid_list.push(Operation::Instr(Instruction::Addi(Reg::G10, reg2.to_owned(), 0)));
            }
            if *reg3 != Reg::G11 {
                mid_list.push(Instruction::Addi(Reg::G11, reg3.to_owned(), 0).into());
            }
            mid_list.push(MacroInstr::Jal(Reg::G1, "_DIV".to_string()).into());
            if *reg1 != Reg::G10 {
                mid_list.push(Instruction::Addi(reg1.to_owned(), Reg::G10, 0).into());
            }

            *accumulator += (mid_list.len() - 1) as i128;
            *pointer += mid_list.len();
            if let Some(labl) = label {
                match mid_list.first().unwrap() {
                    Operation::Instr(instr_in_sec) => mid_list[0] = Operation::LablInstr(labl, instr_in_sec.to_owned()),
                    Operation::Macro(macro_in_sec) => mid_list[0] = Operation::LablMacro(labl, macro_in_sec.to_owned()),
                    _ => unreachable!(),
                }
            }
            instr_list.append(&mut mid_list);
            instr_list.append(&mut right_list);
        },
        MacroInstr::Muln(reg1, reg2, reg3) => {
            let mut right_list = instr_list.split_off(*pointer);
            right_list.remove(0);
            let mut mid_list: Vec<Operation> = vec![];

            if *reg2 != Reg::G10 {
                mid_list.push(Operation::Instr(Instruction::Addi(Reg::G10, reg2.to_owned(), 0)));
            }
            if *reg3 != Reg::G11 {
                mid_list.push(Instruction::Addi(Reg::G11, reg3.to_owned(), 0).into());
            }
            mid_list.push(MacroInstr::Jal(Reg::G1, "_MUL".to_string()).into());
            if *reg1 != Reg::G10 {
                mid_list.push(Instruction::Addi(reg1.to_owned(), Reg::G10, 0).into());
            }

            *accumulator += (mid_list.len() - 1) as i128;
            *pointer += mid_list.len();
            if let Some(labl) = label {
                match mid_list.first().unwrap() {
                    Operation::Instr(instr_in_sec) => mid_list[0] = Operation::LablInstr(labl, instr_in_sec.to_owned()),
                    Operation::Macro(macro_in_sec) => mid_list[0] = Operation::LablMacro(labl, macro_in_sec.to_owned()),
                    _ => unreachable!(),
                }
            }
            instr_list.append(&mut mid_list);
            instr_list.append(&mut right_list);
        },
        MacroInstr::Remu(reg1, reg2, reg3) => {
            let mut right_list = instr_list.split_off(*pointer);
            right_list.remove(0);
            let mut mid_list: Vec<Operation> = vec![];

            if *reg2 != Reg::G10 {
                mid_list.push(Operation::Instr(Instruction::Addi(Reg::G10, reg2.to_owned(), 0)));
            }
            if *reg3 != Reg::G11 {
                mid_list.push(Instruction::Addi(Reg::G11, reg3.to_owned(), 0).into());
            }
            mid_list.push(MacroInstr::Jal(Reg::G1, "_REMU".to_string()).into());
            if *reg1 != Reg::G10 {
                mid_list.push(Instruction::Addi(reg1.to_owned(), Reg::G10, 0).into());
            }

            *accumulator += (mid_list.len() - 1) as i128;
            *pointer += mid_list.len();
            if let Some(labl) = label {
                match mid_list.first().unwrap() {
                    Operation::Instr(instr_in_sec) => mid_list[0] = Operation::LablInstr(labl, instr_in_sec.to_owned()),
                    Operation::Macro(macro_in_sec) => mid_list[0] = Operation::LablMacro(labl, macro_in_sec.to_owned()),
                    _ => unreachable!(),
                }
            }
            instr_list.append(&mut mid_list);
            instr_list.append(&mut right_list);
        },
        MacroInstr::Srr(reg1, reg2, imm) => {
            let mut right_list = instr_list.split_off(*pointer);
            right_list.remove(0);
            let mut mid_list: Vec<Operation> = vec![];

            if *reg2 != Reg::G10 {
                mid_list.push(Instruction::Addi(Reg::G10, reg2.to_owned(), 0).into());
            }
            mid_list.push(Instruction::Addi(Reg::G11, Reg::G0, *imm).into());
            mid_list.push(MacroInstr::Jal(Reg::G1, "_SRR".to_string()).into());
            if *reg1 != Reg::G10 {
                mid_list.push(Instruction::Addi(reg1.to_owned(), Reg::G10, 0).into());
            }

            *accumulator += (mid_list.len() - 1) as i128;
            *pointer += mid_list.len();
            if let Some(labl) = label {
                match mid_list.first().unwrap() {
                    Operation::Instr(instr_in_sec) => mid_list[0] = Operation::LablInstr(labl, instr_in_sec.to_owned()),
                    Operation::Macro(macro_in_sec) => mid_list[0] = Operation::LablMacro(labl, macro_in_sec.to_owned()),
                    _ => unreachable!(),
                }
            }
            instr_list.append(&mut mid_list);
            instr_list.append(&mut right_list);
        },
        MacroInstr::Slr(reg1, reg2, imm) => {
            let mut right_list = instr_list.split_off(*pointer);
            right_list.remove(0);
            let mut mid_list: Vec<Operation> = vec![];

            if *reg2 != Reg::G10 {
                mid_list.push(Instruction::Addi(Reg::G10, reg2.to_owned(), 0).into());
            }
            mid_list.push(Instruction::Addi(Reg::G11, Reg::G0, *imm).into());
            mid_list.push(MacroInstr::Jal(Reg::G1, "_SLR".to_string()).into());
            if *reg1 != Reg::G10 {
                mid_list.push(Instruction::Addi(reg1.to_owned(), Reg::G10, 0).into());
            }

            *accumulator += (mid_list.len() - 1) as i128;
            *pointer += mid_list.len();
            if let Some(labl) = label {
                match mid_list.first().unwrap() {
                    Operation::Instr(instr_in_sec) => mid_list[0] = Operation::LablInstr(labl, instr_in_sec.to_owned()),
                    Operation::Macro(macro_in_sec) => mid_list[0] = Operation::LablMacro(labl, macro_in_sec.to_owned()),
                    _ => unreachable!(),
                }
            }
            instr_list.append(&mut mid_list);
            instr_list.append(&mut right_list);
        },
        MacroInstr::Li(reg, imm) => {
            instr_list.remove(*pointer);
            let mut upper_imm = *imm >> 12;
            let lower_imm = *imm & 0xFFF;

            if lower_imm & 0x800 == 2048 {
                if upper_imm == -1 {
                    // just addi
                    match label {
                        Some(labl) => instr_list.insert(*pointer,
                                        Operation::LablInstr(labl, Instruction::Addi(reg.to_owned(), Reg::G0, lower_imm))),
                        None => instr_list.insert(*pointer, Instruction::Addi(reg.to_owned(), Reg::G0, lower_imm).into()),
                    }
                    *pointer += 1;
                    return;
                } else {
                    let mut mask: i32 = 1;
                    for _ in 0..32 {
                        let is_set = upper_imm & mask != 0;
                        if is_set {
                            upper_imm |= mask;
                            break;
                        }
                        mask <<= 1;
                    }
                }
            }
            
            match label {
                Some(labl) => instr_list.insert(*pointer,
                                Operation::LablInstr(labl, Instruction::Lui(reg.to_owned(), upper_imm))),
                None => instr_list.insert(*pointer, Instruction::Lui(reg.to_owned(), upper_imm).into()),
            }
            *pointer += 1;
            instr_list.insert(*pointer,
            Instruction::Addi(reg.to_owned(), reg.to_owned(), lower_imm).into());
            *pointer += 1;
            *accumulator += 1;
        },
        MacroInstr::LaImm(reg, imm) => {
            instr_list.remove(*pointer);
            match label {
                Some(labl) => instr_list.insert(*pointer,
                                Operation::LablInstr(labl, Instruction::Auipc(reg.to_owned(), imm >> 12))),
                None => instr_list.insert(*pointer, Instruction::Auipc(reg.to_owned(), imm >> 12).into()),
            }
            *pointer += 1;
            instr_list.insert(*pointer,
            Instruction::Addi(reg.to_owned(), reg.to_owned(), *imm).into());
            *pointer += 1;
            *accumulator += 1;
        },
        MacroInstr::LaLabl(reg, targ_labl) => {
            instr_list.remove(*pointer);
            match label {
                Some(labl) => instr_list.insert(*pointer,
                                Operation::LablMacro(labl, MacroInstr::Auipc(reg.to_owned(), targ_labl.to_string(), Part::Upper))),
                None => instr_list.insert(*pointer, MacroInstr::Auipc(reg.to_owned(), targ_labl.to_string(), Part::Upper).into())
            }
            *pointer += 1;
            instr_list.insert(*pointer,
            MacroInstr::Addi(reg.to_owned(), reg.to_owned(), targ_labl.to_string(), Part::Lower).into());
            *pointer += 1;
            *accumulator += 1;
        },
        MacroInstr::CallImm(imm) => {
            instr_list.remove(*pointer);
            match label {
                Some(labl) => instr_list.insert(*pointer,
                                Operation::LablInstr(labl, Instruction::Auipc(Reg::G1, imm >> 12))),
                None => instr_list.insert(*pointer, Operation::Instr(Instruction::Auipc(Reg::G1, imm >> 12)))
            }
            *pointer += 1;
            instr_list.insert(*pointer,
            Instruction::Jalr(Reg::G1, Reg::G1, *imm).into());
            *pointer += 1;
            *accumulator += 1;
        },
        MacroInstr::TailImm(imm) => {
            instr_list.remove(*pointer);
            match label {
                Some(labl) => instr_list.insert(*pointer,
                                Operation::LablInstr(labl, Instruction::Auipc(Reg::G6, imm >> 12))),
                None => instr_list.insert(*pointer, Instruction::Auipc(Reg::G6, imm >> 12).into())
            }
            *pointer += 1;
            instr_list.insert(*pointer,
            Instruction::Jalr(Reg::G0, Reg::G6, *imm).into());
            *pointer += 1;
            *accumulator += 1;
        },
        MacroInstr::CallLabl(targ_labl) => {
            instr_list.remove(*pointer);
            match label {
                Some(labl) => instr_list.insert(*pointer,
                                Operation::LablMacro(labl, MacroInstr::Auipc(Reg::G1, targ_labl.to_string(), Part::Upper))),
                None => instr_list.insert(*pointer, MacroInstr::Auipc(Reg::G1, targ_labl.to_string(), Part::Upper).into())
            }
            *pointer += 1;
            instr_list.insert(*pointer,
            MacroInstr::Jalr(Reg::G1, Reg::G1, targ_labl.to_string(), Part::Lower).into());
            *pointer += 1;
            *accumulator += 1;
        },
        MacroInstr::TailLabl(targ_labl) => {
            instr_list.remove(*pointer);
            match label {
                Some(labl) => instr_list.insert(*pointer,
                                Operation::LablMacro(labl, MacroInstr::Auipc(Reg::G6, targ_labl.to_string(), Part::Upper))),
                None => instr_list.insert(*pointer, MacroInstr::Auipc(Reg::G6, targ_labl.to_string(), Part::Upper).into())
            }
            *pointer += 1;
            instr_list.insert(*pointer,
            MacroInstr::Jalr(Reg::G0, Reg::G6, targ_labl.to_string(), Part::Lower).into());
            *pointer += 1;
            *accumulator += 1;
        },
        MacroInstr::Push(regs) => {
            let mut right_list = instr_list.split_off(*pointer);
            right_list.remove(0);
            let mut mid_list: Vec<Operation> = vec![];

            match label {
                Some(labl) => mid_list.push(Operation::LablInstr(labl, Instruction::Addi(Reg::G2, Reg::G2, -((regs.len() as i32 * 4) + 4)))),
                None => mid_list.push(Instruction::Addi(Reg::G2, Reg::G2, -((regs.len() as i32 * 4) + 4)).into())
            }

            let mut acc: i32 = (regs.len() as i32 * 4) + 4;

            for reg in regs {
                mid_list.push(Instruction::Sw(reg.to_owned(), Reg::G2, acc).into());
                acc -= 4;
            }

            *accumulator += (mid_list.len() - 1) as i128;
            *pointer += mid_list.len();
            instr_list.append(&mut mid_list);
            instr_list.append(&mut right_list);
        },
        MacroInstr::Pop(regs) => {
            let mut right_list = instr_list.split_off(*pointer);
            right_list.remove(0);
            let mut mid_list: Vec<Operation> = vec![];

            let regs_len = regs.len() as i32 * 4;
            let mut acc: i32 = 4;

            match label {
                Some(labl) => mid_list.push(Operation::LablInstr(labl, Instruction::Lw(regs[0].to_owned(), Reg::G2, acc))),
                None => mid_list.push(Instruction::Lw(regs[0].to_owned(), Reg::G2, acc).into())
            }

            for reg in regs {
                if acc == 4 {
                    acc += 4;
                    continue;
                }
                mid_list.push(Instruction::Lw(reg.to_owned(), Reg::G2, acc).into());
                acc += 4;
            }

            mid_list.push(Instruction::Addi(Reg::G2, Reg::G2, regs_len).into());

            *accumulator += (mid_list.len() - 1) as i128;
            *pointer += mid_list.len();
            instr_list.append(&mut mid_list);
            instr_list.append(&mut right_list);
        },

        _ => *pointer += 1,
    }
}

fn expand_instrs(symbol_map: &mut LabelRecog, instr_list: &mut Vec<Operation>) {
    let mut accumulator: i128 = 0;
    let mut pointer = 0;

    loop {
        let operation = instr_list.get(pointer).cloned();
        match operation {
            Some(opera) => {
                match opera {
                    Operation::Instr(_) => pointer += 1,
                    Operation::Macro(macro_in) => translate_macros(&macro_in, instr_list, &mut accumulator, &mut pointer, None),
                    Operation::LablMacro(labl, macro_in) => {
                        if let Some(label) = symbol_map.get_label(&labl.to_string()) {
                            label.add_def(accumulator);
                        };
                        translate_macros(&macro_in, instr_list, &mut accumulator, &mut pointer, Some(labl));
                    },
                    Operation::LablInstr(labl, _) |
                    Operation::Labl(labl) => {
                        if let Some(label) = symbol_map.get_label(&labl.to_string()) {
                            label.add_def(accumulator);
                        }

                        pointer += 1;
                    },
                    Operation::Namespace(_) => unreachable!(),
                }
            },
            None => break,
        };
    }
}

pub fn parse<'a>(input: &'a str, subroutines: &mut Option<&mut Subroutines>) -> IResult<&'a str, (LabelRecog, Vec<Operation<'a>>)> {
    let mut local_ref_not_def: HashSet<String> = HashSet::new();
    let mut symbol_map = LabelRecog::new();
    let mut instr_list: Vec<Operation> = vec![];

    // Key = line forward; value = current line
    let mut abs_to_label_queue: BTreeMap<usize, Vec<usize>> = BTreeMap::new();

    let mut rest = input;
    let mut instr_counter: usize = 0;

    let privileged = subroutines.is_none();

    loop {
        let res = match privileged {
            true => parse_line_priv(rest),
            false => parse_line(rest),
        };

        let mut parsed = match res {
            Ok(line) => {
                rest = line.0;
                line.1
            },
            Err(e) => todo!("Custom parser error! {}", e),
        };

        match &mut parsed {
            (Some(label), Some(instr)) => {
                handle_label_defs(label, &mut symbol_map, &mut local_ref_not_def, instr_counter);

                match instr {
                    Operation::Macro(macro_in) => {
                        handle_label_refs(macro_in, subroutines, &mut symbol_map, &mut local_ref_not_def);
                        *instr = Operation::LablMacro(label.clone(), macro_in.to_owned());
                    },
                    Operation::Instr(instr_in) => {
                        match instr_in {
                            Instruction::Beq(reg1, reg2, imm) => {
                                if let Some(jump_label) = handle_abs_addr_label_conv(instr_counter, &mut abs_to_label_queue, 
                                    &mut instr_list, &mut symbol_map, imm) {
                                    *instr = Operation::LablMacro(label.clone(),
                                    MacroInstr::Beq(reg1.clone(), reg2.clone(), jump_label.to_string()));
                                }
                            },
                            Instruction::Bne(reg1, reg2, imm) => {
                                if let Some(jump_label) = handle_abs_addr_label_conv(instr_counter, 
                                    &mut abs_to_label_queue, &mut instr_list, &mut symbol_map, imm) {
                                    *instr = Operation::LablMacro(label.clone(),
                                    MacroInstr::Bne(reg1.clone(), reg2.clone(), jump_label.to_string()));
                                }
                            },
                            Instruction::Blt(reg1, reg2, imm) => {
                                if let Some(jump_label) = handle_abs_addr_label_conv(instr_counter, &mut abs_to_label_queue, 
                                    &mut instr_list, &mut symbol_map, imm) {
                                    *instr = Operation::LablMacro(label.clone(),
                                    MacroInstr::Blt(reg1.clone(), reg2.clone(), jump_label.to_string()));
                                }
                            },
                            Instruction::Bltu(reg1, reg2, imm) => {
                                if let Some(jump_label) = handle_abs_addr_label_conv(instr_counter, &mut abs_to_label_queue, 
                                    &mut instr_list, &mut symbol_map, imm) {
                                    *instr = Operation::LablMacro(label.clone(),
                                    MacroInstr::Bltu(reg1.clone(), reg2.clone(), jump_label.to_string()));
                                }
                            },
                            Instruction::Bge(reg1, reg2, imm) => {
                                if let Some(jump_label) = handle_abs_addr_label_conv(instr_counter, &mut abs_to_label_queue, 
                                    &mut instr_list, &mut symbol_map, imm) {
                                    *instr = Operation::LablMacro(label.clone(),
                                    MacroInstr::Bge(reg1.clone(), reg2.clone(), jump_label.to_string()));
                                }
                            },
                            Instruction::Bgeu(reg1, reg2, imm) => {
                                if let Some(jump_label) = handle_abs_addr_label_conv(instr_counter, &mut abs_to_label_queue, 
                                    &mut instr_list, &mut symbol_map, imm) {
                                    *instr = Operation::LablMacro(label.clone(),
                                    MacroInstr::Bgeu(reg1.clone(), reg2.clone(), jump_label.to_string()));
                                }
                            },
                            Instruction::Jal(reg, imm) => {
                                if let Some(jump_label) = handle_abs_addr_label_conv(instr_counter, &mut abs_to_label_queue, 
                                    &mut instr_list, &mut symbol_map, imm) {
                                    *instr = Operation::LablMacro(label.clone(),
                                    MacroInstr::Jal(reg.clone(), jump_label.to_string()));
                                }
                            },
                            Instruction::Jalr(reg1, reg2, imm) => {
                                if let Some(jump_label) = handle_abs_addr_label_conv(instr_counter, &mut abs_to_label_queue, 
                                    &mut instr_list, &mut symbol_map, imm) {
                                    *instr = Operation::LablMacro(label.clone(), 
                                    MacroInstr::Jalr(reg1.clone(), reg2.clone(), jump_label.to_string(), Part::None));
                                }
                            },
                            _ => *instr = Operation::LablInstr(label.clone(), instr_in.to_owned()),
                        };
                    }
                    _ => (),
                }

                if let Some(list) = abs_to_label_queue.remove(&(instr_counter + 1)) {
                    symbol_map.set_refd_label(&label.to_string());
                    handle_instr_substitution(&mut instr_list, &list, label);
                };

                instr_counter += 1;
                instr_list.push(instr.to_owned());
            },
            (None, Some(instr)) => {
                match instr {
                    Operation::Macro(macro_in) => handle_label_refs(macro_in, subroutines, &mut symbol_map, &mut local_ref_not_def),
                    Operation::Instr(instr_in) => {
                        match instr_in {
                            Instruction::Beq(reg1, reg2, imm) => {
                                if let Some(jump_label) = handle_abs_addr_label_conv(instr_counter, &mut abs_to_label_queue,
                                     &mut instr_list, &mut symbol_map, imm) {
                                    *instr = Operation::Macro(MacroInstr::Beq(reg1.clone(), reg2.clone(), jump_label.to_string()));
                                }
                            },
                            Instruction::Bne(reg1, reg2, imm) => {
                                if let Some(jump_label) = handle_abs_addr_label_conv(instr_counter, &mut abs_to_label_queue, 
                                    &mut instr_list, &mut symbol_map, imm) {
                                    *instr = Operation::Macro(MacroInstr::Bne(reg1.clone(), reg2.clone(), jump_label.to_string()));
                                }
                            },
                            Instruction::Blt(reg1, reg2, imm) => {
                                if let Some(jump_label) = handle_abs_addr_label_conv(instr_counter, &mut abs_to_label_queue, 
                                    &mut instr_list, &mut symbol_map, imm) {
                                    *instr = Operation::Macro(MacroInstr::Blt(reg1.clone(), reg2.clone(), jump_label.to_string()));
                                }
                            },
                            Instruction::Bltu(reg1, reg2, imm) => {
                                if let Some(jump_label) = handle_abs_addr_label_conv(instr_counter, &mut abs_to_label_queue, 
                                    &mut instr_list, &mut symbol_map, imm) {
                                    *instr = Operation::Macro(MacroInstr::Bltu(reg1.clone(), reg2.clone(), jump_label.to_string()));
                                }
                            },
                            Instruction::Bge(reg1, reg2, imm) => {
                                if let Some(jump_label) = handle_abs_addr_label_conv(instr_counter, &mut abs_to_label_queue, 
                                    &mut instr_list, &mut symbol_map, imm) {
                                    *instr = Operation::Macro(MacroInstr::Bge(reg1.clone(), reg2.clone(), jump_label.to_string()));
                                }
                            },
                            Instruction::Bgeu(reg1, reg2, imm) => {
                                if let Some(jump_label) = handle_abs_addr_label_conv(instr_counter, &mut abs_to_label_queue, 
                                    &mut instr_list, &mut symbol_map, imm) {
                                    *instr = Operation::Macro(MacroInstr::Bgeu(reg1.clone(), reg2.clone(), jump_label.to_string()));
                                }
                            },
                            Instruction::Jal(reg, imm) => {
                                if let Some(jump_label) = handle_abs_addr_label_conv(instr_counter, &mut abs_to_label_queue, 
                                    &mut instr_list, &mut symbol_map, imm) {
                                    *instr = Operation::Macro(MacroInstr::Jal(reg.clone(), jump_label.to_string())); 
                                }
                            },
                            Instruction::Jalr(reg1, reg2, imm) => {
                                if let Some(jump_label) = handle_abs_addr_label_conv(instr_counter, &mut abs_to_label_queue, 
                                    &mut instr_list, &mut symbol_map, imm) {
                                    *instr = Operation::Macro(MacroInstr::Jalr(reg1.clone(), reg2.clone(), jump_label.to_string(), Part::None));   
                                }
                            },
                            _ => (),
                        };
                    }
                    _ => (),
                }
                
                if let Some(list) = abs_to_label_queue.remove(&(instr_counter + 1)) {
                    let jump_label = Cow::from("__".to_string() + &instr_counter.to_string());
                    symbol_map.crt_def_ref(&jump_label.to_string(), false, instr_counter as i128);
                    handle_instr_substitution(&mut instr_list, &list, &jump_label);
                    match &instr {
                        Operation::Instr(instr_in) => *instr = Operation::LablInstr(jump_label, instr_in.to_owned()),
                        Operation::Macro(macro_in) => *instr = Operation::LablMacro(jump_label, macro_in.to_owned()),
                        _ => unreachable!()
                    }
                };

                instr_counter += 1;
                instr_list.push(instr.to_owned());
            },
            (Some(label), None) => {
                handle_label_defs(label, &mut symbol_map, &mut local_ref_not_def, instr_counter);
                if let Some(list) = abs_to_label_queue.remove(&(instr_counter + 1)) {
                    symbol_map.set_refd_label(&label.to_string());
                    handle_instr_substitution(&mut instr_list, &list, label)
                };
                instr_counter += 1;
                instr_list.push(Operation::Labl(label.clone()));
            },
            (None, None) => (),
        }

        if rest.trim().is_empty() {
            break;
        }
    }

    if !abs_to_label_queue.is_empty() {
        let jump_label = match &instr_list[instr_counter - 1] {
            Operation::Labl(labl) => {
                symbol_map.set_refd_label(&labl.to_string());
                labl.clone()
            },
            _ => {
                let res = Cow::from("__".to_string() + &instr_counter.to_string());
                symbol_map.crt_def_ref(&res.to_string(), false, instr_counter as i128);
                res
            }
        };
        for (_, elem) in abs_to_label_queue.iter() {
            handle_instr_substitution(&mut instr_list, elem, &jump_label);
        }
        instr_list.push(Operation::Labl(jump_label));
    }

    expand_instrs(&mut symbol_map, &mut instr_list);

    // NO! TODO: If labels are still in the hashset, return a custom parser error!

    Ok(("", (symbol_map, instr_list)))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_label() {
        assert_ne!(parse_label_definition("invalid"), Ok(("", Cow::from("invalid"))));
        assert_eq!(parse_label_definition("valid0:"), Ok(("", Cow::from("valid0"))));
        assert_ne!(parse_label_definition("invalid :"), Ok(("", Cow::from("invalid"))));
        assert_ne!(parse_label_definition(" "), Ok(("", Cow::from(""))));
        assert_eq!(parse_label_definition("valid:"), Ok(("", Cow::from("valid"))));
        assert_ne!(parse_label_definition("0invalid:"), Ok(("", Cow::from("0invalid"))));
        assert_eq!(parse_label_definition("v415alid:"), Ok(("", Cow::from("v415alid"))));
        assert_eq!(parse_label_definition(".veryvalid:"), Ok(("", Cow::from(".veryvalid"))));
    }

    #[test]
    fn test_parse_label_privileged() {
        assert_ne!(parse_label_definition_priv("invalid"), Ok(("", Cow::from("invalid"))));
        assert_ne!(parse_label_definition_priv("invalid0:"), Ok(("", Cow::from("invalid0"))));
        assert_ne!(parse_label_definition_priv("invalid :"), Ok(("", Cow::from("invalid"))));
        assert_ne!(parse_label_definition_priv(" "), Ok(("", Cow::from(""))));
        assert_eq!(parse_label_definition_priv("_valid:"), Ok(("", Cow::from("_valid"))));
        assert_ne!(parse_label_definition_priv("0invalid:"), Ok(("", Cow::from("0invalid"))));
        assert_eq!(parse_label_definition_priv("_v415alid:"), Ok(("", Cow::from("_v415alid"))));
        assert_eq!(parse_label_definition_priv("_veryvalid:"), Ok(("", Cow::from("_veryvalid"))));
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

    #[test]
    fn test_parse_seper() {
        assert_ne!(parse_seper("invalid"), Ok(("", "")));
        assert_ne!(parse_seper(" "), Ok(("", "")));
        assert_eq!(parse_seper(", "), Ok(("", ",")));
        assert_eq!(parse_seper(","), Ok(("", ",")));
    }
    
    #[test]
    fn test_parse_instrnoparam() {
        assert_ne!(parse_macro_noparm("invalid"), Ok(("", Operation::Instr(Instruction::NA))));
        assert_ne!(parse_macro_noparm("noop"), Ok(("", Operation::Instr(Instruction::Addi(Reg::G0, Reg::G0, 0)))));
        assert_eq!(parse_macro_noparm("nop"), Ok(("", Operation::Instr(Instruction::Addi(Reg::G0, Reg::G0, 0)))));
        assert_ne!(parse_macro_noparm("nop x1"), Ok(("", Operation::Instr(Instruction::Addi(Reg::G0, Reg::G0, 0)))));
        assert_eq!(parse_macro_noparm("ret"), Ok(("", Operation::Instr(Instruction::Jalr(Reg::G0, Reg::G1, 0)))));
        assert_ne!(parse_macro_noparm("ret nop"), Ok(("", Operation::Instr(Instruction::Jalr(Reg::G0, Reg::G1, 0)))));
    }

    #[test]
    fn test_parse_instr1labl() {
        assert_ne!(parse_macro_1labl("invalid"), Ok(("", Instruction::NA.into())));
        assert_ne!(parse_macro_1labl(" "), Ok(("", Instruction::NA.into())));
        assert_ne!(parse_macro_1labl(""), Ok(("", Instruction::NA.into())));
        assert_ne!(parse_macro_1labl("call"), Ok(("", Instruction::NA.into())));
        assert_eq!(parse_macro_1labl("tail test"), Ok(("", MacroInstr::TailLabl("test".to_string()).into())));
        assert_eq!(parse_macro_1labl("call HANS"), Ok(("", MacroInstr::CallLabl("HANS".to_string()).into())));
        assert_ne!(parse_macro_1labl("call label  "), Ok(("", MacroInstr::CallLabl("label".to_string()).into())));
    }

    #[test]
    fn test_parse_instr1imm() {
        assert_ne!(parse_macro_1imm("invalid"), Ok(("", Instruction::NA.into())));
        assert_ne!(parse_macro_1imm(" "), Ok(("", Instruction::NA.into())));
        assert_ne!(parse_macro_1imm(""), Ok(("", Instruction::NA.into())));
        assert_ne!(parse_macro_1imm("j"), Ok(("", Instruction::NA.into())));
        assert_eq!(parse_macro_1imm("j 12"), Ok(("", Instruction::Jal(Reg::G0, 12).into())));
        assert_eq!(parse_macro_1imm("call 0x10"), Ok(("", MacroInstr::CallImm(0x10).into())));
        assert_ne!(parse_macro_1imm("jal 125  "), Ok(("", Instruction::Jal(Reg::G1, 125).into())));
    }

    #[test]
    fn test_parse_instr1reg() {
        assert_ne!(parse_macro_1reg("invalid"), Ok(("", Instruction::NA.into())));
        assert_ne!(parse_macro_1reg(" "), Ok(("", Instruction::NA.into())));
        assert_ne!(parse_macro_1reg(""), Ok(("", Instruction::NA.into())));
        assert_ne!(parse_macro_1reg("jr"), Ok(("", Instruction::Jalr(Reg::NA, Reg::NA, 0).into())));
        assert_eq!(parse_macro_1reg("jalr a2"), Ok(("", Instruction::Jalr(Reg::G1, Reg::str_to_enum("a2").unwrap(), 0).into())));
        assert_eq!(parse_macro_1reg("jr x18"), Ok(("", Instruction::Jalr(Reg::G0, Reg::G18, 0).into())));
        assert_ne!(parse_macro_1reg("jalr x19  "), Ok(("", Instruction::Jalr(Reg::G1, Reg::G19, 0).into())));
    }

    #[test]
    fn test_parse_instr1labl1reg() {
        assert_ne!(parse_macro_1labl1reg(""), Ok(("", Instruction::NA.into())));
        assert_ne!(parse_macro_1labl1reg("lui"), Ok(("", MacroInstr::Lui(Reg::NA, "".to_string()).into())));
        assert_eq!(parse_macro_1labl1reg("lui a2, stop"), Ok(("", MacroInstr::Lui(Reg::G12, "stop".to_string()).into())));
        assert_eq!(parse_macro_1labl1reg("auipc s2, helloWorld"), Ok(("", MacroInstr::Auipc(Reg::G18, "helloWorld".to_string(), Part::None).into())));
        assert_eq!(parse_macro_1labl1reg("jal   x20, test"), Ok(("", MacroInstr::Jal(Reg::G20, "test".to_string()).into())));
        assert_ne!(parse_macro_1labl1reg("jal x19, train "), Ok(("", MacroInstr::Jal(Reg::G19, "train".to_string()).into())));
        assert_eq!(parse_macro_1labl1reg("la x19, HELLOWORLD"), Ok(("", MacroInstr::LaLabl(Reg::G19, "HELLOWORLD".to_string()).into())));
    }

    #[test]
    fn test_parse_instr1imm1reg() {
        assert_ne!(parse_inst_1imm1reg("invalid"), Ok(("", Instruction::NA.into())));
        assert_ne!(parse_inst_1imm1reg(" "), Ok(("",Instruction::NA.into())));
        assert_ne!(parse_inst_1imm1reg(""), Ok(("", Instruction::NA.into())));
        assert_ne!(parse_inst_1imm1reg("ld"), Ok(("", Instruction::NA.into())));
        assert_ne!(parse_inst_1imm1reg(""), Ok(("", Instruction::NA.into())));
        assert_ne!(parse_inst_1imm1reg("lui"), Ok(("", Instruction::Lui(Reg::NA, 0).into())));
        assert_eq!(parse_inst_1imm1reg("lui x12, 12"), Ok(("", Instruction::Lui(Reg::G12, 12).into())));
        assert_eq!(parse_inst_1imm1reg("auipc x18, 0x20"), Ok(("", Instruction::Auipc(Reg::G18, 32).into())));
        assert_eq!(parse_inst_1imm1reg("jal x20, 5"), Ok(("", Instruction::Jal(Reg::G20, 5).into())));
        assert_ne!(parse_inst_1imm1reg("jal x19, 125 "), Ok(("", Instruction::Jal(Reg::G19, 125).into())));
        assert_eq!(parse_inst_1imm1reg("la x19, 0x0F"), Ok(("", MacroInstr::LaImm(Reg::G19, 0x0F).into())));
    }

    #[test]
    fn test_parse_inst_2reg() {
        assert_ne!(parse_macro_2reg("invalid"), Ok(("", Instruction::NA.into())));
        assert_ne!(parse_macro_2reg("   "), Ok(("", Instruction::NA.into())));
        assert_ne!(parse_macro_2reg("ld x1, 0xAA"), Ok(("", Instruction::NA.into())));
        assert_ne!(parse_macro_2reg("mv x1, 0xAA"), Ok(("", Instruction::NA.into())));
        assert_eq!(parse_macro_2reg("mv x1, x4"), Ok(("", Instruction::Addi(Reg::G1, Reg::G4, 0).into())));
        assert_eq!(parse_macro_2reg("mv x12,x4"), Ok(("", Instruction::Addi(Reg::G12, Reg::G4, 0).into())));
    }

    #[test]
    fn test_parse_instr1labl2reg() {
        assert_ne!(parse_macro_1labl2reg("invalid"), Ok(("", Instruction::NA.into())));
        assert_ne!(parse_macro_1labl2reg("   "), Ok(("", Instruction::NA.into())));
        assert_ne!(parse_macro_1labl2reg("sb x1, x6"), Ok(("", MacroInstr::Sb(Reg::G1, Reg::G6, "".to_string(), Part::None).into())));
        assert_ne!(parse_macro_1labl2reg("lb x1, total"), Ok(("", MacroInstr::Lb(Reg::G1, Reg::NA, "total".to_string(), Part::None).into())));
        assert_eq!(parse_macro_1labl2reg("bgeu  x1, x4, sTaRt"), Ok(("", MacroInstr::Bgeu(Reg::G1, Reg::G4, "sTaRt".to_string()).into())));
        assert_ne!(parse_macro_1labl2reg("slli x1x4,eNND"), Ok(("", MacroInstr::Slli(Reg::G1, Reg::G4, "eNND".to_string()).into())));
        assert_eq!(parse_macro_1labl2reg("blt x10,x10, last"), Ok(("", MacroInstr::Blt(Reg::G10, Reg::G10, "last".to_string()).into())));
        assert_ne!(parse_macro_1labl2reg("jalr  x6,  x8,test"), Ok(("", MacroInstr::Jalr(Reg::G6, Reg::G8, "test".to_string(), Part::None).into())));
        assert_eq!(parse_macro_1labl2reg("lhu x1, x2, hans"), Ok(("", MacroInstr::Lhu(Reg::G1, Reg::G2, "hans".to_string()).into())));
        assert_eq!(parse_macro_1labl2reg("sb x13,x15,loading"), Ok(("", MacroInstr::Sb(Reg::G13, Reg::G15, "loading".to_string(), Part::None).into())));
        assert_ne!(parse_macro_1labl2reg("beqx1x15,start"), Ok(("", MacroInstr::Beq(Reg::G1, Reg::G15, "start".to_string()).into())));
        assert_ne!(parse_macro_1labl2reg("lbu x12, x15,  dasletzte"), Ok(("", MacroInstr::Lbu(Reg::G12, Reg::G15, "dasletzte".to_string()).into())));
    }

    #[test]
    fn test_parse_inst_1imm2reg() {
        assert_ne!(parse_inst_1imm2reg_lw("invalid"), Ok(("", Instruction::NA.into())));
        assert_ne!(parse_inst_1imm2reg_lw("   "), Ok(("", Instruction::NA.into())));
        assert_ne!(parse_inst_1imm2reg_lw("addi x1, x6"), Ok(("", Instruction::NA.into())));
        assert_ne!(parse_inst_1imm2reg_lw("lbu x1, 0xAA"), Ok(("", Instruction::Lbu(Reg::G1, Reg::NA, 0xAA).into())));
        assert_eq!(parse_inst_1imm2reg_lw("blt x1, x4, 5"), Ok(("", Instruction::Blt(Reg::G1, Reg::G4, 5).into())));
        assert_ne!(parse_inst_1imm2reg_lw("lb x1x4,0x6"), Ok(("", Instruction::Lb(Reg::G1, Reg::G4, 6).into())));
        assert_eq!(parse_inst_1imm2reg_lw("sb x10,x10, 51"), Ok(("", Instruction::Sb(Reg::G10, Reg::G10, 51).into())));
        assert_ne!(parse_inst_1imm2reg_lw("bge x6,  x8,5"), Ok(("", Instruction::Bge(Reg::G6, Reg::G8, 5).into())));

        assert_eq!(parse_inst_1imm2reg_up("addi x1, x2, 0xAA"), Ok(("", Instruction::Addi(Reg::G1, Reg::G2, 0xAA).into())));
        assert_eq!(parse_inst_1imm2reg_up("srr   x13,x15,6"), Ok(("", MacroInstr::Srr(Reg::G13, Reg::G15, 6).into())));
        assert_ne!(parse_inst_1imm2reg_up("sltix1x15,6"), Ok(("", Instruction::Slti(Reg::G1, Reg::G15, 6).into())));
        assert_ne!(parse_inst_1imm2reg_up("slli x12, x15,  6"), Ok(("", Instruction::Slli(Reg::G12, Reg::G15, 6).into())));
        // TODO: More tests
    }

    #[test]
    fn test_parse_inst_3reg() {
        assert_ne!(parse_inst_3reg("invalid"), Ok(("", Instruction::NA.into())));
        assert_ne!(parse_inst_3reg("   "), Ok(("", Instruction::NA.into())));
        assert_ne!(parse_inst_3reg("addi x1, x6, 0xAA"), Ok(("", Instruction::Addi(Reg::G1, Reg::G6, 0xAA).into())));
        assert_ne!(parse_inst_3reg("add x1, x2"), Ok(("", Instruction::Addn(Reg::G1, Reg::G2, Reg::NA).into())));
        assert_eq!(parse_inst_3reg("mul x1, x4, x6"), Ok(("", MacroInstr::Muln(Reg::G1, Reg::G4, Reg::G6).into())));
        assert_ne!(parse_inst_3reg("div x10x14,x7"), Ok(("", MacroInstr::Divn(Reg::G10, Reg::G14, Reg::G7).into())));
        
        assert_eq!(parse_inst_3reg("xor x10,x11, x10"), Ok(("", Instruction::Xor(Reg::G10, Reg::G11, Reg::G10).into())));
        assert_ne!(parse_inst_3reg("xnor x6,  x8,x5"), Ok(("", Instruction::NA.into())));
        assert_eq!(parse_inst_3reg("and x6, x8, x14"), Ok(("", Instruction::And(Reg::G6, Reg::G8, Reg::G14).into())));
        assert_ne!(parse_inst_3reg("sll x6,  x8, x14"), Ok(("", Instruction::Sll(Reg::G6, Reg::G8, Reg::G14).into())));
    }

    #[test]
    fn test_parse_inst_multiarg() {
        assert_ne!(parse_macro_multiarg("push"), Ok(("", MacroInstr::Push(vec![]).into())));
        assert_eq!(parse_macro_multiarg("push x12"), Ok(("", MacroInstr::Push(Vec::from([
            Reg::G12
        ])).into())));
        assert_eq!(parse_macro_multiarg("push x12, x13, x14"), Ok(("", MacroInstr::Push(Vec::from([
            Reg::G12,
            Reg::G13,
            Reg::G14
        ])).into())));
        assert_eq!(parse_macro_multiarg("push   x12,x13,x14"), Ok(("", MacroInstr::Push(Vec::from([
            Reg::G12,
            Reg::G13,
            Reg::G14
        ])).into())));
        assert_ne!(parse_macro_multiarg("pop"), Ok(("", MacroInstr::Pop(vec![]).into())));
        assert_eq!(parse_macro_multiarg("pop x12"), Ok(("", MacroInstr::Pop(Vec::from([
            Reg::G12
        ])).into())));
        assert_eq!(parse_macro_multiarg("pop x12, x13, x14"), Ok(("", MacroInstr::Pop(Vec::from([
            Reg::G12,
            Reg::G13,
            Reg::G14
        ])).into())));
        assert_eq!(parse_macro_multiarg("pop   x12,x13,x14"), Ok(("", MacroInstr::Pop(Vec::from([
            Reg::G12,
            Reg::G13,
            Reg::G14
        ])).into())));
    }

    #[test]
    fn test_parse_instruction() {
        assert_eq!(parse_instruction("mv x1, x6"), Ok(("", Instruction::Addi(Reg::G1, Reg::G6, 0).into())));
        assert_ne!(parse_instruction("addi x1, 0xAA"), Ok(("", Instruction::Addi(Reg::G1, Reg::NA, 0xAA).into())));
        assert_eq!(parse_instruction("mul x1, x4, x6"), Ok(("", MacroInstr::Muln(Reg::G1, Reg::G4, Reg::G6).into())));
        assert_ne!(parse_instruction("xor x10x14,x7"), Ok(("", Instruction::Xor(Reg::G10, Reg::G14, Reg::G7).into())));
        assert_eq!(parse_instruction("add x10,x11, x10"), Ok(("", Instruction::Addn(Reg::G10, Reg::G11, Reg::G10).into())));
        assert_ne!(parse_instruction("xnor x6,  x8,x5"), Ok(("", Instruction::NA.into())));
        assert_eq!(parse_instruction("srr x5, x8, 7"), Ok(("", MacroInstr::Srr(Reg::G5, Reg::G8, 7).into())));
        // More tests & seperate
    }

    #[test]
    fn test_parse_line() {
        assert_eq!(parse_line("label: add x1, x5, x6"),
                   Ok(("", (Some(Cow::from("label")), Some(Instruction::Addn(Reg::G1, Reg::G5, Reg::G6).into())))));
        assert_eq!(parse_line("\ntest:\n\nsub x6, x5, x11"),
                   Ok(("", (Some(Cow::from("test")), Some(Instruction::Subn(Reg::G6, Reg::G5, Reg::G11).into())))));
        assert_eq!(parse_line("\n\n\nreturn:\n"),
                   Ok(("", (Some(Cow::from("return")), None))));
        assert_eq!(parse_line("mv x15, x12\naddi x12, x10, 0x05"),
                   Ok(("\naddi x12, x10, 0x05", (None, Some(Instruction::Addi(Reg::G15, Reg::G12, 0).into())))));
        assert_eq!(parse_line("label:\ndiv x14, x13, x10"),
                   Ok(("", (Some(Cow::from("label")), Some(MacroInstr::Divn(Reg::G14, Reg::G13, Reg::G10).into())))));
    }

    #[test]
    fn test_parse_line_privileged() {
        assert_eq!(parse_line_priv("_label: add x1, x5, x6"),
                    Ok(("", (Some(Cow::from("_label")), Some(Instruction::Addn(Reg::G1, Reg::G5, Reg::G6).into())))));
        assert_eq!(parse_line_priv("\n_test:\n\nsub x6, x5, x11"),
                    Ok(("", (Some(Cow::from("_test")), Some(Instruction::Subn(Reg::G6, Reg::G5, Reg::G11).into())))));
        assert_eq!(parse_line_priv("\n\n\n_return:\n"),
                    Ok(("", (Some(Cow::from("_return")), None))));
        assert_eq!(parse_line_priv("mv x15, x12\naddi x12, x10, 0x05"),
                    Ok(("\naddi x12, x10, 0x05", (None, Some(Instruction::Addi(Reg::G15, Reg::G12, 0).into())))));
        assert_eq!(parse_line_priv("_label:\ndiv x14, x13, x10"),
                    Ok(("", (Some(Cow::from("_label")), Some(MacroInstr::Divn(Reg::G14, Reg::G13, Reg::G10).into())))));
    }

    #[test]
    fn test_parse() {
        let source_code = r#"START:
    li x4, 16
    mv x3, x4
MUL: beq x3, x4, END
    mul x6, x4, x3
    lui x4, 0x16
    j MUL
END:
"#;

        let mut subroutines = Subroutines::new();

        let mut symbols = LabelRecog::new();
        let mut label = LabelElem::new();
        label.set_name("START".to_string());
        label.set_scope(true);
        label.set_def(0);
        let _ = symbols.insert_label(label);

        label = LabelElem::new_refd("MUL".to_string());
        label.set_scope(true);
        label.set_def(3);
        let _ = symbols.insert_label(label);

        label = LabelElem::new_refd("END".to_string());
        label.set_scope(true);
        label.set_def(10);
        let _ = symbols.insert_label(label);

        label = LabelElem::new_refd("_MUL".to_string());
        label.set_scope(true);
        let _ = symbols.insert_label(label);
        
        let correct_vec: Vec<Operation> = vec![
                                                 Operation::LablInstr(Cow::from("START"), Instruction::Lui(Reg::G4, 0)),
                                                 Operation::from(Instruction::Addi(Reg::G4, Reg::G4, 16)),
                                                 Operation::from(Instruction::Addi(Reg::G3, Reg::G4, 0)),
                                                 Operation::LablMacro(Cow::from("MUL"), MacroInstr::Beq(Reg::G3, Reg::G4, "END".to_string())),
                                                 Operation::from(Instruction::Addi(Reg::G10, Reg::G4, 0)),
                                                 Operation::from(Instruction::Addi(Reg::G11, Reg::G3, 0)),
                                                 Operation::from(MacroInstr::Jal(Reg::G1, "_MUL".to_string())),
                                                 Operation::from(Instruction::Addi(Reg::G6, Reg::G10, 0)),
                                                 Operation::from(Instruction::Lui(Reg::G4, 0x16)),
                                                 Operation::from(MacroInstr::Jal(Reg::G0, "MUL".to_string())),
                                                 Operation::Labl(Cow::from("END"))
                                                ];

        assert_eq!(parse(source_code, &mut Some(&mut subroutines)),
                   Ok(("", (symbols, correct_vec))));
        assert_eq!(subroutines.get_code(), Vec::from([Subroutines::MUL_SUB]));
        // TODO: Probably more test cases!
    }

    #[test]
    fn test_parse_abs_addresses_smallex() {
        let source_code = 
r#" li  x4, 16
    mv  x3, x4
    beq x3, x4, 16
    mul x6, x4, x3
    beq x3, x4, 16
    lui x4, 0x16
    j   -12
"#;
        let mut subroutines = Subroutines::new();

        let mut symbols = LabelRecog::new();

        let mut label = LabelElem::new_refd("_MUL".to_string());
        label.set_scope(true);
        let _ = symbols.insert_label(label);

        label = LabelElem::new_refd("__3".to_string());
        label.set_scope(false);
        label.set_def(4);
        let _ = symbols.insert_label(label);

        label = LabelElem::new_refd("__6".to_string());
        label.set_scope(false);
        label.set_def(10);
        let _ = symbols.insert_label(label);

        label = LabelElem::new_refd("__7".to_string());
        label.set_scope(false);
        label.set_def(11);
        let _ = symbols.insert_label(label);
        
        let correct_vec: Vec<Operation> = vec![
                                                 Operation::Instr(Instruction::Lui(Reg::G4, 0)),
                                                 Operation::from(Instruction::Addi(Reg::G4, Reg::G4, 16)),
                                                 Operation::from(Instruction::Addi(Reg::G3, Reg::G4, 0)),
                                                 Operation::Macro(MacroInstr::Beq(Reg::G3, Reg::G4, "__6".to_string())),
                                                 Operation::LablInstr(Cow::from("__3"), Instruction::Addi(Reg::G10, Reg::G4, 0)),
                                                 Operation::from(Instruction::Addi(Reg::G11, Reg::G3, 0)),
                                                 Operation::from(MacroInstr::Jal(Reg::G1, "_MUL".to_string())),
                                                 Operation::from(Instruction::Addi(Reg::G6, Reg::G10, 0)),
                                                 Operation::Macro(MacroInstr::Beq(Reg::G3, Reg::G4, "__7".to_string())),
                                                 Operation::from(Instruction::Lui(Reg::G4, 0x16)),
                                                 Operation::LablMacro(Cow::from("__6"), MacroInstr::Jal(Reg::G0, "__3".to_string())),
                                                 Operation::Labl(Cow::from("__7"))
                                                ];

        assert_eq!(parse(source_code, &mut Some(&mut subroutines)),
                   Ok(("", (symbols, correct_vec))));
        assert_eq!(subroutines.get_code(), Vec::from([Subroutines::MUL_SUB]));
    }

    #[test]
    fn test_parse_abs_addresses_biggerex() {
        let source_code = 
r#"
    addi a7, zero, 1
    mv a2, a0
    mv a3, a1
    mv a0, zero
    mv a1, zero
    bne a2, a3, 16
    slli a3, a3, 1
    sub a2, a2, a3
    add a0, a0, a7
    blt a2, a3, 40
    slli a3, a3, 1
    slli a7, a7, 1
    blt a3, a2, -8
    srli a3, a3, 1
    srli a7, a7, 1
    sub a2, a2, a3
    add a0, a0, a7
    bne a2, zero, -32
    beq zero, zero, 44
    srli a3, a3, 1
    srli a7, a7, 1
    blt a2, a3, -8
    slli a3, a3, 1
    slli a7, a7, 1
    beq a2, a3, 8
    srli a3, a3, 1
TEST: srli a7, a7, 1
    sub a2, a2, a3
    add a0, a0, a7
    bne a2, zero, -80
    ret
"#;
        let mut subroutines = Subroutines::new();

        let mut symbols = LabelRecog::new();

        let mut label = LabelElem::new_refd("__9".to_string());
        label.set_scope(false);
        label.set_def(9);
        let _ = symbols.insert_label(label);

        label = LabelElem::new_refd("__10".to_string());
        label.set_scope(false);
        label.set_def(10);
        let _ = symbols.insert_label(label);

        label = LabelElem::new_refd("__19".to_string());
        label.set_scope(false);
        label.set_def(19);
        let _ = symbols.insert_label(label);


        label = LabelElem::new_refd("TEST".to_string());
        label.set_scope(true);
        label.set_def(26);
        let _ = symbols.insert_label(label);

        label = LabelElem::new_refd("__29".to_string());
        label.set_scope(false);
        label.set_def(29);
        let _ = symbols.insert_label(label);

        let mut correct_vec: Vec<Operation> = vec![];
        correct_vec.push(Operation::Instr(Instruction::Addi(Reg::G17, Reg::G0, 1)));
        correct_vec.push(Operation::Instr(Instruction::Addi(Reg::G12, Reg::G10, 0)));
        correct_vec.push(Operation::Instr(Instruction::Addi(Reg::G13, Reg::G11, 0)));
        correct_vec.push(Operation::Instr(Instruction::Addi(Reg::G10, Reg::G0, 0)));
        correct_vec.push(Operation::Instr(Instruction::Addi(Reg::G11, Reg::G0, 0)));

        correct_vec.push(Operation::Macro(MacroInstr::Bne(Reg::G12, Reg::G13, "__9".to_string())));
        correct_vec.push(Operation::Instr(Instruction::Slli(Reg::G13, Reg::G13, 1)));
        correct_vec.push(Operation::Instr(Instruction::Subn(Reg::G12, Reg::G12, Reg::G13)));
        correct_vec.push(Operation::Instr(Instruction::Addn(Reg::G10, Reg::G10, Reg::G17)));
        correct_vec.push(Operation::LablMacro(Cow::from("__9"), MacroInstr::Blt(Reg::G12, Reg::G13, "__19".to_string())));

        correct_vec.push(Operation::LablInstr(Cow::from("__10"), Instruction::Slli(Reg::G13, Reg::G13, 1)));
        correct_vec.push(Operation::Instr(Instruction::Slli(Reg::G17, Reg::G17, 1)));
        correct_vec.push(Operation::Macro(MacroInstr::Blt(Reg::G13, Reg::G12, "__10".to_string())));
        correct_vec.push(Operation::Instr(Instruction::Srli(Reg::G13, Reg::G13, 1)));
        correct_vec.push(Operation::Instr(Instruction::Srli(Reg::G17, Reg::G17, 1)));
        correct_vec.push(Operation::Instr(Instruction::Subn(Reg::G12, Reg::G12, Reg::G13)));
        correct_vec.push(Operation::Instr(Instruction::Addn(Reg::G10, Reg::G10, Reg::G17)));
        correct_vec.push(Operation::Macro(MacroInstr::Bne(Reg::G12, Reg::G0, "__9".to_string())));
        correct_vec.push(Operation::Macro(MacroInstr::Beq(Reg::G0, Reg::G0, "__29".to_string())));

        correct_vec.push(Operation::LablInstr(Cow::from("__19"), Instruction::Srli(Reg::G13, Reg::G13, 1)));
        correct_vec.push(Operation::Instr(Instruction::Srli(Reg::G17, Reg::G17, 1)));
        correct_vec.push(Operation::Macro(MacroInstr::Blt(Reg::G12, Reg::G13, "__19".to_string())));
        correct_vec.push(Operation::Instr(Instruction::Slli(Reg::G13, Reg::G13, 1)));
        correct_vec.push(Operation::Instr(Instruction::Slli(Reg::G17, Reg::G17, 1)));
        correct_vec.push(Operation::Macro(MacroInstr::Beq(Reg::G12, Reg::G13, "TEST".to_string())));
        correct_vec.push(Operation::Instr(Instruction::Srli(Reg::G13, Reg::G13, 1)));
        correct_vec.push(Operation::LablInstr(Cow::from("TEST"), Instruction::Srli(Reg::G17, Reg::G17, 1)));
        correct_vec.push(Operation::Instr(Instruction::Subn(Reg::G12, Reg::G12, Reg::G13)));
        correct_vec.push(Operation::Instr(Instruction::Addn(Reg::G10, Reg::G10, Reg::G17)));
        correct_vec.push(Operation::LablMacro(Cow::from("__29"), MacroInstr::Bne(Reg::G12, Reg::G0, "__9".to_string())));
        correct_vec.push(Operation::Instr(Instruction::Jalr(Reg::G0, Reg::G1, 0)));

        assert_eq!(parse(source_code, &mut Some(&mut subroutines)),
                   Ok(("", (symbols, correct_vec))));
        assert!(subroutines.get_code().is_empty());
    }
}

// Lokale und globale Labels
// Lokale labels: .L1:
// Globale labels: L2:
// Done

// Pseudo-Opcodes:
// "call [LABEL]", "jsr []", "pop [REG]" - "ld [REG], [IMM]", "push [REG]" - "st [REG], [IMM]"

// Konstanten:
// .data
// [LABEL] .[ASSEMBLER INSTRUCTION] [IMM]
// .text
// CODE
