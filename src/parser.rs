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
        tag,
        tag_no_case
    },
    branch::alt,
    combinator::{
        opt,
        value,
        map_res
    },
    character::complete::{
        alpha1,
        digit1,
        hex_digit1,
        multispace0,
        multispace1, 
        alphanumeric0,
        alphanumeric1
    },
    sequence::{
        tuple,
        separated_pair,
    },
};
use std::collections::HashSet;
use std::borrow::Cow;

use crate::common::*;

pub struct Subroutines {
    code_str_vec: HashSet<String>
}

impl Subroutines {
    // TODO:
    const MUL_SUB: &'static str = r#"
_MUL:
"#;
    const DIV_SUB: &'static str = r#"
_DIV:
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

    pub fn mul_defined(&mut self) -> () {
        self.code_str_vec.insert(Self::MUL_SUB.to_string());
    }

    pub fn div_defined(&mut self) -> () {
        self.code_str_vec.insert(Self::DIV_SUB.to_string());
    }

    pub fn srr_defined(&mut self) -> () {
        self.code_str_vec.insert(Self::SRR_SUB.to_string());
    }

    pub fn slr_defined(&mut self) -> () {
        self.code_str_vec.insert(Self::SLR_SUB.to_string());
    }

    pub fn get_code(&self) -> Vec<String> {
        self.code_str_vec.to_owned().into_iter().collect()
    }
}

#[derive(Debug, Clone)]
pub struct LabelInsertError {
    label: String,
}

impl LabelInsertError {
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
    Imm::from_str_radix(input, 16)
}

fn parse_imm(input: &str) -> IResult<&str, Imm> {
    let (rest, parsed) = opt(tag_no_case("0x"))(input)?;
    if parsed.is_none() {
        // Decimal
        let (rest, parsed) = opt(tag("-"))(rest)?;
        if parsed.is_none() {
            // Positive decimal
            map_res(digit1, str::parse)(rest)
        } else {
            // Negative decimal
            map_res(digit1, |s: &str| {
                let mut string_build = parsed.unwrap_or("").to_string();
                string_build.push_str(s);
                string_build.parse::<Imm>()
            })(rest)
        }
    } else {
        // Hex
        map_res(hex_digit1, from_hex)(rest)
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

fn parse_inst_noparm(input: &str) -> IResult<&str, Operation> {
    let (rest, instr) = alt((
        value(Operation::Macro(MacroInstr::NOP), tag("nop")),
        value(Operation::Macro(MacroInstr::Ret), tag("ret")),
    ))(input)?;

    Ok((rest, instr))
}

fn parse_inst_1labl(input: &str) -> IResult<&str, Operation> {
    let (rest, macro_in) = alt((
        value(MacroInstr::Calll("".to_string()), tag("call")),
        value(MacroInstr::Taill("".to_string()), tag("tail")),

        value(MacroInstr::Jall("".to_string()), tag("jal")),
        value(MacroInstr::Jl("".to_string()), tag("j")),
    ))(input)?;
    let (rest, _) = tag(" ")(rest)?;
    let (rest, labl) = parse_label_name(rest)?;

    let instr = match macro_in {
        MacroInstr::Calll(_) => MacroInstr::Calll(labl.to_string()),
        MacroInstr::Taill(_) => MacroInstr::Taill(labl.to_string()),
        MacroInstr::Jl(_) => MacroInstr::Jl(labl.to_string()),
        MacroInstr::Jall(_) => MacroInstr::Jall(labl.to_string()),
        _ => MacroInstr::NA,
    };

    Ok((rest, instr.into()))
}

fn parse_inst_1imm(input: &str) -> IResult<&str, Operation> {
    let (rest, macro_in) = alt((
        value(MacroInstr::Jali(0), tag("jal")),
        value(MacroInstr::Calli(0), tag("call")),
        value(MacroInstr::Taili(0), tag("tail")),
        value(MacroInstr::J(0), tag("j")),
    ))(input)?;
    let (rest, _) = tag(" ")(rest)?;
    let (rest, imm) = parse_imm(rest)?;

    let instr = match macro_in {
        MacroInstr::J(_) => MacroInstr::J(imm),
        MacroInstr::Jali(_) => MacroInstr::Jali(imm),
        MacroInstr::Calli(_) => MacroInstr::Calli(imm),
        MacroInstr::Taili(_) => MacroInstr::Taili(imm),
        _ => MacroInstr::NA,
    };

    Ok((rest, instr.into()))
}

fn parse_inst_1reg(input: &str) -> IResult<&str, Operation> {
    let (rest, macro_in) = alt((
        value(MacroInstr::Jr(Reg::NA), tag("jr")),
        value(MacroInstr::Jalrs(Reg::NA), tag("jalr")),
    ))(input)?;
    let (rest, _) = tag(" ")(rest)?;
    let (rest, register) = parse_reg(rest)?;

    let instr = match macro_in {
        MacroInstr::Jr(_) => MacroInstr::Jr(register),
        MacroInstr::Jalrs(_) => MacroInstr::Jalrs(register),
        _ => MacroInstr::NA,
    };

    Ok((rest, instr.into()))
}

fn parse_inst_1labl1reg(input: &str) -> IResult<&str, Operation> {
    let (rest, macro_in) = alt((
        value(MacroInstr::Lui(Reg::NA, String::new()), tag("lui")),
        value(MacroInstr::Auipc(Reg::NA, String::new()), tag("auipc")),
        value(MacroInstr::Jal(Reg::NA, String::new()), tag("jal")),
    ))(input)?;
    let (rest, _) = tag(" ")(rest)?;
    let (rest, args) = separated_pair(parse_reg, parse_seper, parse_label_name)(rest)?;

    let instr = match macro_in {
        MacroInstr::Lui(_, _) => MacroInstr::Lui(args.0, args.1.to_string()),
        MacroInstr::Auipc(_, _) => MacroInstr::Auipc(args.0, args.1.to_string()),
        MacroInstr::Jal(_, _) => MacroInstr::Jal(args.0, args.1.to_string()),
        _ => MacroInstr::NA,
    };

    Ok((rest, instr.into()))
}

// ld x3, 0x30
fn parse_inst_1imm1reg(input: &str) -> IResult<&str, Operation> {
    let (rest, instr): (&str, Operation) = alt((
        value(Instruction::Lui(Reg::NA, 0).into(), tag("lui")),
        value(Instruction::Auipc(Reg::NA, 0).into(), tag("auipc")),
        value(Instruction::Jal(Reg::NA, 0).into(), tag("jal")),

        value(MacroInstr::Li(Reg::NA, 0).into(), tag("li")),
    ))(input)?;
    let (rest, _) = tag(" ")(rest)?;
    let (rest, args) = separated_pair(parse_reg, parse_seper, parse_imm)(rest)?;

    let instr = match instr {
        Operation::Instr(Instruction::Lui(_, _)) => Operation::Instr(Instruction::Lui(args.0, args.1)),
        Operation::Instr(Instruction::Auipc(_, _)) => Operation::Instr(Instruction::Auipc(args.0, args.1)),
        Operation::Instr(Instruction::Jal(_, _)) => Operation::Instr(Instruction::Jal(args.0, args.1)),

        Operation::Macro(MacroInstr::Li(_, _)) => Operation::Macro(MacroInstr::Li(args.0, args.1)),

        _ => Operation::Instr(Instruction::NA),
    };

    Ok((rest, instr))
}

fn parse_inst_2reg(input: &str) -> IResult<&str, Operation> {
    let (rest, mut instr) = (
        value(MacroInstr::Mv(Reg::NA, Reg::NA), tag("mv"))
    )(input)?;
    let (rest, _) = tag(" ")(rest)?;
    let (rest, args) = separated_pair(parse_reg, parse_seper, parse_reg)(rest)?;

    if let MacroInstr::Mv(_, _) = instr {
        instr = MacroInstr::Mv(args.0, args.1);
    } else {
        instr = MacroInstr::NA;
    }

    Ok((rest, instr.into()))
}

fn parse_inst_1labl2reg(input: &str) -> IResult<&str, Operation> {
    let (rest, instr) = alt((
        value(MacroInstr::Beq(Reg::NA, Reg::NA, String::new()), tag("beq")),
        value(MacroInstr::Bne(Reg::NA, Reg::NA, String::new()), tag("bne")),
        value(MacroInstr::Bltu(Reg::NA, Reg::NA, String::new()), tag("bltu")),
        value(MacroInstr::Bgeu(Reg::NA, Reg::NA, String::new()), tag("bgeu")),
        value(MacroInstr::Blt(Reg::NA, Reg::NA, String::new()), tag("blt")),
        value(MacroInstr::Bge(Reg::NA, Reg::NA, String::new()), tag("bge")),

        value(MacroInstr::Jalr(Reg::NA, Reg::NA, String::new()), tag("jalr")),

        value(MacroInstr::Slli(Reg::NA, Reg::NA, String::new()), tag("slli")),
        value(MacroInstr::Srli(Reg::NA, Reg::NA, String::new()), tag("srli")),
        value(MacroInstr::Srai(Reg::NA, Reg::NA, String::new()), tag("srai")),

        value(MacroInstr::Sb(Reg::NA, Reg::NA, String::new()), tag("sb")),
        value(MacroInstr::Sh(Reg::NA, Reg::NA, String::new()), tag("sh")),
        value(MacroInstr::Sw(Reg::NA, Reg::NA, String::new()), tag("sw")),

        value(MacroInstr::Lbu(Reg::NA, Reg::NA, String::new()), tag("lbu")),
        value(MacroInstr::Lhu(Reg::NA, Reg::NA, String::new()), tag("lhu")),
        value(MacroInstr::Lb(Reg::NA, Reg::NA, String::new()), tag("lb")),
        value(MacroInstr::Lh(Reg::NA, Reg::NA, String::new()), tag("lh")),
        value(MacroInstr::Lw(Reg::NA, Reg::NA, String::new()), tag("lw")),
    ))(input)?;
    let (rest, _) = tag(" ")(rest)?;
    let (rest, args) = tuple((parse_reg, parse_seper, parse_reg, parse_seper, parse_label_name))(rest)?;

    let instr = match instr {
        MacroInstr::Beq(_, _, _) => MacroInstr::Beq(args.0, args.2, args.4.to_string()),
        MacroInstr::Bne(_, _, _) => MacroInstr::Bne(args.0, args.2, args.4.to_string()),
        MacroInstr::Blt(_, _, _) => MacroInstr::Blt(args.0, args.2, args.4.to_string()),
        MacroInstr::Bltu(_, _, _) => MacroInstr::Bltu(args.0, args.2, args.4.to_string()),
        MacroInstr::Bge(_, _, _) => MacroInstr::Bge(args.0, args.2, args.4.to_string()),
        MacroInstr::Bgeu(_, _, _) => MacroInstr::Bgeu(args.0, args.2, args.4.to_string()),

        MacroInstr::Jalr(_, _, _) => MacroInstr::Jalr(args.0, args.2, args.4.to_string()),

        MacroInstr::Slli(_, _, _) => MacroInstr::Slli(args.0, args.2, args.4.to_string()),
        MacroInstr::Srli(_, _, _) => MacroInstr::Srli(args.0, args.2, args.4.to_string()),
        MacroInstr::Srai(_, _, _) => MacroInstr::Srai(args.0, args.2, args.4.to_string()),

        MacroInstr::Sb(_, _, _) => MacroInstr::Sb(args.0, args.2, args.4.to_string()),
        MacroInstr::Sh(_, _, _) => MacroInstr::Sh(args.0, args.2, args.4.to_string()),
        MacroInstr::Sw(_, _, _) => MacroInstr::Sw(args.0, args.2, args.4.to_string()),

        MacroInstr::Lb(_, _, _) => MacroInstr::Lb(args.0, args.2, args.4.to_string()),
        MacroInstr::Lbu(_, _, _) => MacroInstr::Lbu(args.0, args.2, args.4.to_string()),
        MacroInstr::Lh(_, _, _) => MacroInstr::Lh(args.0, args.2, args.4.to_string()),
        MacroInstr::Lhu(_, _, _) => MacroInstr::Lhu(args.0, args.2, args.4.to_string()),
        MacroInstr::Lw(_, _, _) => MacroInstr::Lw(args.0, args.2, args.4.to_string()),

        _ => MacroInstr::NA
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
    let (rest, _) = tag(" ")(rest)?;
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
    let (rest, instr): (&str, Operation) = alt((
        value(Instruction::Addi(Reg::NA, Reg::NA, 0).into(), tag("addi")),

        value(Instruction::Slti(Reg::NA, Reg::NA, 0).into(), tag("slti")),
        value(Instruction::Sltiu(Reg::NA, Reg::NA, 0).into(), tag("sltiu")),

        value(Instruction::Xori(Reg::NA, Reg::NA, 0).into(), tag("xori")),
        value(Instruction::Ori(Reg::NA, Reg::NA, 0).into(), tag("ori")),
        value(Instruction::Andi(Reg::NA, Reg::NA, 0).into(), tag("andi")),

        value(Instruction::Jalr(Reg::NA, Reg::NA, 0).into(), tag("jalr")),

        value(MacroInstr::Srr(Reg::NA, Reg::NA, 0).into(), tag("srr")),
        value(MacroInstr::Slr(Reg::NA, Reg::NA, 0).into(), tag("slr"))
    ))(input)?;
    let (rest, _) = tag(" ")(rest)?;
    let (rest, args) = tuple((parse_reg, parse_seper, parse_reg, parse_seper, parse_imm))(rest)?;

    let instr = match instr {
        Operation::Instr(Instruction::Addi(_, _, _)) => Operation::Instr(Instruction::Addi(args.0, args.2, args.4)),

        Operation::Instr(Instruction::Slti(_, _, _)) => Operation::Instr(Instruction::Slti(args.0, args.2, args.4)),
        Operation::Instr(Instruction::Sltiu(_, _, _)) => Operation::Instr(Instruction::Sltiu(args.0, args.2, args.4)),
        Operation::Instr(Instruction::Xori(_, _, _)) => Operation::Instr(Instruction::Xori(args.0, args.2, args.4)),
        Operation::Instr(Instruction::Ori(_, _, _)) => Operation::Instr(Instruction::Ori(args.0, args.2, args.4)),
        Operation::Instr(Instruction::Andi(_, _, _)) => Operation::Instr(Instruction::Andi(args.0, args.2, args.4)),

        Operation::Instr(Instruction::Jalr(_, _, _)) => Operation::Instr(Instruction::Jalr(args.0, args.2, args.4)),

        Operation::Macro(MacroInstr::Srr(_, _, _)) => Operation::Macro(MacroInstr::Srr(args.0, args.2, args.4)),
        Operation::Macro(MacroInstr::Slr(_, _, _)) => Operation::Macro(MacroInstr::Slr(args.0, args.2, args.4)),

        _ => Operation::Instr(Instruction::NA)
    };

    Ok((rest, instr))
}

fn parse_inst_3reg(input: &str) -> IResult<&str, Operation> {
    let (rest, instr): (&str, Operation) = alt((
        value(Instruction::Addn(Reg::NA, Reg::NA, Reg::NA).into(), tag("add")),
        value(Instruction::Subn(Reg::NA, Reg::NA, Reg::NA).into(), tag("sub")),

        value(Instruction::Xor(Reg::NA, Reg::NA, Reg::NA).into(), tag("xor")),
        value(Instruction::Or(Reg::NA, Reg::NA, Reg::NA).into(), tag("or")),
        value(Instruction::And(Reg::NA, Reg::NA, Reg::NA).into(), tag("and")),

        value(Instruction::Slt(Reg::NA, Reg::NA, Reg::NA).into(),tag("slt")),
        value(Instruction::Sltu(Reg::NA, Reg::NA, Reg::NA).into(),tag("sltu")),

        value(Instruction::Sll(Reg::NA, Reg::NA, Reg::NA).into(), tag("sll")),
        value(Instruction::Srl(Reg::NA, Reg::NA, Reg::NA).into(), tag("srl")),
        value(Instruction::Sra(Reg::NA, Reg::NA, Reg::NA).into(), tag("sra")),

        value(MacroInstr::Divn(Reg::NA, Reg::NA, Reg::NA).into(), tag("div")),
        value(MacroInstr::Muln(Reg::NA, Reg::NA, Reg::NA).into(), tag("mul")),

        value(MacroInstr::Xnor(Reg::NA, Reg::NA, Reg::NA).into(), tag("xnor")),
        value(MacroInstr::Nor(Reg::NA, Reg::NA, Reg::NA).into(), tag("nor")),
    ))(input)?;
    let (rest, _) = tag(" ")(rest)?;
    let (rest, args) = tuple((parse_reg, parse_seper, parse_reg, parse_seper, parse_reg))(rest)?;

    let instr: Operation = match instr {
        Operation::Instr(instruct) => {
            match instruct {
                Instruction::Addn(_, _, _) => Instruction::Addn(args.0, args.2, args.4),

                Instruction::Subn(_, _, _) => Instruction::Subn(args.0, args.2, args.4),

                Instruction::Xor(_, _, _) => Instruction::Xor(args.0, args.2, args.4),
                Instruction::Or(_, _, _) => Instruction::Or(args.0, args.2, args.4),
                Instruction::And(_, _, _) => Instruction::And(args.0, args.2, args.4),
                Instruction::Slt(_, _, _) => Instruction::Slt(args.0, args.2, args.4),
                Instruction::Sltu(_, _, _) => Instruction::Sltu(args.0, args.2, args.4),

                Instruction::Sll(_, _, _) => Instruction::Sll(args.0, args.2, args.4),
                Instruction::Srl(_, _, _) => Instruction::Srl(args.0, args.2, args.4),
                Instruction::Sra(_, _, _) => Instruction::Sra(args.0, args.2, args.4),

                _ => Instruction::NA
            }.into()
        },
        Operation::Macro(macro_in) => {
            match macro_in {
                MacroInstr::Divn(_, _, _) => MacroInstr::Divn(args.0, args.2, args.4),
                MacroInstr::Muln(_, _, _) => MacroInstr::Muln(args.0, args.2, args.4),

                MacroInstr::Xnor(_, _, _) => MacroInstr::Xnor(args.0, args.2, args.4),
                MacroInstr::Nor(_, _, _) => MacroInstr::Nor(args.0, args.2, args.4),

                _ => MacroInstr::NA,
            }.into()
        },
        _ => Operation::Instr(Instruction::NA),
    };

    Ok((rest, instr.into()))
}

fn parse_instruction(input: &str) -> IResult<&str, Operation> {
    alt((
        parse_inst_noparm,
        parse_inst_1labl,
        parse_inst_1imm,
        parse_inst_1reg,
        parse_inst_1labl1reg,
        parse_inst_1imm1reg,
        parse_inst_1labl2reg,
        parse_inst_1imm2reg_up,
        parse_inst_1imm2reg_lw,
        parse_inst_2reg,
        parse_inst_3reg
    ))(input)
}

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

pub fn parse<'a>(input: &'a str, subroutines: &mut Option<&mut Subroutines>) -> IResult<&'a str, (LabelRecog, Vec<Operation<'a>>)> {
    let mut local_ref_not_def: HashSet<String> = HashSet::new();
    let mut symbol_map = LabelRecog::new();
    let mut instr_list: Vec<Operation> = vec![];

    let mut rest = input;
    let mut instr_counter: usize = 0;

    let mut privileged = false;

    if let None = subroutines {
        privileged = true;
    }

    loop {
        let res = match privileged {
            true => parse_line_priv(rest),
            false => parse_line(rest),
        };

        let parsed = match res {
            Ok(line) => {
                rest = line.0;
                line.1
            },
            Err(_) => todo!("Custom parser error!"),
        };

        match parsed {
            (Some(label), Some(instr)) => {
                let _ = match label.strip_prefix(".") {
                    Some(label) => {
                        // Local label; Track definitions and references!
                        let label_string = &label.to_string();
                        // TODO: Evaluate if .unwrap is appropriate!
                        symbol_map.crt_or_def_label(label_string, false, instr_counter.try_into().unwrap());
                        local_ref_not_def.remove(label_string);
                    },
                    None => {
                        // Global label; Do not track definitions and references!
                        // TODO: Evaluate if .unwrap is appropriate!
                        symbol_map.crt_or_def_label(&label.to_string(), true, instr_counter.try_into().unwrap())
                    },
                };

                match &instr {
                    Operation::Macro(macro_in) => {
                        match macro_in {
                            MacroInstr::Muln(_, _, _) => subroutines.as_mut().unwrap().mul_defined(),
                            MacroInstr::Divn(_, _, _) => subroutines.as_mut().unwrap().div_defined(),
                            MacroInstr::Srr(_, _, _) => subroutines.as_mut().unwrap().srr_defined(),
                            MacroInstr::Slr(_, _, _) => subroutines.as_mut().unwrap().slr_defined(),

                            MacroInstr::Beq(_, _, labl) | 
                            MacroInstr::Bne(_, _, labl) |
                            MacroInstr::Blt(_, _, labl) |
                            MacroInstr::Bltu(_, _, labl) |
                            MacroInstr::Bge(_, _, labl) |
                            MacroInstr::Bgeu(_, _, labl) |

                            MacroInstr::Jal(_, labl) |
                            MacroInstr::Jalr(_, _, labl) |

                            MacroInstr::Jl(labl) |
                            MacroInstr::Jall(labl) |
                            MacroInstr::Calll(labl) |
                            MacroInstr::Taill(labl) |

                            MacroInstr::Lui(_, labl) |
                            MacroInstr::Auipc(_, labl) |

                            MacroInstr::Slli(_, _, labl) |
                            MacroInstr::Srli(_, _, labl) |
                            MacroInstr::Srai(_, _, labl) |

                            MacroInstr::Lb(_, _, labl) |
                            MacroInstr::Lh(_, _, labl) |
                            MacroInstr::Lw(_, _, labl) |

                            MacroInstr::Lbu(_, _, labl) |
                            MacroInstr::Lhu(_, _, labl) |

                            MacroInstr::Sb(_, _, labl) |
                            MacroInstr::Sh(_, _, labl) |
                            MacroInstr::Sw(_, _, labl) => {
                                if !symbol_map.crt_or_ref_label(labl, false) {
                                    local_ref_not_def.insert(labl.clone());
                                }
                            },
                            _ => (),
                        }
                        instr_counter = instr_counter + macro_in.lines();

                        instr_list.push(Operation::LablMacro(label, macro_in.to_owned()));
                    },
                    Operation::Instr(instr_in) => {
                        instr_counter = instr_counter + 1;
                        instr_list.push(Operation::LablInstr(label, instr_in.to_owned()));
                    }
                    _ => (),
                }
            },
            (None, Some(instr)) => {
                match &instr {
                    Operation::Macro(macro_in) => {
                        match macro_in {
                            MacroInstr::Muln(_, _, _) => subroutines.as_mut().unwrap().mul_defined(),
                            MacroInstr::Divn(_, _, _) => subroutines.as_mut().unwrap().div_defined(),
                            MacroInstr::Srr(_, _, _) => subroutines.as_mut().unwrap().srr_defined(),
                            MacroInstr::Slr(_, _, _) => subroutines.as_mut().unwrap().slr_defined(),

                            MacroInstr::Beq(_, _, labl) | 
                            MacroInstr::Bne(_, _, labl) |
                            MacroInstr::Blt(_, _, labl) |
                            MacroInstr::Bltu(_, _, labl) |
                            MacroInstr::Bge(_, _, labl) |
                            MacroInstr::Bgeu(_, _, labl) |

                            MacroInstr::Jal(_, labl) |
                            MacroInstr::Jalr(_, _, labl) |

                            MacroInstr::Jl(labl) |
                            MacroInstr::Jall(labl) |
                            MacroInstr::Calll(labl) |
                            MacroInstr::Taill(labl) |

                            MacroInstr::Lui(_, labl) |
                            MacroInstr::Auipc(_, labl) |

                            MacroInstr::Slli(_, _, labl) |
                            MacroInstr::Srli(_, _, labl) |
                            MacroInstr::Srai(_, _, labl) |

                            MacroInstr::Lb(_, _, labl) |
                            MacroInstr::Lh(_, _, labl) |
                            MacroInstr::Lw(_, _, labl) |

                            MacroInstr::Lbu(_, _, labl) |
                            MacroInstr::Lhu(_, _, labl) |

                            MacroInstr::Sb(_, _, labl) |
                            MacroInstr::Sh(_, _, labl) |
                            MacroInstr::Sw(_, _, labl) => {
                                if !symbol_map.crt_or_ref_label(labl, false) {
                                    local_ref_not_def.insert(labl.clone());
                                }
                            },
                            _ => (),
                        }

                        instr_counter = instr_counter + macro_in.lines();
                    },
                    _ => instr_counter = instr_counter + 1,
                }

                instr_list.push(instr);
            },
            (Some(label), None) => {
                let _ = match label.strip_prefix(".") {
                    Some(label) => {
                        // Local label; Track definitions and references!
                        let label_string = &label.to_string();
                        // TODO: Evaluate if .unwrap is appropriate!
                        symbol_map.crt_or_def_label(label_string, false, instr_counter.try_into().unwrap());
                        local_ref_not_def.remove(label_string);
                    },
                    None => {
                        // Global label; Do not track definitions and references!
                        // TODO: Evaluate if .unwrap is appropriate!
                        symbol_map.crt_or_def_label(&label.to_string(), true, instr_counter.try_into().unwrap())
                    },
                };
                instr_counter = instr_counter + 1;
                instr_list.push(Operation::Labl(label));
            },
            (None, None) => (),
        }

        if rest.is_empty() {
            break;
        }
    }

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
        assert_ne!(parse_inst_noparm("invalid"), Ok(("", Operation::Macro(MacroInstr::NOP))));
        assert_ne!(parse_inst_noparm("noop"), Ok(("", Operation::Macro(MacroInstr::NOP))));
        assert_eq!(parse_inst_noparm("nop"), Ok(("", Operation::Macro(MacroInstr::NOP))));
        assert_ne!(parse_inst_noparm("nop x1"), Ok(("", Operation::Macro(MacroInstr::NOP))));
        assert_eq!(parse_inst_noparm("ret"), Ok(("", Operation::Macro(MacroInstr::Ret))));
        assert_ne!(parse_inst_noparm("ret nop"), Ok(("", Operation::Macro(MacroInstr::Ret))));
    }

    #[test]
    fn test_parse_instr1labl() {
        assert_ne!(parse_inst_1labl("invalid"), Ok(("", MacroInstr::NA.into())));
        assert_ne!(parse_inst_1labl(" "), Ok(("", MacroInstr::NA.into())));
        assert_ne!(parse_inst_1labl(""), Ok(("", MacroInstr::NA.into())));
        assert_ne!(parse_inst_1labl("call"), Ok(("", MacroInstr::NA.into())));
        assert_eq!(parse_inst_1labl("tail test"), Ok(("", MacroInstr::Taill("test".to_string()).into())));
        assert_eq!(parse_inst_1labl("call HANS"), Ok(("", MacroInstr::Calll("HANS".to_string()).into())));
        assert_ne!(parse_inst_1labl("call label  "), Ok(("", MacroInstr::Calll("label".to_string()).into())));
    }

    #[test]
    fn test_parse_instr1imm() {
        assert_ne!(parse_inst_1imm("invalid"), Ok(("", MacroInstr::NA.into())));
        assert_ne!(parse_inst_1imm(" "), Ok(("", MacroInstr::NA.into())));
        assert_ne!(parse_inst_1imm(""), Ok(("", MacroInstr::NA.into())));
        assert_ne!(parse_inst_1imm("j"), Ok(("", MacroInstr::NA.into())));
        assert_eq!(parse_inst_1imm("j 12"), Ok(("", MacroInstr::J(12).into())));
        assert_eq!(parse_inst_1imm("call 0x10"), Ok(("", MacroInstr::Calli(16).into())));
        assert_ne!(parse_inst_1imm("jal 125  "), Ok(("", MacroInstr::Jali(125).into())));
    }

    #[test]
    fn test_parse_instr1reg() {
        assert_ne!(parse_inst_1reg("invalid"), Ok(("", MacroInstr::NA.into())));
        assert_ne!(parse_inst_1reg(" "), Ok(("", MacroInstr::NA.into())));
        assert_ne!(parse_inst_1reg(""), Ok(("", MacroInstr::NA.into())));
        assert_ne!(parse_inst_1reg("jr"), Ok(("", MacroInstr::Jr(Reg::NA).into())));
        assert_eq!(parse_inst_1reg("jalr a2"), Ok(("", MacroInstr::Jalrs(Reg::G12).into())));
        assert_eq!(parse_inst_1reg("jr x18"), Ok(("", MacroInstr::Jr(Reg::G18).into())));
        assert_ne!(parse_inst_1reg("jalr x19  "), Ok(("", MacroInstr::Jalrs(Reg::G19).into())));
    }

    #[test]
    fn test_parse_instr1labl1reg() {
        assert_ne!(parse_inst_1labl1reg(""), Ok(("", MacroInstr::NA.into())));
        assert_ne!(parse_inst_1labl1reg("lui"), Ok(("", MacroInstr::Lui(Reg::NA, "".to_string()).into())));
        assert_eq!(parse_inst_1labl1reg("lui a2, stop"), Ok(("", MacroInstr::Lui(Reg::G12, "stop".to_string()).into())));
        assert_eq!(parse_inst_1labl1reg("auipc s2, helloWorld"), Ok(("", MacroInstr::Auipc(Reg::G18, "helloWorld".to_string()).into())));
        assert_eq!(parse_inst_1labl1reg("jal x20, test"), Ok(("", MacroInstr::Jal(Reg::G20, "test".to_string()).into())));
        assert_ne!(parse_inst_1labl1reg("jal x19, train "), Ok(("", MacroInstr::Jal(Reg::G19, "train".to_string()).into())));
    }

    #[test]
    fn test_parse_instr1imm1reg() {
        assert_ne!(parse_inst_1imm1reg("invalid"), Ok(("", Instruction::NA.into())));
        assert_ne!(parse_inst_1imm1reg(" "), Ok(("", Instruction::NA.into())));
        assert_ne!(parse_inst_1imm1reg(""), Ok(("", Instruction::NA.into())));
        assert_ne!(parse_inst_1imm1reg("ld"), Ok(("", Instruction::NA.into())));
        assert_ne!(parse_inst_1imm1reg(""), Ok(("", Instruction::NA.into())));
        assert_ne!(parse_inst_1imm1reg("lui"), Ok(("", Instruction::Lui(Reg::NA, 0).into())));
        assert_eq!(parse_inst_1imm1reg("lui x12, 12"), Ok(("", Instruction::Lui(Reg::G12, 12).into())));
        assert_eq!(parse_inst_1imm1reg("auipc x18, 0x20"), Ok(("", Instruction::Auipc(Reg::G18, 32).into())));
        assert_eq!(parse_inst_1imm1reg("jal x20, 5"), Ok(("", Instruction::Jal(Reg::G20, 5).into())));
        assert_ne!(parse_inst_1imm1reg("jal x19, 125 "), Ok(("", Instruction::Jal(Reg::G19, 125).into())));
    }

    #[test]
    fn test_parse_inst_2reg() {
        assert_ne!(parse_inst_2reg("invalid"), Ok(("", MacroInstr::NA.into())));
        assert_ne!(parse_inst_2reg("   "), Ok(("", MacroInstr::NA.into())));
        assert_ne!(parse_inst_2reg("ld x1, 0xAA"), Ok(("", MacroInstr::NA.into())));
        assert_ne!(parse_inst_2reg("mv x1, 0xAA"), Ok(("", MacroInstr::NA.into())));
        assert_eq!(parse_inst_2reg("mv x1, x4"), Ok(("", MacroInstr::Mv(Reg::G1, Reg::G4).into())));
        assert_eq!(parse_inst_2reg("mv x12,x4"), Ok(("", MacroInstr::Mv(Reg::G12, Reg::G4).into())));
    }

    #[test]
    fn test_parse_instr1labl2reg() {
        assert_ne!(parse_inst_1labl2reg("invalid"), Ok(("", MacroInstr::NA.into())));
        assert_ne!(parse_inst_1labl2reg("   "), Ok(("", MacroInstr::NA.into())));
        assert_ne!(parse_inst_1labl2reg("sb x1, x6"), Ok(("", MacroInstr::Sb(Reg::G1, Reg::G6, "".to_string()).into())));
        assert_ne!(parse_inst_1labl2reg("lb x1, total"), Ok(("", MacroInstr::Lb(Reg::G1, Reg::NA, "total".to_string()).into())));
        assert_eq!(parse_inst_1labl2reg("bgeu x1, x4, sTaRt"), Ok(("", MacroInstr::Bgeu(Reg::G1, Reg::G4, "sTaRt".to_string()).into())));
        assert_ne!(parse_inst_1labl2reg("slli x1x4,eNND"), Ok(("", MacroInstr::Slli(Reg::G1, Reg::G4, "eNND".to_string()).into())));
        assert_eq!(parse_inst_1labl2reg("blt x10,x10, last"), Ok(("", MacroInstr::Blt(Reg::G10, Reg::G10, "last".to_string()).into())));
        assert_ne!(parse_inst_1labl2reg("jalr x6,  x8,test"), Ok(("", MacroInstr::Jalr(Reg::G6, Reg::G8, "test".to_string()).into())));
        assert_eq!(parse_inst_1labl2reg("lhu x1, x2, hans"), Ok(("", MacroInstr::Lhu(Reg::G1, Reg::G2, "hans".to_string()).into())));
        assert_eq!(parse_inst_1labl2reg("sb x13,x15,loading"), Ok(("", MacroInstr::Sb(Reg::G13, Reg::G15, "loading".to_string()).into())));
        assert_ne!(parse_inst_1labl2reg("beqx1x15,start"), Ok(("", MacroInstr::Beq(Reg::G1, Reg::G15, "start".to_string()).into())));
        assert_ne!(parse_inst_1labl2reg("lbu x12, x15,  dasletzte"), Ok(("", MacroInstr::Lbu(Reg::G12, Reg::G15, "dasletzte".to_string()).into())));
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
        assert_eq!(parse_inst_1imm2reg_up("srr x13,x15,6"), Ok(("", MacroInstr::Srr(Reg::G13, Reg::G15, 6).into())));
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
        assert_ne!(parse_inst_3reg("xnor x6,  x8,x5"), Ok(("", MacroInstr::Xnor(Reg::G6, Reg::G8, Reg::G5).into())));
        assert_eq!(parse_inst_3reg("and x6, x8, x14"), Ok(("", Instruction::And(Reg::G6, Reg::G8, Reg::G14).into())));
        assert_ne!(parse_inst_3reg("sll x6,  x8, x14"), Ok(("", Instruction::Sll(Reg::G6, Reg::G8, Reg::G14).into())));
    }

    #[test]
    fn test_parse_instruction() {
        assert_eq!(parse_instruction("mv x1, x6"), Ok(("", MacroInstr::Mv(Reg::G1, Reg::G6).into())));
        assert_ne!(parse_instruction("addi x1, 0xAA"), Ok(("", Instruction::Addi(Reg::G1, Reg::NA, 0xAA).into())));
        assert_eq!(parse_instruction("mul x1, x4, x6"), Ok(("", MacroInstr::Muln(Reg::G1, Reg::G4, Reg::G6).into())));
        assert_ne!(parse_instruction("xor x10x14,x7"), Ok(("", Instruction::Xor(Reg::G10, Reg::G14, Reg::G7).into())));
        assert_eq!(parse_instruction("add x10,x11, x10"), Ok(("", Instruction::Addn(Reg::G10, Reg::G11, Reg::G10).into())));
        assert_ne!(parse_instruction("xnor x6,  x8,x5"), Ok(("", MacroInstr::Xnor(Reg::G6, Reg::G8, Reg::G5).into())));
        assert_eq!(parse_instruction("srr x5, x8, 7"), Ok(("", MacroInstr::Srr(Reg::G5, Reg::G8, 7).into())));
        // More tests?
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
                   Ok(("\naddi x12, x10, 0x05", (None, Some(MacroInstr::Mv(Reg::G15, Reg::G12).into())))));
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
                    Ok(("\naddi x12, x10, 0x05", (None, Some(MacroInstr::Mv(Reg::G15, Reg::G12).into())))));
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

        let correct_vec: Vec<Operation> = vec![
                                                 Operation::LablMacro(Cow::from("START"), MacroInstr::Li(Reg::G4, 16)),
                                                 Operation::from(MacroInstr::Mv(Reg::G3, Reg::G4)),
                                                 Operation::LablMacro(Cow::from("MUL"), MacroInstr::Beq(Reg::G3, Reg::G4, "END".to_string())),
                                                 Operation::from(MacroInstr::Muln(Reg::G6, Reg::G4, Reg::G3)),
                                                 Operation::from(Instruction::Lui(Reg::G4, 0x16)),
                                                 Operation::from(MacroInstr::Jl("MUL".to_string())),
                                                 Operation::Labl(Cow::from("END"))
                                                ];

        assert_eq!(parse(source_code, &mut Some(&mut subroutines)),
                   Ok(("", (symbols, correct_vec))));
        assert_eq!(subroutines.get_code(), Vec::from([Subroutines::MUL_SUB]));

        // TODO: Probably more test cases!
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
