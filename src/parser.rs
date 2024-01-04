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
    Xnor,
    Nor,
    Equal
}

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

    pub fn mul_defined(&mut self) {
        self.code_str_vec.insert(Self::MUL_SUB.to_string());
    }

    pub fn div_defined(&mut self) {
        self.code_str_vec.insert(Self::DIV_SUB.to_string());
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

fn parse_macro_noparm(input: &str) -> IResult<&str, Operation> {
    let (rest, instr) = alt((
        value(Operation::Instr(Instruction::Addi(Reg::G0, Reg::G0, 0)), tag("nop")),
        value(Operation::Instr(Instruction::Jalr(Reg::G0, Reg::G1, 0)), tag("ret")),
    ))(input)?;

    Ok((rest, instr))
}

fn parse_macro_1labl(input: &str) -> IResult<&str, Vec<Operation>> {
    let (rest, macro_in) = alt((
        value(IntermediateOp::Call, tag("call")),
        value(IntermediateOp::Tail, tag("tail")),

        value(IntermediateOp::Jal, tag("jal")),
        value(IntermediateOp::J, tag("j")),
    ))(input)?;
    let (rest, _) = tag(" ")(rest)?;
    let (rest, labl) = parse_label_name(rest)?;

    let instr = match macro_in {
        IntermediateOp::Call => Vec::from([
            MacroInstr::Auipc(Reg::G1, labl.to_string()).into(),
            MacroInstr::Jalr(Reg::G1, Reg::G1, labl.to_string()).into()
        ]),
        IntermediateOp::Tail => Vec::from([
            MacroInstr::Auipc(Reg::G6, labl.to_string()).into(),
            MacroInstr::Jalr(Reg::G0, Reg::G6, labl.to_string()).into()
        ]),
        IntermediateOp::J => Vec::from([MacroInstr::Jal(Reg::G0, labl.to_string()).into()]),
        IntermediateOp::Jal => Vec::from([MacroInstr::Jal(Reg::G1, labl.to_string()).into()]),

        op => panic!("[Error] Could not map parsed instruction to internal data structure: {:?}", op),
    };

    Ok((rest, instr))
}

fn parse_macro_1imm(input: &str) -> IResult<&str, Vec<Operation>> {
    let (rest, macro_in) = alt((
        value(IntermediateOp::Call, tag("call")),
        value(IntermediateOp::Tail, tag("tail")),

        value(IntermediateOp::Jal, tag("jal")),
        value(IntermediateOp::J, tag("j")),
    ))(input)?;
    let (rest, _) = tag(" ")(rest)?;
    let (rest, imm) = parse_imm(rest)?;

    let instr = match macro_in {
        IntermediateOp::Call => Vec::from([
            Instruction::Auipc(Reg::G1, imm >> 12).into(),
            Instruction::Jalr(Reg::G1, Reg::G1, imm).into()
        ]),
        IntermediateOp::Tail => Vec::from([
            Instruction::Auipc(Reg::G6, imm >> 12).into(),
            Instruction::Jalr(Reg::G0, Reg::G6, imm).into()
        ]),
        IntermediateOp::J => Vec::from([Instruction::Jal(Reg::G0, imm).into()]),
        IntermediateOp::Jal => Vec::from([Instruction::Jal(Reg::G1, imm).into()]),
        op => panic!("[Error] Could not map parsed instruction to internal data structure: {:?}", op),
    };

    Ok((rest, instr))
}

fn parse_macro_1reg(input: &str) -> IResult<&str, Operation> {
    let (rest, macro_in) = alt((
        value(IntermediateOp::Jr, tag("jr")),
        value(IntermediateOp::Jalr, tag("jalr")),
    ))(input)?;
    let (rest, _) = tag(" ")(rest)?;
    let (rest, register) = parse_reg(rest)?;

    let instr = match macro_in {
        IntermediateOp::Jr => Instruction::Jalr(Reg::G0, register, 0),
        IntermediateOp::Jalr => Instruction::Jalr(Reg::G1, register, 0),
        op => panic!("[Error] Could not map parsed instruction to internal data structure: {:?}", op),
    };

    Ok((rest, instr.into()))
}

fn parse_macro_1labl1reg(input: &str) -> IResult<&str, Operation> {
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
        op => panic!("[Error] Could not map parsed instruction to internal data structure: {:?}", op),
    };

    Ok((rest, instr.into()))
}

// ld x3, 0x30
fn parse_inst_1imm1reg(input: &str) -> IResult<&str, Vec<Operation>> {
    let (rest, instr) = alt((
        value(IntermediateOp::Lui, tag("lui")),
        value(IntermediateOp::Auipc, tag("auipc")),
        value(IntermediateOp::Jal, tag("jal")),

        value(IntermediateOp::Li, tag("li")),
    ))(input)?;
    let (rest, _) = tag(" ")(rest)?;
    let (rest, args) = separated_pair(parse_reg, parse_seper, parse_imm)(rest)?;

    let instr = match instr {
        IntermediateOp::Lui => Vec::from([Instruction::Lui(args.0, args.1).into()]),
        IntermediateOp::Auipc => Vec::from([Instruction::Auipc(args.0, args.1).into()]),
        IntermediateOp::Jal => Vec::from([Instruction::Jal(args.0, args.1).into()]),

        IntermediateOp::Li => Vec::from([
            Instruction::Lui(args.0.clone(), args.1 >> 12).into(), 
            Instruction::Addi(args.0.clone(), args.0.clone(), args.1).into()
        ]),

        op => panic!("[Error] Could not map parsed instruction to internal data structure: {:?}", op),
    };

    Ok((rest, instr))
}

fn parse_macro_2reg(input: &str) -> IResult<&str, Operation> {
    let (rest, instr) = (
        value(IntermediateOp::Mv, tag("mv"))
    )(input)?;
    let (rest, _) = tag(" ")(rest)?;
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

fn parse_inst_1imm2reg_up(input: &str) -> IResult<&str, Vec<Operation>> {
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
    let (rest, _) = tag(" ")(rest)?;
    let (rest, args) = tuple((parse_reg, parse_seper, parse_reg, parse_seper, parse_imm))(rest)?;

    let instr = match instr {
        IntermediateOp::Addi => Vec::from([Instruction::Addi(args.0, args.2, args.4).into()]),

        IntermediateOp::Slti => Vec::from([Instruction::Slti(args.0, args.2, args.4).into()]),
        IntermediateOp::Sltiu => Vec::from([Instruction::Sltiu(args.0, args.2, args.4).into()]),
        IntermediateOp::Xori => Vec::from([Instruction::Xori(args.0, args.2, args.4).into()]),
        IntermediateOp::Ori => Vec::from([Instruction::Ori(args.0, args.2, args.4).into()]),
        IntermediateOp::Andi => Vec::from([Instruction::Andi(args.0, args.2, args.4).into()]),

        IntermediateOp::Jalr => Vec::from([Instruction::Jalr(args.0, args.2, args.4).into()]),

        IntermediateOp::Srr => {
            let mut returned_vec: Vec<Operation> = Vec::with_capacity(4);

            if args.2 != Reg::G10 {
                returned_vec.push(Instruction::Addi(Reg::G10, args.2, 0).into());
            }
            returned_vec.push(Instruction::Addi(Reg::G11, Reg::G0, args.4).into());
            returned_vec.push(MacroInstr::Jal(Reg::G1, "_SRR".to_string()).into());
            if args.0 != Reg::G10 {
                returned_vec.push(Instruction::Addi(args.0, Reg::G10, 0).into());
            }

            returned_vec
        },
        IntermediateOp::Slr => {
            let mut returned_vec: Vec<Operation> = Vec::with_capacity(4);

            if args.2 != Reg::G10 {
                returned_vec.push(Instruction::Addi(Reg::G10, args.2, 0).into());
            }
            returned_vec.push(Instruction::Addi(Reg::G11, Reg::G0, args.4).into());
            returned_vec.push(MacroInstr::Jal(Reg::G1, "_SLR".to_string()).into());
            if args.0 != Reg::G10 {
                returned_vec.push(Instruction::Addi(args.0, Reg::G10, 0).into());
            }

            returned_vec
        },

        op => panic!("[Error] Could not map parsed instruction to internal data structure: {:?}", op),
    };

    Ok((rest, instr))
}

fn parse_inst_3reg(input: &str) -> IResult<&str, Vec<Operation>> {
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

        value(IntermediateOp::Xnor, tag("xnor")),
        value(IntermediateOp::Equal, tag("eq")),
        value(IntermediateOp::Nor, tag("nor")),
    ))(input)?;
    let (rest, _) = tag(" ")(rest)?;
    let (rest, args) = tuple((parse_reg, parse_seper, parse_reg, parse_seper, parse_reg))(rest)?;

    let instr = match instr {
        IntermediateOp::Add => Vec::from([Instruction::Addn(args.0, args.2, args.4).into()]),
        IntermediateOp::Sub => Vec::from([Instruction::Subn(args.0, args.2, args.4).into()]),

        IntermediateOp::Xor => Vec::from([Instruction::Xor(args.0, args.2, args.4).into()]),
        IntermediateOp::Or => Vec::from([Instruction::Or(args.0, args.2, args.4).into()]),
        IntermediateOp::And => Vec::from([Instruction::And(args.0, args.2, args.4).into()]),

        IntermediateOp::Slt => Vec::from([Instruction::Slt(args.0, args.2, args.4).into()]),
        IntermediateOp::Sltu => Vec::from([Instruction::Sltu(args.0, args.2, args.4).into()]),

        IntermediateOp::Sll => Vec::from([Instruction::Sll(args.0, args.2, args.4).into()]),
        IntermediateOp::Srl => Vec::from([Instruction::Srl(args.0, args.2, args.4).into()]),
        IntermediateOp::Sra => Vec::from([Instruction::Sra(args.0, args.2, args.4).into()]),

        IntermediateOp::Div => {
            let mut returned_vec: Vec<Operation> = Vec::with_capacity(4);

            if args.2 != Reg::G10 {
                returned_vec.push(Instruction::Addi(Reg::G10, args.2, 0).into());
            }
            if args.4 != Reg::G11 {
                returned_vec.push(Instruction::Addi(Reg::G11, args.4, 0).into());
            }
            returned_vec.push(MacroInstr::Jal(Reg::G1, "_DIV".to_string()).into());
            if args.0 != Reg::G10 {
                returned_vec.push(Instruction::Addi(args.0, Reg::G10, 0).into());
            }

            returned_vec
        },
        IntermediateOp::Mul => {
            let mut returned_vec: Vec<Operation> = Vec::with_capacity(4);

            if args.2 != Reg::G10 {
                returned_vec.push(Instruction::Addi(Reg::G10, args.2, 0).into());
            }
            if args.4 != Reg::G11 {
                returned_vec.push(Instruction::Addi(Reg::G11, args.4, 0).into());
            }
            returned_vec.push(MacroInstr::Jal(Reg::G1, "_MUL".to_string()).into());
            if args.0 != Reg::G10 {
                returned_vec.push(Instruction::Addi(args.0, Reg::G10, 0).into());
            }

            returned_vec
        },

        IntermediateOp::Equal => Vec::from([Instruction::Equal(args.0, args.2, args.4).into()]),
        IntermediateOp::Xnor => Vec::from([Instruction::Xnor(args.0, args.2, args.4).into()]),
        IntermediateOp::Nor => todo!("Not implemented yet!"),

        op => panic!("[Error] Could not map parsed instruction to internal data structure: {:?}", op),
    };

    Ok((rest, instr))
}

fn parse_instruction(input: &str) -> IResult<&str, Vec<Operation>> {
    let (rest, op) = alt((
        parse_macro_noparm,
        parse_macro_1reg,
        parse_macro_1labl1reg,
        parse_macro_2reg,
        parse_macro_1labl2reg,
        parse_inst_1imm2reg_lw
    ))(input)?;

    Ok((rest, Vec::from([op])))
}

fn parse_multiline_macro(input: &str) -> IResult<&str, Vec<Operation>> {
    alt((
        parse_macro_1labl,
        parse_macro_1imm,
        parse_inst_1imm1reg,
        parse_inst_1imm2reg_up,
        parse_inst_3reg
    ))(input)
}

#[allow(clippy::type_complexity)]
fn parse_line(input: &str) -> IResult<&str, (Option<Cow<str>>, Option<Vec<Operation>>)> {
    let (rest, _) = multispace0(input)?;
    let (rest, label) = opt(parse_label_definition)(rest)?;
    if label.is_some() {
        let (rest, _) = multispace1(rest)?;
        let (rest, instr) = opt(alt((
            parse_instruction,
            parse_multiline_macro
        )))(rest)?;
        Ok((rest, (label, instr)))
    } else {
        let (rest, instr) = alt((
            parse_instruction,
            parse_multiline_macro
        ))(rest)?;
        Ok((rest, (label, Some(instr))))
    }
}

#[allow(clippy::type_complexity)]
fn parse_line_priv(input: &str) -> IResult<&str, (Option<Cow<str>>, Option<Vec<Operation>>)> {
    let (rest, _) = multispace0(input)?;
    let (rest, label) = opt(parse_label_definition_priv)(rest)?;
    if label.is_some() {
        let (rest, _) = multispace1(rest)?;
        let (rest, instr) = opt(alt((
            parse_instruction,
            parse_multiline_macro
        )))(rest)?;
        Ok((rest, (label, instr)))
    } else {
        let (rest, instr) = alt((
            parse_instruction,
            parse_multiline_macro
        ))(rest)?;
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

    if subroutines.is_none() {
        privileged = true;
    }

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
            Err(_) => todo!("Custom parser error!"),
        };

        match &mut parsed {
            (Some(label), Some(instr)) => {
                match label.strip_prefix('.') {
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

                for (local_counter, operation) in instr.iter_mut().enumerate() {
                    match operation {
                        Operation::Macro(macro_in) => {
                            match macro_in {
                                /*
                                MacroInstr::Muln(_, _, _) => subroutines.as_mut().unwrap().mul_defined(),
                                MacroInstr::Divn(_, _, _) => subroutines.as_mut().unwrap().div_defined(),
                                MacroInstr::Srr(_, _, _) => subroutines.as_mut().unwrap().srr_defined(),
                                MacroInstr::Slr(_, _, _) => subroutines.as_mut().unwrap().slr_defined(),*/
                                MacroInstr::Beq(_, _, labl) | 
                                MacroInstr::Bne(_, _, labl) |
                                MacroInstr::Blt(_, _, labl) |
                                MacroInstr::Bltu(_, _, labl) |
                                MacroInstr::Bge(_, _, labl) |
                                MacroInstr::Bgeu(_, _, labl) |
    
                                MacroInstr::Jal(_, labl) |
                                MacroInstr::Jalr(_, _, labl) |
    
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
                                    let mut scope = false;
                                    if labl.starts_with('_') {
                                        scope = true;
                                        if let Some(subs) = subroutines {
                                            match labl.as_str() {
                                                "_MUL" => subs.mul_defined(),
                                                "_DIV" => subs.div_defined(),
                                                "_SRR" => subs.srr_defined(),
                                                "_SLR" => subs.slr_defined(),
                                                unknown => println!("[Warning] Label does not point to subroutine: {}", unknown),
                                            }
                                        }
                                    }
                                    if !symbol_map.crt_or_ref_label(labl, scope) {
                                        local_ref_not_def.insert(labl.clone());
                                    }
                                },
                            }
                            if local_counter == 0 {
                                *operation = Operation::LablMacro(label.clone(), macro_in.to_owned());
                            }
                        },
                        Operation::Instr(instr_in) => {
                            if local_counter == 0 {
                                *operation = Operation::LablInstr(label.clone(), instr_in.to_owned());
                            }
                        }
                        _ => (),
                    }
                }

                instr_counter += instr.len();
                instr_list.append(instr);
            },
            (None, Some(instr)) => {
                for operation in instr.iter() {
                    if let Operation::Macro(macro_in) = operation {
                        match macro_in {
                            MacroInstr::Beq(_, _, labl) | 
                            MacroInstr::Bne(_, _, labl) |
                            MacroInstr::Blt(_, _, labl) |
                            MacroInstr::Bltu(_, _, labl) |
                            MacroInstr::Bge(_, _, labl) |
                            MacroInstr::Bgeu(_, _, labl) |

                            MacroInstr::Jal(_, labl) |
                            MacroInstr::Jalr(_, _, labl) |

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
                                let mut scope = false;
                                if labl.starts_with('_') {
                                    scope = true;
                                    if let Some(subs) = subroutines {
                                        match labl.as_str() {
                                            "_MUL" => subs.mul_defined(),
                                            "_DIV" => subs.div_defined(),
                                            "_SRR" => subs.srr_defined(),
                                            "_SLR" => subs.slr_defined(),
                                            unknown => println!("[Warning] Label does not point to subroutine: {}", unknown),
                                        }
                                    }
                                }
                                if !symbol_map.crt_or_ref_label(labl, scope) {
                                    local_ref_not_def.insert(labl.clone());
                                }
                            },
                        }
                    }
                }

                instr_counter += instr.len();
                instr_list.append(instr);
            },
            (Some(label), None) => {
                match label.strip_prefix('.') {
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
                instr_counter += 1;
                instr_list.push(Operation::Labl(label.clone()));
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
        assert_ne!(parse_macro_noparm("invalid"), Ok(("", Operation::Instr(Instruction::NA))));
        assert_ne!(parse_macro_noparm("noop"), Ok(("", Operation::Instr(Instruction::Addi(Reg::G0, Reg::G0, 0)))));
        assert_eq!(parse_macro_noparm("nop"), Ok(("", Operation::Instr(Instruction::Addi(Reg::G0, Reg::G0, 0)))));
        assert_ne!(parse_macro_noparm("nop x1"), Ok(("", Operation::Instr(Instruction::Addi(Reg::G0, Reg::G0, 0)))));
        assert_eq!(parse_macro_noparm("ret"), Ok(("", Operation::Instr(Instruction::Jalr(Reg::G0, Reg::G1, 0)))));
        assert_ne!(parse_macro_noparm("ret nop"), Ok(("", Operation::Instr(Instruction::Jalr(Reg::G0, Reg::G1, 0)))));
    }

    #[test]
    fn test_parse_instr1labl() {
        assert_ne!(parse_macro_1labl("invalid"), Ok(("", Vec::from([Instruction::NA.into()]))));
        assert_ne!(parse_macro_1labl(" "), Ok(("", Vec::from([Instruction::NA.into()]))));
        assert_ne!(parse_macro_1labl(""), Ok(("", Vec::from([Instruction::NA.into()]))));
        assert_ne!(parse_macro_1labl("call"), Ok(("", Vec::from([Instruction::NA.into()]))));
        assert_eq!(parse_macro_1labl("tail test"), Ok(("", Vec::from([
            MacroInstr::Auipc(Reg::G6, "test".to_string()).into(),
            MacroInstr::Jalr(Reg::G0, Reg::G6, "test".to_string()).into()
        ]))));
        assert_eq!(parse_macro_1labl("call HANS"), Ok(("", Vec::from([
            MacroInstr::Auipc(Reg::G1, "HANS".to_string()).into(),
            MacroInstr::Jalr(Reg::G1, Reg::G1, "HANS".to_string()).into()
        ]))));
        assert_ne!(parse_macro_1labl("call label  "), Ok(("", Vec::from([
            MacroInstr::Auipc(Reg::G1, "label".to_string()).into(),
            MacroInstr::Jalr(Reg::G1, Reg::G1, "label".to_string()).into()
        ]))));
    }

    #[test]
    fn test_parse_instr1imm() {
        assert_ne!(parse_macro_1imm("invalid"), Ok(("", Vec::from([Instruction::NA.into()]))));
        assert_ne!(parse_macro_1imm(" "), Ok(("", Vec::from([Instruction::NA.into()]))));
        assert_ne!(parse_macro_1imm(""), Ok(("", Vec::from([Instruction::NA.into()]))));
        assert_ne!(parse_macro_1imm("j"), Ok(("", Vec::from([Instruction::NA.into()]))));
        assert_eq!(parse_macro_1imm("j 12"), Ok(("", Vec::from([Instruction::Jal(Reg::G0, 12).into()]))));
        assert_eq!(parse_macro_1imm("call 0x10"), Ok(("", Vec::from([
            Instruction::Auipc(Reg::G1, 0).into(),
            Instruction::Jalr(Reg::G1, Reg::G1, 0x10).into()
        ]))));
        assert_ne!(parse_macro_1imm("jal 125  "), Ok(("", Vec::from([Instruction::Jal(Reg::G1, 125).into()]))));
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
        assert_eq!(parse_macro_1labl1reg("auipc s2, helloWorld"), Ok(("", MacroInstr::Auipc(Reg::G18, "helloWorld".to_string()).into())));
        assert_eq!(parse_macro_1labl1reg("jal x20, test"), Ok(("", MacroInstr::Jal(Reg::G20, "test".to_string()).into())));
        assert_ne!(parse_macro_1labl1reg("jal x19, train "), Ok(("", MacroInstr::Jal(Reg::G19, "train".to_string()).into())));
    }

    #[test]
    fn test_parse_instr1imm1reg() {
        assert_ne!(parse_inst_1imm1reg("invalid"), Ok(("", Vec::from([Instruction::NA.into()]))));
        assert_ne!(parse_inst_1imm1reg(" "), Ok(("", Vec::from([Instruction::NA.into()]))));
        assert_ne!(parse_inst_1imm1reg(""), Ok(("", Vec::from([Instruction::NA.into()]))));
        assert_ne!(parse_inst_1imm1reg("ld"), Ok(("", Vec::from([Instruction::NA.into()]))));
        assert_ne!(parse_inst_1imm1reg(""), Ok(("", Vec::from([Instruction::NA.into()]))));
        assert_ne!(parse_inst_1imm1reg("lui"), Ok(("", Vec::from([Instruction::Lui(Reg::NA, 0).into()]))));
        assert_eq!(parse_inst_1imm1reg("lui x12, 12"), Ok(("", Vec::from([Instruction::Lui(Reg::G12, 12).into()]))));
        assert_eq!(parse_inst_1imm1reg("auipc x18, 0x20"), Ok(("", Vec::from([Instruction::Auipc(Reg::G18, 32).into()]))));
        assert_eq!(parse_inst_1imm1reg("jal x20, 5"), Ok(("", Vec::from([Instruction::Jal(Reg::G20, 5).into()]))));
        assert_ne!(parse_inst_1imm1reg("jal x19, 125 "), Ok(("", Vec::from([Instruction::Jal(Reg::G19, 125).into()]))));
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
        assert_ne!(parse_macro_1labl2reg("sb x1, x6"), Ok(("", MacroInstr::Sb(Reg::G1, Reg::G6, "".to_string()).into())));
        assert_ne!(parse_macro_1labl2reg("lb x1, total"), Ok(("", MacroInstr::Lb(Reg::G1, Reg::NA, "total".to_string()).into())));
        assert_eq!(parse_macro_1labl2reg("bgeu x1, x4, sTaRt"), Ok(("", MacroInstr::Bgeu(Reg::G1, Reg::G4, "sTaRt".to_string()).into())));
        assert_ne!(parse_macro_1labl2reg("slli x1x4,eNND"), Ok(("", MacroInstr::Slli(Reg::G1, Reg::G4, "eNND".to_string()).into())));
        assert_eq!(parse_macro_1labl2reg("blt x10,x10, last"), Ok(("", MacroInstr::Blt(Reg::G10, Reg::G10, "last".to_string()).into())));
        assert_ne!(parse_macro_1labl2reg("jalr x6,  x8,test"), Ok(("", MacroInstr::Jalr(Reg::G6, Reg::G8, "test".to_string()).into())));
        assert_eq!(parse_macro_1labl2reg("lhu x1, x2, hans"), Ok(("", MacroInstr::Lhu(Reg::G1, Reg::G2, "hans".to_string()).into())));
        assert_eq!(parse_macro_1labl2reg("sb x13,x15,loading"), Ok(("", MacroInstr::Sb(Reg::G13, Reg::G15, "loading".to_string()).into())));
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

        assert_eq!(parse_inst_1imm2reg_up("addi x1, x2, 0xAA"), Ok(("", Vec::from([Instruction::Addi(Reg::G1, Reg::G2, 0xAA).into()]))));
        assert_eq!(parse_inst_1imm2reg_up("srr x13,x15,6"), Ok(("", Vec::from([
            Instruction::Addi(Reg::G10, Reg::G15, 0).into(),
            Instruction::Addi(Reg::G11, Reg::G0, 6).into(),
            MacroInstr::Jal(Reg::G1, "_SRR".to_string()).into(),
            Instruction::Addi(Reg::G13, Reg::G10, 0).into()
        ]))));
        assert_ne!(parse_inst_1imm2reg_up("sltix1x15,6"), Ok(("", Vec::from([Instruction::Slti(Reg::G1, Reg::G15, 6).into()]))));
        assert_ne!(parse_inst_1imm2reg_up("slli x12, x15,  6"), Ok(("", Vec::from([Instruction::Slli(Reg::G12, Reg::G15, 6).into()]))));
        // TODO: More tests
    }

    #[test]
    fn test_parse_inst_3reg() {
        assert_ne!(parse_inst_3reg("invalid"), Ok(("", Vec::from([Instruction::NA.into()]))));
        assert_ne!(parse_inst_3reg("   "), Ok(("", Vec::from([Instruction::NA.into()]))));
        assert_ne!(parse_inst_3reg("addi x1, x6, 0xAA"), Ok(("", Vec::from([Instruction::Addi(Reg::G1, Reg::G6, 0xAA).into()]))));
        assert_ne!(parse_inst_3reg("add x1, x2"), Ok(("", Vec::from([Instruction::Addn(Reg::G1, Reg::G2, Reg::NA).into()]))));
        assert_eq!(parse_inst_3reg("mul x1, x4, x6"), Ok(("", Vec::from([
            Instruction::Addi(Reg::G10, Reg::G4, 0).into(),
            Instruction::Addi(Reg::G11, Reg::G6, 0).into(),
            MacroInstr::Jal(Reg::G1, "_MUL".to_string()).into(),
            Instruction::Addi(Reg::G1, Reg::G10, 0).into()
        ]))));
        assert_ne!(parse_inst_3reg("div x10x14,x7"), Ok(("", Vec::from([
            Instruction::Addi(Reg::G10, Reg::G14, 0).into(),
            Instruction::Addi(Reg::G11, Reg::G7, 0).into(),
            MacroInstr::Jal(Reg::G1, "_DIV".to_string()).into()
        ]))));
        assert_eq!(parse_inst_3reg("xor x10,x11, x10"), Ok(("", Vec::from([Instruction::Xor(Reg::G10, Reg::G11, Reg::G10).into()]))));
        assert_ne!(parse_inst_3reg("xnor x6,  x8,x5"), Ok(("", Vec::from([
            Instruction::NA.into()
        ]))));
        assert_eq!(parse_inst_3reg("and x6, x8, x14"), Ok(("", Vec::from([Instruction::And(Reg::G6, Reg::G8, Reg::G14).into()]))));
        assert_ne!(parse_inst_3reg("sll x6,  x8, x14"), Ok(("", Vec::from([Instruction::Sll(Reg::G6, Reg::G8, Reg::G14).into()]))));
    }

    #[test]
    fn test_parse_instruction() {
        assert_eq!(parse_instruction("mv x1, x6"), Ok(("", Vec::from([Instruction::Addi(Reg::G1, Reg::G6, 0).into()]))));
        assert_ne!(parse_instruction("addi x1, 0xAA"), Ok(("", Vec::from([Instruction::Addi(Reg::G1, Reg::NA, 0xAA).into()]))));
        assert_eq!(parse_multiline_macro("mul x1, x4, x6"), Ok(("", Vec::from([
            Instruction::Addi(Reg::G10, Reg::G4, 0).into(),
            Instruction::Addi(Reg::G11, Reg::G6, 0).into(),
            MacroInstr::Jal(Reg::G1, "_MUL".to_string()).into(),
            Instruction::Addi(Reg::G1, Reg::G10, 0).into()
        ]))));
        assert_ne!(parse_instruction("xor x10x14,x7"), Ok(("", Vec::from([Instruction::Xor(Reg::G10, Reg::G14, Reg::G7).into()]))));
        assert_eq!(parse_multiline_macro("add x10,x11, x10"), Ok(("", Vec::from([Instruction::Addn(Reg::G10, Reg::G11, Reg::G10).into()]))));
        assert_ne!(parse_multiline_macro("xnor x6,  x8,x5"), Ok(("", Vec::from([
            Instruction::NA.into()
        ]))));
        assert_eq!(parse_multiline_macro("srr x5, x8, 7"), Ok(("", Vec::from([
            Instruction::Addi(Reg::G10, Reg::G8, 0).into(),
            Instruction::Addi(Reg::G11, Reg::G0, 7).into(),
            MacroInstr::Jal(Reg::G1, "_SRR".to_string()).into(),
            Instruction::Addi(Reg::G5, Reg::G10, 0).into()
        ]))));
        // More tests & seperate
    }

    #[test]
    fn test_parse_line() {
        assert_eq!(parse_line("label: add x1, x5, x6"),
                   Ok(("", (Some(Cow::from("label")), Some(Vec::from([Instruction::Addn(Reg::G1, Reg::G5, Reg::G6).into()]))))));
        assert_eq!(parse_line("\ntest:\n\nsub x6, x5, x11"),
                   Ok(("", (Some(Cow::from("test")), Some(Vec::from([Instruction::Subn(Reg::G6, Reg::G5, Reg::G11).into()]))))));
        assert_eq!(parse_line("\n\n\nreturn:\n"),
                   Ok(("", (Some(Cow::from("return")), None))));
        assert_eq!(parse_line("mv x15, x12\naddi x12, x10, 0x05"),
                   Ok(("\naddi x12, x10, 0x05", (None, Some(Vec::from([Instruction::Addi(Reg::G15, Reg::G12, 0).into()]))))));
        assert_eq!(parse_line("label:\ndiv x14, x13, x10"),
                   Ok(("", (Some(Cow::from("label")), Some(Vec::from([
                    Instruction::Addi(Reg::G10, Reg::G13, 0).into(),
                    Instruction::Addi(Reg::G11, Reg::G10, 0).into(),
                    MacroInstr::Jal(Reg::G1, "_DIV".to_string()).into(),
                    Instruction::Addi(Reg::G14, Reg::G10, 0).into()
                ]))))));
    }

    #[test]
    fn test_parse_line_privileged() {
        assert_eq!(parse_line_priv("_label: add x1, x5, x6"),
                    Ok(("", (Some(Cow::from("_label")), Some(Vec::from([Instruction::Addn(Reg::G1, Reg::G5, Reg::G6).into()]))))));
        assert_eq!(parse_line_priv("\n_test:\n\nsub x6, x5, x11"),
                    Ok(("", (Some(Cow::from("_test")), Some(Vec::from([Instruction::Subn(Reg::G6, Reg::G5, Reg::G11).into()]))))));
        assert_eq!(parse_line_priv("\n\n\n_return:\n"),
                    Ok(("", (Some(Cow::from("_return")), None))));
        assert_eq!(parse_line_priv("mv x15, x12\naddi x12, x10, 0x05"),
                    Ok(("\naddi x12, x10, 0x05", (None, Some(Vec::from([Instruction::Addi(Reg::G15, Reg::G12, 0).into()]))))));
        assert_eq!(parse_line_priv("_label:\ndiv x14, x13, x10"),
                    Ok(("", (Some(Cow::from("_label")), Some(Vec::from([
                        Instruction::Addi(Reg::G10, Reg::G13, 0).into(),
                        Instruction::Addi(Reg::G11, Reg::G10, 0).into(),
                        MacroInstr::Jal(Reg::G1, "_DIV".to_string()).into(),
                        Instruction::Addi(Reg::G14, Reg::G10, 0).into()
        ]))))));
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
