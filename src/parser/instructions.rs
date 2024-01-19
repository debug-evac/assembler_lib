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
        tag
    },
    branch::alt,
    combinator::{
        opt,
        value,
        recognize
    },
    sequence::{
        tuple,
        separated_pair,
        pair
    }, multi::separated_list1,
};

use crate::parser::literals::{parse_imm, parse_reg, parse_label_name};
use crate::common::{MacroInstr, Instruction, Operation, Reg, Part};

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
    Equal,
    Push,
    Pop,
    La
}

/*
TODO: Incorporate
impl IntermediateOp {
    fn translate_parse(self, input: &str) -> IResult<&str, Operation> {
        let (rest, _) = parse_instr_args_seper(input)?;
        match self {
            IntermediateOp::Call => {
                let (rest, args) = alt((
                    pair(
                        success(None),
                        map(parse_label_name, Some)
                    ),
                    pair(
                        map(parse_imm, Some),
                        success(None)
                    )
                ))(rest)?;

                match args {
                    (Some(imm), None) => Ok((rest, MacroInstr::CallImm(imm).into())),
                    (None, Some(labl)) => Ok((rest, MacroInstr::CallLabl(labl.to_string()).into())),
                    _ => unreachable!(),
                }
            },
            IntermediateOp::Tail => todo!(),
            IntermediateOp::Jal => todo!(),
            IntermediateOp::J => todo!(),
            IntermediateOp::Jr => todo!(),
            IntermediateOp::Jalr => todo!(),
            IntermediateOp::Lui => todo!(),
            IntermediateOp::Auipc => todo!(),
            IntermediateOp::Li => todo!(),
            IntermediateOp::Mv => todo!(),
            IntermediateOp::Addi => todo!(),
            IntermediateOp::Slti => todo!(),
            IntermediateOp::Sltiu => todo!(),
            IntermediateOp::Xori => todo!(),
            IntermediateOp::Ori => todo!(),
            IntermediateOp::Andi => todo!(),
            IntermediateOp::Srr => todo!(),
            IntermediateOp::Slr => todo!(),
            IntermediateOp::Add => todo!(),
            IntermediateOp::Sub => todo!(),
            IntermediateOp::Xor => todo!(),
            IntermediateOp::Or => todo!(),
            IntermediateOp::And => todo!(),
            IntermediateOp::Slt => todo!(),
            IntermediateOp::Sltu => todo!(),
            IntermediateOp::Sll => todo!(),
            IntermediateOp::Srl => todo!(),
            IntermediateOp::Sra => todo!(),
            IntermediateOp::Div => todo!(),
            IntermediateOp::Mul => todo!(),
            IntermediateOp::Xnor => todo!(),
            IntermediateOp::Nor => todo!(),
            IntermediateOp::Equal => todo!(),
            IntermediateOp::Push => todo!(),
            IntermediateOp::Pop => todo!(),
            IntermediateOp::La => todo!(),
        }
    }
}
*/

fn parse_seper(input: &str) -> IResult<&str, &str> {
    recognize(
        pair(nom::character::complete::char(','), 
        opt(nom::character::complete::char(' ')))
    )(input)
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

/*
TODO: Incorporate
fn parse_macro_TEST(input: &str) -> IResult<&str, Operation> {
    let (rest, inter) = alt((
        value(IntermediateOp::Call, tag("call")),
        value(IntermediateOp::Tail, tag("tail")),
        value(IntermediateOp::Jal, tag("jal")),
        value(IntermediateOp::J, tag("j")),
    ))(input)?;

    inter.translate_parse(rest)
}
*/

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

        value(MacroInstr::Addi(Reg::NA, Reg::NA, String::new(), Part::None), tag("addi")),
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

        MacroInstr::Addi(_, _, _, ir) => MacroInstr::Addi(args.0, args.2, args.4.to_string(), ir),

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

pub fn parse_instruction(input: &str) -> IResult<&str, Operation> {
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_seper() {
        assert_ne!(parse_seper("invalid"), Ok(("", "")));
        assert_ne!(parse_seper(" "), Ok(("", "")));
        assert_eq!(parse_seper(", "), Ok(("", ", ")));
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
}