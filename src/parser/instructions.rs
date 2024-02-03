/*
 * Copyright (c) 2023 Steven Becker
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

use nom::{
    IResult,
    bytes::complete::tag,
    character::complete::{space1, digit1},
    branch::alt,
    combinator::{
        opt,
        value,
        recognize,
        map_res
    },
    sequence::{
        tuple,
        separated_pair,
        pair,
        preceded
    }, multi::separated_list1,
};

use crate::parser::literals::{parse_imm, parse_reg, parse_label_name};
use crate::common::{MacroInstr, Instruction, Operation, Reg, Part};

#[derive(Clone)]
enum InstrType {
    Labl(IntermediateOp),
    Imm(IntermediateOp),
    Reg(IntermediateOp),
    RegLabl(IntermediateOp),
    RegImm(IntermediateOp),
    Reg2(IntermediateOp),
    Reg2Labl(IntermediateOp),
    Reg2Imm(IntermediateOp),
    Reg3(IntermediateOp),
    RegVar(IntermediateOp)
}

impl InstrType {
    fn translate_parse(self, input: &str) -> IResult<&str, Operation> {
        let (rest, _) = space1(input)?;
        match self {
            InstrType::Labl(interop) => {
                let (rest, labl) = parse_label_name(rest)?;

                Ok((rest, match interop {
                    IntermediateOp::Call => MacroInstr::CallLabl(labl.to_string()).into(),
                    IntermediateOp::Tail => MacroInstr::TailLabl(labl.to_string()).into(),
                    IntermediateOp::J => MacroInstr::Jal(Reg::G0, labl.to_string()).into(),
                    IntermediateOp::Jal => MacroInstr::Jal(Reg::G1, labl.to_string()).into(),

                    op => panic!("[Error] Could not map parsed instruction to internal data structure: {:?}", op),
                }))
            },
            InstrType::Imm(interop) => {
                let (rest, imm) = parse_imm(rest)?;
                
                Ok((rest, match interop {
                    IntermediateOp::Call => MacroInstr::CallImm(imm).into(),
                    IntermediateOp::Tail => MacroInstr::TailImm(imm).into(),
                    IntermediateOp::J => Instruction::Jal(Reg::G0, imm).into(),
                    IntermediateOp::Jal => Instruction::Jal(Reg::G1, imm).into(),

                    op => panic!("[Error] Could not map parsed instruction to internal data structure: {:?}", op),
                }))
            },
            InstrType::Reg(interop) => {
                let (rest, reg) = parse_reg(rest)?;

                Ok((rest, match interop {
                    IntermediateOp::Jr => Instruction::Jalr(Reg::G0, reg, 0).into(),
                    IntermediateOp::Jalr => Instruction::Jalr(Reg::G1, reg, 0).into(),

                    op => panic!("[Error] Could not map parsed instruction to internal data structure: {:?}", op),
                }))
            },
            InstrType::RegLabl(interop) => {
                let (rest, args) = separated_pair(parse_reg, parse_seper, parse_label_name)(rest)?;

                Ok((rest, match interop {
                    IntermediateOp::Lui => MacroInstr::Lui(args.0, args.1.to_string()).into(),
                    IntermediateOp::Auipc => MacroInstr::Auipc(args.0, args.1.to_string(), Part::None).into(),
                    IntermediateOp::Jal => MacroInstr::Jal(args.0, args.1.to_string()).into(),
                    IntermediateOp::La => MacroInstr::LaLabl(args.0, args.1.to_string()).into(),

                    op => panic!("[Error] Could not map parsed instruction to internal data structure: {:?}", op),
                }))
            },
            InstrType::RegImm(interop) => {
                let (rest, args) = separated_pair(parse_reg, parse_seper, parse_imm)(rest)?;

                Ok((rest, match interop {
                    IntermediateOp::Lui => Instruction::Lui(args.0, args.1).into(),
                    IntermediateOp::Auipc => Instruction::Auipc(args.0, args.1).into(),
                    IntermediateOp::Jal => Instruction::Jal(args.0, args.1).into(),
            
                    IntermediateOp::Li => MacroInstr::Li(args.0, args.1).into(),
                    IntermediateOp::La => MacroInstr::LaImm(args.0, args.1).into(),
            
                    op => panic!("[Error] Could not map parsed instruction to internal data structure: {:?}", op),
                }))
            },
            InstrType::Reg2(interop) => {
                let (rest, args) = separated_pair(parse_reg, parse_seper, parse_reg)(rest)?;

                Ok((rest, match interop {
                    IntermediateOp::Mv => Instruction::Addi(args.0, args.1, 0).into(),
                    op  => panic!("[Error] Could not map parsed instruction to internal data structure: {:?}", op),
                }))
            },
            InstrType::Reg2Labl(interop) => {
                let (rest, args) = tuple((
                    parse_reg,
                    preceded(parse_seper, parse_reg),
                    preceded(parse_seper, parse_label_name)))
                (rest)?;

                Ok((rest, match interop {
                    IntermediateOp::Beq => MacroInstr::Beq(args.0, args.1, args.2.to_string()).into(),
                    IntermediateOp::Bne => MacroInstr::Bne(args.0, args.1, args.2.to_string()).into(),
                    IntermediateOp::Blt => MacroInstr::Blt(args.0, args.1, args.2.to_string()).into(),
                    IntermediateOp::Bltu => MacroInstr::Bltu(args.0, args.1, args.2.to_string()).into(),
                    IntermediateOp::Bge => MacroInstr::Bge(args.0, args.1, args.2.to_string()).into(),
                    IntermediateOp::Bgeu => MacroInstr::Bgeu(args.0, args.1, args.2.to_string()).into(),
            
                    IntermediateOp::Jalr => MacroInstr::Jalr(args.0, args.1, args.2.to_string(), Part::None).into(),
            
                    IntermediateOp::Slli => MacroInstr::Slli(args.0, args.1, args.2.to_string()).into(),
                    IntermediateOp::Srli => MacroInstr::Srli(args.0, args.1, args.2.to_string()).into(),
                    IntermediateOp::Srai => MacroInstr::Srai(args.0, args.1, args.2.to_string()).into(),
            
                    IntermediateOp::Sb => MacroInstr::Sb(args.0, args.1, args.2.to_string(), Part::None).into(),
                    IntermediateOp::Sh => MacroInstr::Sh(args.0, args.1, args.2.to_string(), Part::None).into(),
                    IntermediateOp::Sw => MacroInstr::Sw(args.0, args.1, args.2.to_string(), Part::None).into(),
            
                    IntermediateOp::Lb => MacroInstr::Lb(args.0, args.1, args.2.to_string(), Part::None).into(),
                    IntermediateOp::Lbu => MacroInstr::Lbu(args.0, args.1, args.2.to_string()).into(),
                    IntermediateOp::Lh => MacroInstr::Lh(args.0, args.1, args.2.to_string(), Part::None).into(),
                    IntermediateOp::Lhu => MacroInstr::Lhu(args.0, args.1, args.2.to_string()).into(),
                    IntermediateOp::Lw => MacroInstr::Lw(args.0, args.1, args.2.to_string(), Part::None).into(),
            
                    IntermediateOp::Addi => MacroInstr::Addi(args.0, args.1, args.2.to_string(), Part::None).into(),
            
                    op => panic!("[Error] Could not map parsed instruction to internal data structure: {:?}", op),
                }))
            },
            InstrType::Reg2Imm(interop) => {
                let (rest, args) = tuple((
                    parse_reg,
                    preceded(parse_seper, parse_reg),
                    preceded(parse_seper, parse_imm)))
                (rest)?;

                Ok((rest, match interop {
                    IntermediateOp::Beq => Instruction::Beq(args.0, args.1, args.2).into(),
                    IntermediateOp::Bne => Instruction::Bne(args.0, args.1, args.2).into(),
                    IntermediateOp::Blt => Instruction::Blt(args.0, args.1, args.2).into(),
                    IntermediateOp::Bltu => Instruction::Bltu(args.0, args.1, args.2).into(),
                    IntermediateOp::Bge => Instruction::Bge(args.0, args.1, args.2).into(),
                    IntermediateOp::Bgeu => Instruction::Bgeu(args.0, args.1, args.2).into(),
            
                    IntermediateOp::Slli => Instruction::Slli(args.0, args.1, args.2).into(),
                    IntermediateOp::Srli => Instruction::Srli(args.0, args.1, args.2).into(),
                    IntermediateOp::Srai => Instruction::Srai(args.0, args.1, args.2).into(),
            
                    IntermediateOp::Sb => Instruction::Sb(args.0, args.1, args.2).into(),
                    IntermediateOp::Sh => Instruction::Sh(args.0, args.1, args.2).into(),
                    IntermediateOp::Sw => Instruction::Sw(args.0, args.1, args.2).into(),
            
                    IntermediateOp::Lb => Instruction::Lb(args.0, args.1, args.2).into(),
                    IntermediateOp::Lbu => Instruction::Lbu(args.0, args.1, args.2).into(),
                    IntermediateOp::Lh => Instruction::Lh(args.0, args.1, args.2).into(),
                    IntermediateOp::Lhu => Instruction::Lhu(args.0, args.1, args.2).into(),
                    IntermediateOp::Lw => Instruction::Lw(args.0, args.1, args.2).into(),
            
                    IntermediateOp::Addi => Instruction::Addi(args.0, args.1, args.2).into(),

                    IntermediateOp::Slti => Instruction::Slti(args.0, args.1, args.2).into(),
                    IntermediateOp::Sltiu => Instruction::Sltiu(args.0, args.1, args.2).into(),
                    IntermediateOp::Xori => Instruction::Xori(args.0, args.1, args.2).into(),
                    IntermediateOp::Ori => Instruction::Ori(args.0, args.1, args.2).into(),
                    IntermediateOp::Andi => Instruction::Andi(args.0, args.1, args.2).into(),

                    IntermediateOp::Jalr => Instruction::Jalr(args.0, args.1, args.2).into(),

                    IntermediateOp::Srr => MacroInstr::Srr(args.0, args.1, args.2).into(),
                    IntermediateOp::Slr => MacroInstr::Slr(args.0, args.1, args.2).into(),

                    op => panic!("[Error] Could not map parsed instruction to internal data structure: {:?}", op),
                }))
            },
            InstrType::Reg3(interop) => {
                let (rest, args) = tuple((
                    parse_reg,
                    preceded(parse_seper, parse_reg),
                    preceded(parse_seper, parse_reg)))
                (rest)?;

                Ok((rest, match interop {
                    IntermediateOp::Add => Instruction::Add(args.0, args.1, args.2).into(),
                    IntermediateOp::Sub => Instruction::Sub(args.0, args.1, args.2).into(),
            
                    IntermediateOp::Xor => Instruction::Xor(args.0, args.1, args.2).into(),
                    IntermediateOp::Or => Instruction::Or(args.0, args.1, args.2).into(),
                    IntermediateOp::And => Instruction::And(args.0, args.1, args.2).into(),
            
                    IntermediateOp::Slt => Instruction::Slt(args.0, args.1, args.2).into(),
                    IntermediateOp::Sltu => Instruction::Sltu(args.0, args.1, args.2).into(),
            
                    IntermediateOp::Sll => Instruction::Sll(args.0, args.1, args.2).into(),
                    IntermediateOp::Srl => Instruction::Srl(args.0, args.1, args.2).into(),
                    IntermediateOp::Sra => Instruction::Sra(args.0, args.1, args.2).into(),
            
                    IntermediateOp::Div => MacroInstr::Div(args.0, args.1, args.2).into(),
                    IntermediateOp::Mul => Instruction::Mul(args.0, args.1, args.2).into(),
                    IntermediateOp::Mulh => Instruction::Mulh(args.0, args.1, args.2).into(),
                    IntermediateOp::Mulhsu => Instruction::Mulhsu(args.0, args.1, args.2).into(),
                    IntermediateOp::Mulhu => Instruction::Mulhu(args.0, args.1, args.2).into(),
                    IntermediateOp::Remu => MacroInstr::Remu(args.0, args.1, args.2).into(),
            
                    IntermediateOp::Equal => Instruction::Equal(args.0, args.1, args.2).into(),
                    IntermediateOp::Xnor => Instruction::Xnor(args.0, args.1, args.2).into(),
                    IntermediateOp::Nor => todo!("Not implemented yet!"),
            
                    op => panic!("[Error] Could not map parsed instruction to internal data structure: {:?}", op),
                }))
            },
            InstrType::RegVar(interop) => {
                let (rest, args) = separated_list1(parse_seper, parse_reg)(rest)?;

                Ok((rest, match interop {
                    IntermediateOp::Push => MacroInstr::Push(args).into(),
                    IntermediateOp::Pop => MacroInstr::Pop(args).into(),

                    op => panic!("[Error] Could not map parsed instruction to internal data structure: {:?}", op),
                }))
            },
        }
    }
}

#[derive(Clone, Debug)]
enum IntermediateOp {
    Call, Tail,
    J, Jr, Jal, Jalr,
    Lui, Auipc,
    Li, La,
    Mv,
    Addi,
    Slti,
    Sltiu,
    Xori,
    Ori,
    Andi,
    Slli,
    Srli,
    Srai,
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
    Mul, Mulh, Mulhsu, Mulhu,
    Remu,
    Xnor,
    Nor,
    Equal,
    Push,
    Pop,
    Beq,
    Bne,
    Blt,
    Bltu,
    Bge,
    Bgeu,
    Sb, Sh, Sw,
    Lb, Lbu, Lh, Lhu, Lw
}

fn parse_seper(input: &str) -> IResult<&str, &str> {
    recognize(
        pair(nom::character::complete::char(','), 
        opt(nom::character::complete::char(' ')))
    )(input)
}

fn parse_macro_noparm(input: &str) -> IResult<&str, Operation> {
    let (rest, instr) = alt((
        value(Operation::Instr(Instruction::Addi(Reg::G0, Reg::G0, 0)), tag("nop")),
        value(Operation::Instr(Instruction::Jalr(Reg::G0, Reg::G1, 0)), tag("ret")),
    ))(input)?;

    Ok((rest, instr))
}

fn parse_macro_1labl(input: &str) -> IResult<&str, Operation> {
    let (rest, instr) = alt((
        value(InstrType::Labl(IntermediateOp::Call), tag("call")),
        value(InstrType::Labl(IntermediateOp::Tail), tag("tail")),

        value(InstrType::Labl(IntermediateOp::Jal), tag("jal")),
        value(InstrType::Labl(IntermediateOp::J), tag("j")),
    ))(input)?;
    instr.translate_parse(rest)
}

fn parse_macro_1imm(input: &str) -> IResult<&str, Operation> {
    let (rest, instr) = alt((
        value(InstrType::Imm(IntermediateOp::Call), tag("call")),
        value(InstrType::Imm(IntermediateOp::Tail), tag("tail")),

        value(InstrType::Imm(IntermediateOp::Jal), tag("jal")),
        value(InstrType::Imm(IntermediateOp::J), tag("j")),
    ))(input)?;
    instr.translate_parse(rest)
}

fn parse_macro_1reg(input: &str) -> IResult<&str, Operation> {
    let (rest, instr) = alt((
        value(InstrType::Reg(IntermediateOp::Jr), tag("jr")),
        value(InstrType::Reg(IntermediateOp::Jalr), tag("jalr")),
    ))(input)?;
    instr.translate_parse(rest)
}

fn parse_macro_1labl1reg(input: &str) -> IResult<&str, Operation> {
    let (rest, instr) = alt((
        value(InstrType::RegLabl(IntermediateOp::Lui), tag("lui")),
        value(InstrType::RegLabl(IntermediateOp::Auipc), tag("auipc")),
        value(InstrType::RegLabl(IntermediateOp::Jal), tag("jal")),
        value(InstrType::RegLabl(IntermediateOp::La), tag("la")),
    ))(input)?;
    instr.translate_parse(rest)
}

fn parse_inst_1imm1reg(input: &str) -> IResult<&str, Operation> {
    let (rest, instr) = alt((
        value(InstrType::RegImm(IntermediateOp::Lui), tag("lui")),
        value(InstrType::RegImm(IntermediateOp::Auipc), tag("auipc")),
        value(InstrType::RegImm(IntermediateOp::Jal), tag("jal")),

        value(InstrType::RegImm(IntermediateOp::Li), tag("li")),
        value(InstrType::RegImm(IntermediateOp::La), tag("la")),
    ))(input)?;
    instr.translate_parse(rest)
}

fn parse_macro_2reg(input: &str) -> IResult<&str, Operation> {
    let (rest, instr) = (
        value(InstrType::Reg2(IntermediateOp::Mv), tag("mv"))
    )(input)?;
    instr.translate_parse(rest)
}

fn parse_macro_1labl2reg(input: &str) -> IResult<&str, Operation> {
    let (rest, instr) = alt((
        value(InstrType::Reg2Labl(IntermediateOp::Beq), tag("beq")),
        value(InstrType::Reg2Labl(IntermediateOp::Bne), tag("bne")),
        value(InstrType::Reg2Labl(IntermediateOp::Bltu), tag("bltu")),
        value(InstrType::Reg2Labl(IntermediateOp::Bgeu), tag("bgeu")),
        value(InstrType::Reg2Labl(IntermediateOp::Blt), tag("blt")),
        value(InstrType::Reg2Labl(IntermediateOp::Bge), tag("bge")),

        value(InstrType::Reg2Labl(IntermediateOp::Jalr), tag("jalr")),

        value(InstrType::Reg2Labl(IntermediateOp::Slli), tag("slli")),
        value(InstrType::Reg2Labl(IntermediateOp::Srli), tag("srli")),
        value(InstrType::Reg2Labl(IntermediateOp::Srai), tag("srai")),

        value(InstrType::Reg2Labl(IntermediateOp::Sb), tag("sb")),
        value(InstrType::Reg2Labl(IntermediateOp::Sh), tag("sh")),
        value(InstrType::Reg2Labl(IntermediateOp::Sw), tag("sw")),

        value(InstrType::Reg2Labl(IntermediateOp::Lbu), tag("lbu")),
        value(InstrType::Reg2Labl(IntermediateOp::Lhu), tag("lhu")),
        value(InstrType::Reg2Labl(IntermediateOp::Lb), tag("lb")),
        value(InstrType::Reg2Labl(IntermediateOp::Lh), tag("lh")),
        value(InstrType::Reg2Labl(IntermediateOp::Lw), tag("lw")),

        value(InstrType::Reg2Labl(IntermediateOp::Addi), tag("addi")),
    ))(input)?;
    instr.translate_parse(rest)
}

fn parse_inst_1imm2reg_lw(input: &str) -> IResult<&str, Operation> {
    let (rest, instr) = alt((
        value(InstrType::Reg2Imm(IntermediateOp::Beq), tag("beq")),
        value(InstrType::Reg2Imm(IntermediateOp::Bne), tag("bne")),
        value(InstrType::Reg2Imm(IntermediateOp::Blt), tag("blt")),
        value(InstrType::Reg2Imm(IntermediateOp::Bltu), tag("bltu")),
        value(InstrType::Reg2Imm(IntermediateOp::Bge), tag("bge")),
        value(InstrType::Reg2Imm(IntermediateOp::Bgeu), tag("bgeu")),

        value(InstrType::Reg2Imm(IntermediateOp::Slli), tag("slli")),
        value(InstrType::Reg2Imm(IntermediateOp::Srli), tag("srli")),
        value(InstrType::Reg2Imm(IntermediateOp::Srai), tag("srai")),

        value(InstrType::Reg2Imm(IntermediateOp::Sb), tag("sb")),
        value(InstrType::Reg2Imm(IntermediateOp::Sh), tag("sh")),
        value(InstrType::Reg2Imm(IntermediateOp::Sw), tag("sw")),

        value(InstrType::Reg2Imm(IntermediateOp::Lbu), tag("lbu")),
        value(InstrType::Reg2Imm(IntermediateOp::Lhu), tag("lhu")),
        value(InstrType::Reg2Imm(IntermediateOp::Lb), tag("lb")),
        value(InstrType::Reg2Imm(IntermediateOp::Lh), tag("lh")),
        value(InstrType::Reg2Imm(IntermediateOp::Lw), tag("lw")),
    ))(input)?;
    instr.translate_parse(rest)
}

fn parse_inst_1imm2reg_up(input: &str) -> IResult<&str, Operation> {
    let (rest, instr) = alt((
        value(InstrType::Reg2Imm(IntermediateOp::Addi), tag("addi")),

        value(InstrType::Reg2Imm(IntermediateOp::Sltiu), tag("sltiu")),
        value(InstrType::Reg2Imm(IntermediateOp::Slti), tag("slti")),

        value(InstrType::Reg2Imm(IntermediateOp::Xori), tag("xori")),
        value(InstrType::Reg2Imm(IntermediateOp::Ori), tag("ori")),
        value(InstrType::Reg2Imm(IntermediateOp::Andi), tag("andi")),

        value(InstrType::Reg2Imm(IntermediateOp::Jalr), tag("jalr")),

        value(InstrType::Reg2Imm(IntermediateOp::Srr), tag("srr")),
        value(InstrType::Reg2Imm(IntermediateOp::Slr), tag("slr"))
    ))(input)?;
    instr.translate_parse(rest)
}

fn parse_inst_3reg(input: &str) -> IResult<&str, Operation> {
    let (rest, instr) = alt((
        value(InstrType::Reg3(IntermediateOp::Add), tag("add")),
        value(InstrType::Reg3(IntermediateOp::Sub), tag("sub")),

        value(InstrType::Reg3(IntermediateOp::Xor), tag("xor")),
        value(InstrType::Reg3(IntermediateOp::Or), tag("or")),
        value(InstrType::Reg3(IntermediateOp::And), tag("and")),

        value(InstrType::Reg3(IntermediateOp::Sltu), tag("sltu")),
        value(InstrType::Reg3(IntermediateOp::Slt), tag("slt")),

        value(InstrType::Reg3(IntermediateOp::Sll), tag("sll")),
        value(InstrType::Reg3(IntermediateOp::Srl), tag("srl")),
        value(InstrType::Reg3(IntermediateOp::Sra), tag("sra")),

        value(InstrType::Reg3(IntermediateOp::Div), tag("div")),
        value(InstrType::Reg3(IntermediateOp::Mul), tag("mul")),
        value(InstrType::Reg3(IntermediateOp::Mulhsu), tag("mulhsu")),
        value(InstrType::Reg3(IntermediateOp::Mulhu), tag("mulhu")),
        value(InstrType::Reg3(IntermediateOp::Mulh), tag("mulh")),
        value(InstrType::Reg3(IntermediateOp::Remu), tag("remu")),

        value(InstrType::Reg3(IntermediateOp::Xnor), tag("xnor")),
        value(InstrType::Reg3(IntermediateOp::Equal), tag("eq")),
        value(InstrType::Reg3(IntermediateOp::Nor), tag("nor")),
    ))(input)?;
    instr.translate_parse(rest)
}

fn parse_macro_multiarg(input: &str) -> IResult<&str, Operation> {
    let (rest, instr) = alt((
        value(InstrType::RegVar(IntermediateOp::Push), tag("push")),
        value(InstrType::RegVar(IntermediateOp::Pop), tag("pop")),
    ))(input)?;
    instr.translate_parse(rest)
}

fn parse_special_macro(input: &str) -> IResult<&str, Operation> {
    map_res(tuple((
        tag("rep"), 
        space1, 
        separated_pair(map_res(digit1, str::parse), parse_seper, parse_instruction)
    )),
    |parsed| {
        match parsed.2.1 {
            Operation::Namespace(_) |
            Operation::LablMacro(_, _) |
            Operation::LablInstr(_, _) |
            Operation::Labl(_) => unreachable!(),
            Operation::Instr(instr_in) => Ok(MacroInstr::RepInstr(parsed.2.0, instr_in).into()),
            Operation::Macro(macro_in) => {
                match macro_in {
                    MacroInstr::RepInstr(_, _) |
                    MacroInstr::RepMacro(_, _) => Err("[Error] Repeat Macro contains repeat macro!"),
                    op => Ok(MacroInstr::RepMacro(parsed.2.0, Box::new(op)).into()),
                }
            },
        }
    }
    )(input)
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
        parse_macro_multiarg,
        parse_special_macro
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
        assert_ne!(parse_macro_noparm("invalid"), Ok(("", Operation::Instr(Instruction::Addi(Reg::G0, Reg::G0, 0)))));
        assert_ne!(parse_macro_noparm("noop"), Ok(("", Operation::Instr(Instruction::Addi(Reg::G0, Reg::G0, 0)))));
        assert_eq!(parse_macro_noparm("nop"), Ok(("", Operation::Instr(Instruction::Addi(Reg::G0, Reg::G0, 0)))));
        assert_ne!(parse_macro_noparm("nop x1"), Ok(("", Operation::Instr(Instruction::Addi(Reg::G0, Reg::G0, 0)))));
        assert_eq!(parse_macro_noparm("ret"), Ok(("", Operation::Instr(Instruction::Jalr(Reg::G0, Reg::G1, 0)))));
        assert_ne!(parse_macro_noparm("ret nop"), Ok(("", Operation::Instr(Instruction::Jalr(Reg::G0, Reg::G1, 0)))));
    }

    #[test]
    fn test_parse_instr1labl() {
        assert_ne!(parse_macro_1labl("invalid"), Ok(("", Instruction::Addi(Reg::G0, Reg::G0, 0).into())));
        assert_ne!(parse_macro_1labl(" "), Ok(("", Instruction::Addi(Reg::G0, Reg::G0, 0).into())));
        assert_ne!(parse_macro_1labl(""), Ok(("", Instruction::Addi(Reg::G0, Reg::G0, 0).into())));
        assert_ne!(parse_macro_1labl("call"), Ok(("", MacroInstr::CallImm(0).into())));
        assert_eq!(parse_macro_1labl("tail test"), Ok(("", MacroInstr::TailLabl("test".to_string()).into())));
        assert_eq!(parse_macro_1labl("call HANS"), Ok(("", MacroInstr::CallLabl("HANS".to_string()).into())));
        assert_ne!(parse_macro_1labl("call label  "), Ok(("", MacroInstr::CallLabl("label".to_string()).into())));
    }

    #[test]
    fn test_parse_instr1imm() {
        assert_ne!(parse_macro_1imm("invalid"), Ok(("", Instruction::Addi(Reg::G0, Reg::G0, 0).into())));
        assert_ne!(parse_macro_1imm(" "), Ok(("", Instruction::Addi(Reg::G0, Reg::G0, 0).into())));
        assert_ne!(parse_macro_1imm(""), Ok(("", Instruction::Addi(Reg::G0, Reg::G0, 0).into())));
        assert_ne!(parse_macro_1imm("j"), Ok(("", Instruction::Jal(Reg::G0, 0).into())));
        assert_eq!(parse_macro_1imm("j 12"), Ok(("", Instruction::Jal(Reg::G0, 12).into())));
        assert_eq!(parse_macro_1imm("call 0x10"), Ok(("", MacroInstr::CallImm(0x10).into())));
        assert_ne!(parse_macro_1imm("jal 125  "), Ok(("", Instruction::Jal(Reg::G1, 125).into())));
    }

    #[test]
    fn test_parse_instr1reg() {
        assert_ne!(parse_macro_1reg("invalid"), Ok(("", Instruction::Addi(Reg::G0, Reg::G0, 0).into())));
        assert_ne!(parse_macro_1reg(" "), Ok(("", Instruction::Addi(Reg::G0, Reg::G0, 0).into())));
        assert_ne!(parse_macro_1reg(""), Ok(("", Instruction::Addi(Reg::G0, Reg::G0, 0).into())));
        assert_ne!(parse_macro_1reg("jr"), Ok(("", Instruction::Jalr(Reg::G0, Reg::G0, 0).into())));
        assert_eq!(parse_macro_1reg("jalr a2"), Ok(("", Instruction::Jalr(Reg::G1, Reg::str_to_enum("a2").unwrap(), 0).into())));
        assert_eq!(parse_macro_1reg("jr x18"), Ok(("", Instruction::Jalr(Reg::G0, Reg::G18, 0).into())));
        assert_ne!(parse_macro_1reg("jalr x19  "), Ok(("", Instruction::Jalr(Reg::G1, Reg::G19, 0).into())));
    }

    #[test]
    fn test_parse_instr1labl1reg() {
        assert_ne!(parse_macro_1labl1reg(""), Ok(("", Instruction::Addi(Reg::G0, Reg::G0, 0).into())));
        assert_ne!(parse_macro_1labl1reg("lui"), Ok(("", MacroInstr::Lui(Reg::G0, "".to_string()).into())));
        assert_eq!(parse_macro_1labl1reg("lui a2, stop"), Ok(("", MacroInstr::Lui(Reg::G12, "stop".to_string()).into())));
        assert_eq!(parse_macro_1labl1reg("auipc s2, helloWorld"), Ok(("", MacroInstr::Auipc(Reg::G18, "helloWorld".to_string(), Part::None).into())));
        assert_eq!(parse_macro_1labl1reg("jal   x20, test"), Ok(("", MacroInstr::Jal(Reg::G20, "test".to_string()).into())));
        assert_ne!(parse_macro_1labl1reg("jal x19, train "), Ok(("", MacroInstr::Jal(Reg::G19, "train".to_string()).into())));
        assert_eq!(parse_macro_1labl1reg("la x19, HELLOWORLD"), Ok(("", MacroInstr::LaLabl(Reg::G19, "HELLOWORLD".to_string()).into())));
    }

    #[test]
    fn test_parse_instr1imm1reg() {
        assert_ne!(parse_inst_1imm1reg("invalid"), Ok(("", Instruction::Addi(Reg::G0, Reg::G0, 0).into())));
        assert_ne!(parse_inst_1imm1reg(" "), Ok(("", Instruction::Addi(Reg::G0, Reg::G0, 0).into())));
        assert_ne!(parse_inst_1imm1reg(""), Ok(("", Instruction::Addi(Reg::G0, Reg::G0, 0).into())));
        assert_ne!(parse_inst_1imm1reg("lb"), Ok(("", Instruction::Lb(Reg::G0, Reg::G0, 0).into())));
        assert_ne!(parse_inst_1imm1reg(""), Ok(("", Instruction::Addi(Reg::G0, Reg::G0, 0).into())));
        assert_ne!(parse_inst_1imm1reg("lui"), Ok(("", Instruction::Lui(Reg::G0, 0).into())));
        assert_eq!(parse_inst_1imm1reg("lui x12, 12"), Ok(("", Instruction::Lui(Reg::G12, 12).into())));
        assert_eq!(parse_inst_1imm1reg("auipc x18, 0x20"), Ok(("", Instruction::Auipc(Reg::G18, 32).into())));
        assert_eq!(parse_inst_1imm1reg("jal x20, 5"), Ok(("", Instruction::Jal(Reg::G20, 5).into())));
        assert_ne!(parse_inst_1imm1reg("jal x19, 125 "), Ok(("", Instruction::Jal(Reg::G19, 125).into())));
        assert_eq!(parse_inst_1imm1reg("la x19, 0x0F"), Ok(("", MacroInstr::LaImm(Reg::G19, 0x0F).into())));
    }

    #[test]
    fn test_parse_inst_2reg() {
        assert_ne!(parse_macro_2reg("invalid"), Ok(("", Instruction::Addi(Reg::G0, Reg::G0, 0).into())));
        assert_ne!(parse_macro_2reg("   "), Ok(("", Instruction::Addi(Reg::G0, Reg::G0, 0).into())));
        assert_ne!(parse_macro_2reg("lb x1, 0xAA"), Ok(("", Instruction::Lb(Reg::G1, Reg::G0, 0xAA).into())));
        assert_ne!(parse_macro_2reg("mv x1, 0xAA"), Ok(("", Instruction::Addi(Reg::G1, Reg::G0, 0xAA).into())));
        assert_eq!(parse_macro_2reg("mv x1, x4"), Ok(("", Instruction::Addi(Reg::G1, Reg::G4, 0).into())));
        assert_eq!(parse_macro_2reg("mv x12,x4"), Ok(("", Instruction::Addi(Reg::G12, Reg::G4, 0).into())));
    }

    #[test]
    fn test_parse_instr1labl2reg() {
        assert_ne!(parse_macro_1labl2reg("invalid"), Ok(("", Instruction::Addi(Reg::G0, Reg::G0, 0).into())));
        assert_ne!(parse_macro_1labl2reg("   "), Ok(("", Instruction::Addi(Reg::G0, Reg::G0, 0).into())));
        assert_ne!(parse_macro_1labl2reg("sb x1, x6"), Ok(("", MacroInstr::Sb(Reg::G1, Reg::G6, "".to_string(), Part::None).into())));
        assert_ne!(parse_macro_1labl2reg("lb x1, total"), Ok(("", MacroInstr::Lb(Reg::G1, Reg::G0, "total".to_string(), Part::None).into())));
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
        assert_ne!(parse_inst_1imm2reg_lw("invalid"), Ok(("", Instruction::Addi(Reg::G0, Reg::G0, 0).into())));
        assert_ne!(parse_inst_1imm2reg_lw("   "), Ok(("", Instruction::Addi(Reg::G0, Reg::G0, 0).into())));
        assert_ne!(parse_inst_1imm2reg_lw("addi x1, x6"), Ok(("", Instruction::Addi(Reg::G1, Reg::G6, 0).into())));
        assert_ne!(parse_inst_1imm2reg_lw("lbu x1, 0xAA"), Ok(("", Instruction::Lbu(Reg::G1, Reg::G0, 0xAA).into())));
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
        assert_ne!(parse_inst_3reg("invalid"), Ok(("", Instruction::Addi(Reg::G0, Reg::G0, 0).into())));
        assert_ne!(parse_inst_3reg("   "), Ok(("", Instruction::Addi(Reg::G0, Reg::G0, 0).into())));
        assert_ne!(parse_inst_3reg("addi x1, x6, 0xAA"), Ok(("", Instruction::Addi(Reg::G1, Reg::G6, 0xAA).into())));
        assert_ne!(parse_inst_3reg("add x1, x2"), Ok(("", Instruction::Add(Reg::G1, Reg::G2, Reg::G0).into())));
        assert_eq!(parse_inst_3reg("mul x1, x4, x6"), Ok(("", Instruction::Mul(Reg::G1, Reg::G4, Reg::G6).into())));
        assert_ne!(parse_inst_3reg("div x10x14,x7"), Ok(("", MacroInstr::Div(Reg::G10, Reg::G14, Reg::G7).into())));
        assert_eq!(parse_inst_3reg("xor x10,x11, x10"), Ok(("", Instruction::Xor(Reg::G10, Reg::G11, Reg::G10).into())));
        assert_ne!(parse_inst_3reg("xnor x6,  x8,x5"), Ok(("", Instruction::Xnor(Reg::G6, Reg::G8, Reg::G5).into())));
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
        assert_ne!(parse_instruction("addi x1, 0xAA"), Ok(("", Instruction::Addi(Reg::G1, Reg::G0, 0xAA).into())));
        assert_eq!(parse_instruction("mul x1, x4, x6"), Ok(("", Instruction::Mul(Reg::G1, Reg::G4, Reg::G6).into())));
        assert_ne!(parse_instruction("xor x10x14,x7"), Ok(("", Instruction::Xor(Reg::G10, Reg::G14, Reg::G7).into())));
        assert_eq!(parse_instruction("add x10,x11, x10"), Ok(("", Instruction::Add(Reg::G10, Reg::G11, Reg::G10).into())));
        assert_ne!(parse_instruction("xnor x6,  x8,x5"), Ok(("", Instruction::Xnor(Reg::G6, Reg::G8, Reg::G5).into())));
        assert_eq!(parse_instruction("srr x5, x8, 7"), Ok(("", MacroInstr::Srr(Reg::G5, Reg::G8, 7).into())));
        assert_eq!(parse_instruction("rep 20, nop"), Ok(("", MacroInstr::RepInstr(20, Instruction::Addi(Reg::G0, Reg::G0, 0)).into())));
        // More tests & seperate
    }
}