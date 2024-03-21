/*
 * Copyright (c) 2023 Steven Becker
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

use nom::{
    branch::alt, bytes::complete::tag, character::complete::{digit1, space1}, combinator::{
        fail, map_opt, map_res, opt, recognize, value
    }, multi::separated_list1, sequence::{
        delimited, pair, preceded, separated_pair, terminated, tuple
    }, IResult
};

use crate::parser::literals::{parse_imm, parse_reg, parse_label_name};
use crate::common::{MacroInstr, Instruction, Operation, Reg, Part};

use super::symbols::Symbols;

#[derive(Clone)]
enum InstrType {
    Labl(IntermediateOp),
    Reg(IntermediateOp),
    RegLabl(IntermediateOp),
    RegImm(IntermediateOp),
    Reg2(IntermediateOp),
    Reg2Labl(IntermediateOp),
    Reg2Imm(IntermediateOp),
    Reg3(IntermediateOp),
    RegVar(IntermediateOp),
    StoreOp(IntermediateOp),
    LoadOp(IntermediateOp),
    SpecOp(IntermediateOp)
}

impl InstrType {
    fn translate_parse(self, input: &str) -> IResult<&str, Operation> {
        let (rest, _) = space1(input)?;
        match self {
            InstrType::Labl(interop) => {
                let (rest, labl) = parse_label_name(rest)?;

                Ok((rest, match interop {
                    IntermediateOp::Call => MacroInstr::Call(labl.into()).into(),
                    IntermediateOp::Tail => MacroInstr::Tail(labl.into()).into(),
                    IntermediateOp::J => MacroInstr::Jal(Reg::G0, labl.into()).into(),
                    IntermediateOp::Jal => MacroInstr::Jal(Reg::G1, labl.into()).into(),

                    op => unreachable!("[Error] Could not map parsed instruction to internal data structure: {:?}", op),
                }))
            },
            InstrType::Reg(interop) => {
                let (rest, reg) = parse_reg(rest)?;

                Ok((rest, match interop {
                    IntermediateOp::Jr => Instruction::Jalr(Reg::G0, reg, 0).into(),
                    IntermediateOp::Jalr => Instruction::Jalr(Reg::G1, reg, 0).into(),

                    op => unreachable!("[Error] Could not map parsed instruction to internal data structure: {:?}", op),
                }))
            },
            InstrType::RegLabl(interop) => {
                let (rest, args) = separated_pair(parse_reg, parse_seper, parse_label_name)(rest)?;

                Ok((rest, match interop {
                    IntermediateOp::Lui => MacroInstr::Lui(args.0, args.1.into(), Part::None).into(),
                    IntermediateOp::Jal => MacroInstr::Jal(args.0, args.1.into()).into(),
                    IntermediateOp::La => MacroInstr::La(args.0, args.1.into()).into(),

                    op => unreachable!("[Error] Could not map parsed instruction to internal data structure: {:?}", op),
                }))
            },
            InstrType::RegImm(interop) => {
                let (rest, reg) = terminated(parse_reg, parse_seper)(rest)?;

                let (rest, imm) = match opt(parse_imm)(rest)? {
                    (_, None) => {
                        map_opt(parse_label_name, |label| {
                            let labl = smartstring::alias::String::from(label);
                            Some(Symbols::symbols_read(&labl)? as i32)
                        })(rest)?
                    },
                    (rest_f, Some(val)) => (rest_f, val),
                };

                Ok((rest, match interop {
                    IntermediateOp::Lui => Instruction::Lui(reg, imm << 12).into(),
                    IntermediateOp::Auipc => Instruction::Auipc(reg, imm << 12).into(),
                    IntermediateOp::Jal => Instruction::Jal(reg, imm).into(),
            
                    IntermediateOp::Li => MacroInstr::Li(reg, imm).into(),
            
                    op => unreachable!("[Error] Could not map parsed instruction to internal data structure: {:?}", op),
                }))
            },
            InstrType::Reg2(interop) => {
                let (rest, args) = separated_pair(parse_reg, parse_seper, parse_reg)(rest)?;

                Ok((rest, match interop {
                    IntermediateOp::Mv => Instruction::Addi(args.0, args.1, 0).into(),
                    op  => unreachable!("[Error] Could not map parsed instruction to internal data structure: {:?}", op),
                }))
            },
            InstrType::Reg2Labl(interop) => {
                let (rest, args) = tuple((
                    parse_reg,
                    preceded(parse_seper, parse_reg),
                    preceded(parse_seper, parse_label_name)))
                (rest)?;

                Ok((rest, match interop {
                    IntermediateOp::Beq => MacroInstr::Beq(args.0, args.1, args.2.into()).into(),
                    IntermediateOp::Bne => MacroInstr::Bne(args.0, args.1, args.2.into()).into(),
                    IntermediateOp::Blt => MacroInstr::Blt(args.0, args.1, args.2.into()).into(),
                    IntermediateOp::Bltu => MacroInstr::Bltu(args.0, args.1, args.2.into()).into(),
                    IntermediateOp::Bge => MacroInstr::Bge(args.0, args.1, args.2.into()).into(),
                    IntermediateOp::Bgeu => MacroInstr::Bgeu(args.0, args.1, args.2.into()).into(),
                    
                    // this may be removed
                    IntermediateOp::Jalr => MacroInstr::Jalr(args.0, args.1, args.2.into(), Part::None).into(),
                    // until here
            
                    IntermediateOp::Sb => MacroInstr::SbLabl(args.0, args.1, args.2.into()).into(),
                    IntermediateOp::Sh => MacroInstr::ShLabl(args.0, args.1, args.2.into()).into(),
                    IntermediateOp::Sw => MacroInstr::SwLabl(args.0, args.1, args.2.into()).into(),
            
                    IntermediateOp::Addi => MacroInstr::Addi(args.0, args.1, args.2.into(), Part::None).into(),
            
                    op => unreachable!("[Error] Could not map parsed instruction to internal data structure: {:?}", op),
                }))
            },
            InstrType::Reg2Imm(interop) => {
                let (rest, args) = pair(
                    parse_reg,
                    delimited(parse_seper, parse_reg, parse_seper)
                )(rest)?;

                let (rest, imm) = match opt(parse_imm)(rest)? {
                    (rest_f, None) => {
                        map_opt(parse_label_name, |label| {
                            let labl = smartstring::alias::String::from(label);
                            Some(Symbols::symbols_read(&labl)? as i32)
                        })(rest_f)?
                    },
                    (rest_f, Some(val)) => (rest_f, val),
                };

                Ok((rest, match interop {
                    // these may be removed
                    IntermediateOp::Beq => Instruction::Beq(args.0, args.1, imm).into(),
                    IntermediateOp::Bne => Instruction::Bne(args.0, args.1, imm).into(),
                    IntermediateOp::Blt => Instruction::Blt(args.0, args.1, imm).into(),
                    IntermediateOp::Bltu => Instruction::Bltu(args.0, args.1, imm).into(),
                    IntermediateOp::Bge => Instruction::Bge(args.0, args.1, imm).into(),
                    IntermediateOp::Bgeu => Instruction::Bgeu(args.0, args.1, imm).into(),
                    // until here
            
                    IntermediateOp::Slli => Instruction::Slli(args.0, args.1, imm).into(),
                    IntermediateOp::Srli => Instruction::Srli(args.0, args.1, imm).into(),
                    IntermediateOp::Srai => Instruction::Srai(args.0, args.1, imm).into(),
            
                    IntermediateOp::Addi => Instruction::Addi(args.0, args.1, imm).into(),

                    IntermediateOp::Slti => Instruction::Slti(args.0, args.1, imm).into(),
                    IntermediateOp::Sltiu => Instruction::Sltiu(args.0, args.1, imm).into(),
                    IntermediateOp::Xori => Instruction::Xori(args.0, args.1, imm).into(),
                    IntermediateOp::Ori => Instruction::Ori(args.0, args.1, imm).into(),
                    IntermediateOp::Andi => Instruction::Andi(args.0, args.1, imm).into(),

                    IntermediateOp::Jalr => Instruction::Jalr(args.0, args.1, imm).into(),

                    IntermediateOp::Srr => MacroInstr::Srr(args.0, args.1, imm).into(),
                    IntermediateOp::Slr => MacroInstr::Slr(args.0, args.1, imm).into(),

                    op => unreachable!("[Error] Could not map parsed instruction to internal data structure: {:?}", op),
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
            
                    IntermediateOp::Mul => Instruction::Mul(args.0, args.1, args.2).into(),
                    IntermediateOp::Mulh => Instruction::Mulh(args.0, args.1, args.2).into(),
                    IntermediateOp::Mulhsu => Instruction::Mulhsu(args.0, args.1, args.2).into(),
                    IntermediateOp::Mulhu => Instruction::Mulhu(args.0, args.1, args.2).into(),
                    IntermediateOp::Div => Instruction::Div(args.0, args.1, args.2).into(),
                    IntermediateOp::Divu => Instruction::Divu(args.0, args.1, args.2).into(),
                    IntermediateOp::Rem => Instruction::Rem(args.0, args.1, args.2).into(),
                    IntermediateOp::Remu => Instruction::Remu(args.0, args.1, args.2).into(),
            
                    IntermediateOp::Equal => Instruction::Equal(args.0, args.1, args.2).into(),
                    IntermediateOp::Xnor => Instruction::Xnor(args.0, args.1, args.2).into(),
                    IntermediateOp::Nor => todo!("Not implemented yet!"),
            
                    op => unreachable!("[Error] Could not map parsed instruction to internal data structure: {:?}", op),
                }))
            },
            InstrType::RegVar(interop) => {
                let (rest, args) = separated_list1(parse_seper, parse_reg)(rest)?;

                Ok((rest, match interop {
                    IntermediateOp::Push => MacroInstr::Push(args).into(),
                    IntermediateOp::Pop => MacroInstr::Pop(args).into(),

                    op => unreachable!("[Error] Could not map parsed instruction to internal data structure: {:?}", op),
                }))
            },
            InstrType::StoreOp(interop) => {
                let (rest, treg) = terminated(parse_reg, parse_seper)(rest)?;

                let (rest, imm) = match opt(parse_imm)(rest)? {
                    (rest_f, None) => {
                        opt(map_opt(parse_label_name, |label| {
                            let labl = smartstring::alias::String::from(label);
                            Some(Symbols::symbols_read(&labl)? as i32)
                        }))(rest_f)?
                    },
                    op => op,
                };

                match imm {
                    Some(offset) => { // s{b|w|h} x[0-31], IMM
                        if let (rest, Some(mem_reg)) = opt(delimited(
                            nom::character::complete::char('('),
                            parse_reg,
                            nom::character::complete::char(')')
                        ))(rest)? { // s{b|w|h} x[0-31], IMM(x[0-31])
                            Ok((rest, match interop {
                                IntermediateOp::Sb => Instruction::Sb(treg, mem_reg, offset),
                                IntermediateOp::Sh => Instruction::Sh(treg, mem_reg, offset),
                                IntermediateOp::Sw => Instruction::Sw(treg, mem_reg, offset),
            
                                op => unreachable!("[Error] Could not map parsed instruction to internal data structure: {:?}", op),
                            }.into()))
                        } else { // s{b|w|h} x[0-31], IMM OR s{b|w|h} x[0-31], IMM, x[0-31]
                            if offset > 0b01_11111_11111_i32 || offset < -2048 {
                                let (rest, temp_reg) = preceded(parse_seper, parse_reg)(rest)?;

                                Ok((rest, match interop {
                                    IntermediateOp::Sb => MacroInstr::SbImm(treg, temp_reg, offset),
                                    IntermediateOp::Sh => MacroInstr::ShImm(treg, temp_reg, offset),
                                    IntermediateOp::Sw => MacroInstr::SwImm(treg, temp_reg, offset),
                
                                    op => unreachable!("[Error] Could not map parsed instruction to internal data structure: {:?}", op),
                                }.into()))
                            } else {
                                Ok((rest, match interop {
                                    IntermediateOp::Sb => Instruction::Sb(treg, Reg::G0, offset),
                                    IntermediateOp::Sh => Instruction::Sh(treg, Reg::G0, offset),
                                    IntermediateOp::Sw => Instruction::Sw(treg, Reg::G0, offset),
                
                                    op => unreachable!("[Error] Could not map parsed instruction to internal data structure: {:?}", op),
                                }.into()))
                            }
                        }
                    },
                    None => { // s{b|w|h} x[0-31], 
                        if let (rest, Some(mem_reg)) = opt(delimited(
                            nom::character::complete::char('('),
                            parse_reg,
                            nom::character::complete::char(')')
                        ))(rest)? { // s{b|w|h} x[0-31], (x[0-31])
                            Ok((rest, match interop {
                                IntermediateOp::Sb => Instruction::Sb(treg, mem_reg, 0),
                                IntermediateOp::Sh => Instruction::Sh(treg, mem_reg, 0),
                                IntermediateOp::Sw => Instruction::Sw(treg, mem_reg, 0),
            
                                op => unreachable!("[Error] Could not map parsed instruction to internal data structure: {:?}", op),
                            }.into()))
                        } else {
                            let (rest, (label, temp_reg)) = separated_pair(parse_label_name, parse_seper, parse_reg)(rest)?;
                            Ok((rest, match interop {
                                IntermediateOp::Sb => MacroInstr::SbLabl(treg, temp_reg, label.into()),
                                IntermediateOp::Sh => MacroInstr::ShLabl(treg, temp_reg, label.into()),
                                IntermediateOp::Sw => MacroInstr::SwLabl(treg, temp_reg, label.into()),
            
                                op => unreachable!("[Error] Could not map parsed instruction to internal data structure: {:?}", op),
                            }.into())) // s{b|w|h} x[0-31], LABEL, x[0-31]
                        }
                    },
                }
            },
            InstrType::LoadOp(interop) => {
                let (rest, treg) = terminated(parse_reg, parse_seper)(rest)?;

                let (rest, imm) = match opt(parse_imm)(rest)? {
                    (rest_f, None) => {
                        opt(map_opt(parse_label_name, |label| {
                            let labl = smartstring::alias::String::from(label);
                            Some(Symbols::symbols_read(&labl)? as i32)
                        }))(rest_f)?
                    },
                    op => op,
                };

                match imm {
                    Some(offset) => { // l{b|w|h|bu|hu} x[0-31], IMM
                        if let (rest, Some(mem_reg)) = opt(delimited(
                            nom::character::complete::char('('),
                            parse_reg,
                            nom::character::complete::char(')')
                        ))(rest)? { // l{b|w|h|bu|hu} x[0-31], IMM(x[0-31])
                            Ok((rest, match interop {
                                IntermediateOp::Lb => Instruction::Lb(treg, mem_reg, offset),
                                IntermediateOp::Lbu => Instruction::Lbu(treg, mem_reg, offset),
                                IntermediateOp::Lh => Instruction::Lh(treg, mem_reg, offset),
                                IntermediateOp::Lhu => Instruction::Lhu(treg,mem_reg, offset),
                                IntermediateOp::Lw => Instruction::Lw(treg, mem_reg, offset),
            
                                op => unreachable!("[Error] Could not map parsed instruction to internal data structure: {:?}", op),
                            }.into()))
                        } else { // l{b|w|h|bu|hu} x[0-31], IMM
                            if offset > 0b01_11111_11111_i32 || offset < -2048 {
                                Ok((rest, match interop {
                                    IntermediateOp::Lb => MacroInstr::LbImm(treg, treg, offset),
                                    IntermediateOp::Lbu => MacroInstr::LbuImm(treg, treg, offset),
                                    IntermediateOp::Lh => MacroInstr::LhImm(treg, treg, offset),
                                    IntermediateOp::Lhu => MacroInstr::LhuImm(treg, treg, offset),
                                    IntermediateOp::Lw => MacroInstr::LwImm(treg, treg, offset),
                
                                    op => unreachable!("[Error] Could not map parsed instruction to internal data structure: {:?}", op),
                                }.into()))
                            } else {
                                Ok((rest, match interop {
                                    IntermediateOp::Lb => Instruction::Lb(treg, Reg::G0, offset),
                                    IntermediateOp::Lbu => Instruction::Lbu(treg, Reg::G0, offset),
                                    IntermediateOp::Lh => Instruction::Lh(treg, Reg::G0, offset),
                                    IntermediateOp::Lhu => Instruction::Lhu(treg, Reg::G0, offset),
                                    IntermediateOp::Lw => Instruction::Lw(treg, Reg::G0, offset),
                
                                    op => unreachable!("[Error] Could not map parsed instruction to internal data structure: {:?}", op),
                                }.into()))
                            }
                        }
                    },
                    None => { // l{b|w|h|bu|hu} x[0-31], 
                        if let (rest, Some(mem_reg)) = opt(delimited(
                            nom::character::complete::char('('),
                            parse_reg,
                            nom::character::complete::char(')')
                        ))(rest)? { // l{b|w|h|bu|hu} x[0-31], (x[0-31])
                            Ok((rest, match interop {
                                IntermediateOp::Lb => Instruction::Lb(treg, mem_reg, 0),
                                IntermediateOp::Lbu => Instruction::Lbu(treg, mem_reg, 0),
                                IntermediateOp::Lh => Instruction::Lh(treg, mem_reg, 0),
                                IntermediateOp::Lhu => Instruction::Lhu(treg,mem_reg, 0),
                                IntermediateOp::Lw => Instruction::Lw(treg, mem_reg, 0),
            
                                op => unreachable!("[Error] Could not map parsed instruction to internal data structure: {:?}", op),
                            }.into()))
                        } else if let (rest, Some(label)) = opt(parse_label_name)(rest)? {
                            Ok((rest, match interop {
                                IntermediateOp::Lb => MacroInstr::LbLabl(treg, treg, label.into(), Part::None),
                                IntermediateOp::Lbu => MacroInstr::LbuLabl(treg, treg, label.into()),
                                IntermediateOp::Lh => MacroInstr::LhLabl(treg, treg, label.into(), Part::None),
                                IntermediateOp::Lhu => MacroInstr::LhuLabl(treg, treg, label.into()),
                                IntermediateOp::Lw => MacroInstr::LwLabl(treg, treg, label.into(), Part::None),
            
                                op => unreachable!("[Error] Could not map parsed instruction to internal data structure: {:?}", op),
                            }.into())) // l{b|w|h|bu|hu} x[0-31], LABEL
                        } else {
                            let (rest, (label, mem_reg)) = pair(
                                parse_low_label,
                                delimited(
                                    nom::character::complete::char('('),
                                    parse_reg,
                                    nom::character::complete::char(')')
                                )
                            )(rest)?; // l{b|w|h|bu|hu} x[0-31], %lo(LABEL)(x[0-31])

                            match interop {
                                IntermediateOp::Lbu => fail(""),
                                IntermediateOp::Lhu => fail(""),
                                IntermediateOp::Lb => Ok((rest, MacroInstr::LbLabl(treg, mem_reg, label.into(), Part::Lower).into())),
                                IntermediateOp::Lh => Ok((rest, MacroInstr::LhLabl(treg, mem_reg, label.into(), Part::Lower).into())),
                                IntermediateOp::Lw => Ok((rest, MacroInstr::LwLabl(treg, mem_reg, label.into(), Part::Lower).into())),

                                op => unreachable!("[Error] Could not map parsed instruction to internal data structure: {:?}", op),
                            }
                        }
                    },
                }
            },
            InstrType::SpecOp(interop) => {
                let (rest, reg1) = terminated(parse_reg, parse_seper)(rest)?;

                match interop {
                    IntermediateOp::Addi => {
                        let (rest, reg2) = terminated(parse_reg, parse_seper)(rest)?;
        
                        let (rest, imm) = match opt(parse_imm)(rest)? {
                            (rest_f, None) => {
                                opt(map_opt(parse_label_name, |label| {
                                    let labl = smartstring::alias::String::from(label);
                                    Some(Symbols::symbols_read(&labl)? as i32)
                                }))(rest_f)?
                            },
                            op => op,
                        };

                        if let Some(imm) = imm {
                            Ok((rest, Instruction::Addi(reg1, reg2, imm).into()))
                        } else {
                            let (rest, label) = parse_low_label(rest)?;

                            Ok((rest, MacroInstr::Addi(reg1, reg2, label.into(), Part::None).into()))
                        }
                    },
                    IntermediateOp::Lui => {
                        let (rest, imm) = match opt(parse_imm)(rest)? {
                            (rest_f, None) => {
                                opt(map_opt(parse_label_name, |label| {
                                    let labl = smartstring::alias::String::from(label);
                                    Some(Symbols::symbols_read(&labl)? as i32)
                                }))(rest_f)?
                            },
                            op => op,
                        };

                        if let Some(imm) = imm {
                            Ok((rest, Instruction::Lui(reg1, imm << 12).into()))
                        } else {
                            let (rest, label) = parse_high_label(rest)?;

                            Ok((rest, MacroInstr::Lui(reg1, label.into(), Part::None).into()))
                        }
                    },

                    op => unreachable!("[Error] Could not map parsed instruction to internal data structure: {:?}", op),
                }
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
    Mul, Mulh, Mulhsu, Mulhu,
    Div, Divu, Rem, Remu,
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

pub fn parse_seper(input: &str) -> IResult<&str, &str> {
    recognize(
        pair(nom::character::complete::char(','), 
        opt(nom::character::complete::char(' ')))
    )(input)
}

fn parse_low_label(input: &str) -> IResult<&str, &str> {
    preceded(
        tag("%lo"),
        delimited(
            nom::character::complete::char('('),
            parse_label_name,
            nom::character::complete::char(')')
        )
    )(input)
}

fn parse_high_label(input: &str) -> IResult<&str, &str> {
    preceded(
        tag("%hi"),
        delimited(
            nom::character::complete::char('('),
            parse_label_name,
            nom::character::complete::char(')')
        )
    )(input)
}

fn parse_macro_noparm(input: &str) -> IResult<&str, Operation> {
    let (rest, instr) = alt((
        value(Operation::Instr(Instruction::Addi(Reg::G0, Reg::G0, 0)), tag("nop")),
        value(Operation::Instr(Instruction::Jalr(Reg::G0, Reg::G1, 0)), tag("ret")),

        value(Operation::Instr(Instruction::Ebreak), tag("ebreak")),
        value(Operation::Instr(Instruction::Ecall), tag("ecall")),
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

fn parse_macro_1reg(input: &str) -> IResult<&str, Operation> {
    let (rest, instr) = alt((
        value(InstrType::Reg(IntermediateOp::Jr), tag("jr")),
        value(InstrType::Reg(IntermediateOp::Jalr), tag("jalr")),
    ))(input)?;
    instr.translate_parse(rest)
}

fn parse_macro_1labl1reg(input: &str) -> IResult<&str, Operation> {
    let (rest, instr) = alt((
        value(InstrType::RegLabl(IntermediateOp::Jal), tag("jal")),

        value(InstrType::RegLabl(IntermediateOp::Li), tag("li")),
        value(InstrType::RegLabl(IntermediateOp::La), tag("la")),
    ))(input)?;
    instr.translate_parse(rest)
}

fn parse_inst_1imm1reg(input: &str) -> IResult<&str, Operation> {
    let (rest, instr) = alt((
        value(InstrType::SpecOp(IntermediateOp::Lui), tag("lui")),
        value(InstrType::RegImm(IntermediateOp::Auipc), tag("auipc")),
        value(InstrType::RegImm(IntermediateOp::Jal), tag("jal")),

        value(InstrType::RegImm(IntermediateOp::Li), tag("li")),
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

        value(InstrType::Reg2Labl(IntermediateOp::Lbu), tag("lbu")),
        value(InstrType::Reg2Labl(IntermediateOp::Lhu), tag("lhu")),
        value(InstrType::Reg2Labl(IntermediateOp::Lb), tag("lb")),
        value(InstrType::Reg2Labl(IntermediateOp::Lh), tag("lh")),
        value(InstrType::Reg2Labl(IntermediateOp::Lw), tag("lw")),
    ))(input)?;
    instr.translate_parse(rest)
}

fn parse_inst_1imm2reg_lw(input: &str) -> IResult<&str, Operation> {
    let (rest, instr) = alt((
        value(InstrType::Reg2Imm(IntermediateOp::Beq), tag("beq")),
        value(InstrType::Reg2Imm(IntermediateOp::Bne), tag("bne")),
        value(InstrType::Reg2Imm(IntermediateOp::Bltu), tag("bltu")),
        value(InstrType::Reg2Imm(IntermediateOp::Bgeu), tag("bgeu")),
        value(InstrType::Reg2Imm(IntermediateOp::Blt), tag("blt")),
        value(InstrType::Reg2Imm(IntermediateOp::Bge), tag("bge")),

        value(InstrType::Reg2Imm(IntermediateOp::Slli), tag("slli")),
        value(InstrType::Reg2Imm(IntermediateOp::Srli), tag("srli")),
        value(InstrType::Reg2Imm(IntermediateOp::Srai), tag("srai")),
    ))(input)?;
    instr.translate_parse(rest)
}

fn parse_inst_1imm2reg_up(input: &str) -> IResult<&str, Operation> {
    let (rest, instr) = alt((
        value(InstrType::SpecOp(IntermediateOp::Addi), tag("addi")),

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

        value(InstrType::Reg3(IntermediateOp::Mulhsu), tag("mulhsu")),
        value(InstrType::Reg3(IntermediateOp::Mulhu), tag("mulhu")),
        value(InstrType::Reg3(IntermediateOp::Mulh), tag("mulh")),
        value(InstrType::Reg3(IntermediateOp::Mul), tag("mul")),
        value(InstrType::Reg3(IntermediateOp::Divu), tag("divu")),
        value(InstrType::Reg3(IntermediateOp::Div), tag("div")),
        value(InstrType::Reg3(IntermediateOp::Remu), tag("remu")),
        value(InstrType::Reg3(IntermediateOp::Rem), tag("rem")),

        value(InstrType::Reg3(IntermediateOp::Xnor), tag("xnor")),
        value(InstrType::Reg3(IntermediateOp::Equal), tag("eq")),
        value(InstrType::Reg3(IntermediateOp::Nor), tag("nor")),
    ))(input)?;
    instr.translate_parse(rest)
}

fn parse_mem_ops(input: &str) -> IResult<&str, Operation> {
    let (rest, instr) = alt((
        value(InstrType::StoreOp(IntermediateOp::Sb), tag("sb")),
        value(InstrType::StoreOp(IntermediateOp::Sh), tag("sh")),
        value(InstrType::StoreOp(IntermediateOp::Sw), tag("sw")),

        value(InstrType::LoadOp(IntermediateOp::Lbu), tag("lbu")),
        value(InstrType::LoadOp(IntermediateOp::Lhu), tag("lhu")),
        value(InstrType::LoadOp(IntermediateOp::Lb), tag("lb")),
        value(InstrType::LoadOp(IntermediateOp::Lh), tag("lh")),
        value(InstrType::LoadOp(IntermediateOp::Lw), tag("lw")),
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
    alt((
        parse_special_macro,
        parse_macro_multiarg,
        parse_mem_ops,
        parse_inst_3reg,
        parse_inst_1imm2reg_up,
        parse_inst_1imm2reg_lw,
        parse_macro_1labl2reg,
        parse_macro_2reg,
        parse_inst_1imm1reg,
        parse_macro_1labl1reg,
        parse_macro_1reg,
        parse_macro_1labl,
        parse_macro_noparm,
    ))(input)
}

#[cfg(test)]
mod tests {
    use crate::parser::symbols::Symbols;

    use super::*;

    fn setup_symbols(symbol: smartstring::alias::String, def: i128) {
        Symbols::symbols_clear();
        Symbols::symbols_write(symbol, def);
    }

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
        assert_ne!(parse_macro_1labl("call"), Ok(("", MacroInstr::Call("".into()).into())));
        assert_eq!(parse_macro_1labl("tail test"), Ok(("", MacroInstr::Tail("test".into()).into())));
        assert_eq!(parse_macro_1labl("call HANS"), Ok(("", MacroInstr::Call("HANS".into()).into())));
        assert_ne!(parse_macro_1labl("call label  "), Ok(("", MacroInstr::Call("label".into()).into())));
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
        assert_ne!(parse_macro_1labl1reg("auipc s2, helloWorld"), Ok(("", MacroInstr::Auipc(Reg::G18, "helloWorld".into()).into())));
        assert_eq!(parse_macro_1labl1reg("jal   x20, test"), Ok(("", MacroInstr::Jal(Reg::G20, "test".into()).into())));
        assert_ne!(parse_macro_1labl1reg("jal x19, train "), Ok(("", MacroInstr::Jal(Reg::G19, "train".into()).into())));
        assert_eq!(parse_macro_1labl1reg("la x19, HELLOWORLD"), Ok(("", MacroInstr::La(Reg::G19, "HELLOWORLD".into()).into())));
    }

    #[test]
    fn test_parse_instr1imm1reg() {
        assert_ne!(parse_inst_1imm1reg("invalid"), Ok(("", Instruction::Addi(Reg::G0, Reg::G0, 0).into())));
        assert_ne!(parse_inst_1imm1reg(" "), Ok(("", Instruction::Addi(Reg::G0, Reg::G0, 0).into())));
        assert_ne!(parse_inst_1imm1reg(""), Ok(("", Instruction::Addi(Reg::G0, Reg::G0, 0).into())));
        assert_ne!(parse_inst_1imm1reg("lb"), Ok(("", Instruction::Lb(Reg::G0, Reg::G0, 0).into())));
        assert_ne!(parse_inst_1imm1reg(""), Ok(("", Instruction::Addi(Reg::G0, Reg::G0, 0).into())));
        assert_ne!(parse_inst_1imm1reg("lui"), Ok(("", Instruction::Lui(Reg::G0, 0).into())));
        assert_eq!(parse_inst_1imm1reg("lui x12, 12"), Ok(("", Instruction::Lui(Reg::G12, 49152).into())));
        assert_eq!(parse_inst_1imm1reg("lui a2, %hi(stop)"), Ok(("", MacroInstr::Lui(Reg::G12, "stop".into(), Part::None).into())));
        assert_eq!(parse_inst_1imm1reg("auipc x18, 0x20"), Ok(("", Instruction::Auipc(Reg::G18, 131072).into())));
        assert_eq!(parse_inst_1imm1reg("jal x20, 5"), Ok(("", Instruction::Jal(Reg::G20, 5).into())));
        assert_ne!(parse_inst_1imm1reg("jal x19, 125 "), Ok(("", Instruction::Jal(Reg::G19, 125).into())));
        assert_ne!(parse_inst_1imm1reg("la x19, 0x0F"), Ok(("", MacroInstr::La(Reg::G19, "0x0F".into()).into())));

        setup_symbols("TEST".into(), 25);

        assert_eq!(parse_inst_1imm1reg("li  x11, TEST"), Ok(("", MacroInstr::Li(Reg::G11, 25).into())));
        assert_eq!(parse_inst_1imm1reg("lui x15, TEST"), Ok(("", Instruction::Lui(Reg::G15, 102400).into())));
        assert_ne!(parse_inst_1imm1reg("auipc x15, .TEST"), Ok(("", Instruction::Auipc(Reg::G15, 102400).into())));
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
        assert_eq!(parse_macro_1labl2reg("bgeu  x1, x4, sTaRt"), Ok(("", MacroInstr::Bgeu(Reg::G1, Reg::G4, "sTaRt".into()).into())));
        assert_eq!(parse_macro_1labl2reg("blt x10,x10, last"), Ok(("", MacroInstr::Blt(Reg::G10, Reg::G10, "last".into()).into())));
        assert_ne!(parse_macro_1labl2reg("jalr  x6,  x8,test"), Ok(("", MacroInstr::Jalr(Reg::G6, Reg::G8, "test".into(), Part::None).into())));
        assert_ne!(parse_macro_1labl2reg("beqx1x15,start"), Ok(("", MacroInstr::Beq(Reg::G1, Reg::G15, "start".into()).into())));
    }

    #[test]
    fn test_parse_inst_1imm2reg() {
        assert_ne!(parse_inst_1imm2reg_lw("invalid"), Ok(("", Instruction::Addi(Reg::G0, Reg::G0, 0).into())));
        assert_ne!(parse_inst_1imm2reg_lw("   "), Ok(("", Instruction::Addi(Reg::G0, Reg::G0, 0).into())));
        assert_ne!(parse_inst_1imm2reg_lw("addi x1, x6"), Ok(("", Instruction::Addi(Reg::G1, Reg::G6, 0).into())));
        assert_eq!(parse_inst_1imm2reg_lw("blt x1, x4, 5"), Ok(("", Instruction::Blt(Reg::G1, Reg::G4, 5).into())));
        assert_ne!(parse_inst_1imm2reg_lw("bge x6,  x8,5"), Ok(("", Instruction::Bge(Reg::G6, Reg::G8, 5).into())));

        assert_eq!(parse_inst_1imm2reg_up("addi x1, x2, 0xAA"), Ok(("", Instruction::Addi(Reg::G1, Reg::G2, 0xAA).into())));
        assert_eq!(parse_inst_1imm2reg_up("srr   x13,x15,6"), Ok(("", MacroInstr::Srr(Reg::G13, Reg::G15, 6).into())));
        assert_ne!(parse_inst_1imm2reg_up("sltix1x15,6"), Ok(("", Instruction::Slti(Reg::G1, Reg::G15, 6).into())));
        assert_ne!(parse_inst_1imm2reg_up("slli x12, x15,  6"), Ok(("", Instruction::Slli(Reg::G12, Reg::G15, 6).into())));
        // TODO: More tests

        setup_symbols("HelloWorld".into(), 404);

        assert_eq!(parse_inst_1imm2reg_lw("blt x1, x4, HelloWorld"), Ok(("", Instruction::Blt(Reg::G1, Reg::G4, 404).into())));
    }

    #[test]
    fn test_parse_mem_ops() {
        assert_eq!(parse_mem_ops("sb x10,51(x10)"), Ok(("", Instruction::Sb(Reg::G10, Reg::G10, 51).into())));
        assert_eq!(parse_mem_ops("sh x1, (x10)"), Ok(("", Instruction::Sh(Reg::G1, Reg::G10, 0).into())));
        assert_eq!(parse_mem_ops("sw x12, 12(x16)"), Ok(("", Instruction::Sw(Reg::G12, Reg::G16, 12).into())));

        assert_eq!(parse_mem_ops("sb x17, -12"), Ok(("", Instruction::Sb(Reg::G17, Reg::G0, -12).into())));
        assert_eq!(parse_mem_ops("sh x11, 1506"), Ok(("", Instruction::Sh(Reg::G11, Reg::G0, 1506).into())));
        assert_ne!(parse_mem_ops("sh x12, 15060"), Ok(("", Instruction::Sh(Reg::G12, Reg::G0, 15060).into())));
        assert_ne!(parse_mem_ops("sw x15, -12515"), Ok(("", Instruction::Sw(Reg::G15, Reg::G0, -12515).into())));
        
        assert_eq!(parse_mem_ops("sb x17, 151500, x10"), Ok(("", MacroInstr::SbImm(Reg::G17, Reg::G10, 151500).into())));
        assert_eq!(parse_mem_ops("sh x11, -151251, x17"), Ok(("", MacroInstr::ShImm(Reg::G11, Reg::G17, -151251).into())));
        assert_eq!(parse_mem_ops("sw x10, 15006, x2"), Ok(("", MacroInstr::SwImm(Reg::G10, Reg::G2, 15006).into())));
        
        assert_ne!(parse_mem_ops("sb x1, x6"), Ok(("", MacroInstr::SbLabl(Reg::G1, Reg::G6, "".into()).into())));
        assert_eq!(parse_mem_ops("sb x17, dasLabel, x10"), Ok(("", MacroInstr::SbLabl(Reg::G17, Reg::G10, "dasLabel".into()).into())));
        assert_eq!(parse_mem_ops("sh x13,loading,x15"), Ok(("", MacroInstr::ShLabl(Reg::G13, Reg::G15, "loading".into()).into())));
        assert_eq!(parse_mem_ops("sw x10, randa, x2"), Ok(("", MacroInstr::SwLabl(Reg::G10, Reg::G2, "randa".into()).into())));

        assert_eq!(parse_mem_ops("lbu x1, 0xAA"), Ok(("", Instruction::Lbu(Reg::G1, Reg::G0, 0xAA).into())));
        assert_ne!(parse_mem_ops("lb x1x4,0x6"), Ok(("", Instruction::Lb(Reg::G1, Reg::G4, 6).into())));
        assert_eq!(parse_mem_ops("lb x10,(x10)"), Ok(("", Instruction::Lb(Reg::G10, Reg::G10, 0).into())));
        assert_eq!(parse_mem_ops("lw x10, -1(x10)"), Ok(("", Instruction::Lw(Reg::G10, Reg::G10, -1).into())));
        assert_eq!(parse_mem_ops("lb x26, 0x500(zero)"), Ok(("", Instruction::Lb(Reg::G26, Reg::G0, 0x500).into())));
        assert_ne!(parse_mem_ops("lw x10,x10)"), Ok(("", Instruction::Lw(Reg::G10, Reg::G10, 0).into())));

        assert_ne!(parse_mem_ops("lb x1, total"), Ok(("", MacroInstr::LbLabl(Reg::G1, Reg::G0, "total".into(), Part::None).into())));
        assert_ne!(parse_mem_ops("lhu x1, x2, hans"), Ok(("", MacroInstr::LhuLabl(Reg::G1, Reg::G2, "hans".into()).into())));
        assert_ne!(parse_mem_ops("lbu x12, x15,  dasletzte"), Ok(("", MacroInstr::LbuLabl(Reg::G12, Reg::G15, "dasletzte".into()).into())));

        setup_symbols("HelloWorld".into(), 404);

        assert_eq!(parse_mem_ops("sw x10,HelloWorld(x10)"), Ok(("", Instruction::Sw(Reg::G10, Reg::G10, 404).into())));

        assert_eq!(parse_mem_ops("lbu x1, HelloWorld"), Ok(("", Instruction::Lbu(Reg::G1, Reg::G0, 404).into())));
        assert_ne!(parse_mem_ops("lb x1x4,0x6"), Ok(("", Instruction::Lb(Reg::G1, Reg::G4, 6).into())));
    }

    #[test]
    fn test_parse_inst_3reg() {
        assert_ne!(parse_inst_3reg("invalid"), Ok(("", Instruction::Addi(Reg::G0, Reg::G0, 0).into())));
        assert_ne!(parse_inst_3reg("   "), Ok(("", Instruction::Addi(Reg::G0, Reg::G0, 0).into())));
        assert_ne!(parse_inst_3reg("addi x1, x6, 0xAA"), Ok(("", Instruction::Addi(Reg::G1, Reg::G6, 0xAA).into())));
        assert_ne!(parse_inst_3reg("add x1, x2"), Ok(("", Instruction::Add(Reg::G1, Reg::G2, Reg::G0).into())));
        assert_eq!(parse_inst_3reg("mul x1, x4, x6"), Ok(("", Instruction::Mul(Reg::G1, Reg::G4, Reg::G6).into())));
        assert_ne!(parse_inst_3reg("div x10x14,x7"), Ok(("", Instruction::Div(Reg::G10, Reg::G14, Reg::G7).into())));
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
