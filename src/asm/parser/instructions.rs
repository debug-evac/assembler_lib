/*
 * Copyright (c) 2023 Steven Becker
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

use winnow::{
    ascii::{digit1, space1}, combinator::{backtrack_err, cut_err, delimited, empty, fail, opt, preceded, separated, separated_pair, terminated}, dispatch, error::{StrContext, StrContextValue}, stream::AsChar, token::{literal, take_while}, PResult, Parser
};

use crate::asm::parser::literals::{parse_imm, parse_reg, parse_label_name};
use crate::common::{MacroInstr, Instruction, Operation, Reg, Part};

use super::{errors::ParserError, symbols::Symbols};

#[derive(Clone)]
enum InstrType {
    NoArg(IntermediateOp),
    Labl(IntermediateOp),
    Reg(IntermediateOp),
    RegLabl(IntermediateOp),
    RegImm(IntermediateOp),
    Reg2(IntermediateOp),
    #[allow(dead_code)]
    Reg2Labl(IntermediateOp),
    Reg2LablOrImm(IntermediateOp),
    Reg2Imm(IntermediateOp),
    Reg3(IntermediateOp),
    RegVar(IntermediateOp),
    StoreOp(IntermediateOp),
    LoadOp(IntermediateOp),
    SpecOp(IntermediateOp),
    Jal,
    Jalr,
    Oper,
}

macro_rules! parse_symimm {
    ($input:expr) => {
        match opt(parse_imm).parse_next($input)? {
            None => {
                cut_err(parse_label_name.verify_map(|label| {
                    let labl = smartstring::alias::String::from(label);
                    Some(Symbols::symbols_read(&labl)? as i32)
                }))
                .context(StrContext::Expected(StrContextValue::StringLiteral("<Symbol> OR <Imm>")))
                .parse_next($input)?
            },
            Some(val) => val,
        }
    };
    (opt, $input:expr) => {
        match opt(parse_imm).parse_next($input)? {
            None => {
                opt(parse_label_name.verify_map(|label| {
                    let labl = smartstring::alias::String::from(label);
                    Some(Symbols::symbols_read(&labl)? as i32)
                })).parse_next($input)?
            },
            op => op,
        }
    };
}

fn parse_delimited_reg(input: &mut &str) -> PResult<Reg> {
    delimited(
        '(',
        parse_reg,
        cut_err(')')
            .context(StrContext::Expected(StrContextValue::CharLiteral(')')))
    )
    .parse_next(input)
}

impl InstrType {
    fn translate_parse(self, input: &mut &str) -> PResult<Operation> {
        if let InstrType::NoArg(interop) = self {
            return Ok(match interop {
                IntermediateOp::Nop => Instruction::Addi(Reg::G0, Reg::G0, 0).into(),
                IntermediateOp::Ret => Instruction::Jalr(Reg::G0, Reg::G1, 0).into(), 
                IntermediateOp::Ebreak => Operation::Instr(Instruction::Ebreak), 
                IntermediateOp::Ecall => Operation::Instr(Instruction::Ecall),

                op => unreachable!("[Error] Could not map parsed instruction to internal data structure: {:?}", op),
            })
        }
        let _ = cut_err(space1)
            .context(StrContext::Expected(StrContextValue::Description("one or more spaces")))
            .parse_next(input)?;
        match self {
            InstrType::Labl(interop) => {
                let labl = cut_err(parse_label_name)
                    .context(StrContext::Expected(StrContextValue::StringLiteral("<Label>")))
                    .parse_next(input)?;

                Ok(match interop {
                    IntermediateOp::Call => MacroInstr::Call(labl.into()).into(),
                    IntermediateOp::Tail => MacroInstr::Tail(labl.into()).into(),
                    IntermediateOp::J => MacroInstr::Jal(Reg::G0, labl.into()).into(),

                    op => unreachable!("[Error] Could not map parsed instruction to internal data structure: {:?}", op),
                })
            },
            InstrType::Reg(interop) => {
                let reg = parse_reg(input)?;

                Ok(match interop {
                    IntermediateOp::Jr => Instruction::Jalr(Reg::G0, reg, 0).into(),

                    op => unreachable!("[Error] Could not map parsed instruction to internal data structure: {:?}", op),
                })
            },
            InstrType::RegLabl(interop) => {
                let args = separated_pair(
                    parse_reg,
                    parse_seper,
                    cut_err(parse_label_name)
                        .context(StrContext::Expected(StrContextValue::StringLiteral("<Label>")))
                ).parse_next(input)?;

                Ok(match interop {
                    IntermediateOp::La => MacroInstr::La(args.0, args.1.into()).into(),

                    op => unreachable!("[Error] Could not map parsed instruction to internal data structure: {:?}", op),
                })
            },
            InstrType::RegImm(interop) => {
                let reg = terminated(parse_reg, parse_seper).parse_next(input)?;

                let imm = parse_symimm!(input);

                Ok(match interop {
                    IntermediateOp::Auipc => Instruction::Auipc(reg, imm << 12).into(),
                    IntermediateOp::Li => MacroInstr::Li(reg, imm).into(),
            
                    op => unreachable!("[Error] Could not map parsed instruction to internal data structure: {:?}", op),
                })
            },
            InstrType::Reg2(interop) => {
                let args = separated_pair(parse_reg, parse_seper, parse_reg).parse_next(input)?;

                Ok(match interop {
                    IntermediateOp::Mv => Instruction::Addi(args.0, args.1, 0).into(),
                    op  => unreachable!("[Error] Could not map parsed instruction to internal data structure: {:?}", op),
                })
            },
            InstrType::Reg2Labl(interop) => {
                let args = (
                    parse_reg,
                    preceded(parse_seper, parse_reg),
                    preceded(
                        parse_seper, 
                        cut_err(parse_label_name)
                            .context(StrContext::Expected(StrContextValue::StringLiteral("<Label>")))
                    )
                )
                .parse_next(input)?;

                Ok(match interop {
                    IntermediateOp::Sb => MacroInstr::SbLabl(args.0, args.1, args.2.into()).into(),
                    IntermediateOp::Sh => MacroInstr::ShLabl(args.0, args.1, args.2.into()).into(),
                    IntermediateOp::Sw => MacroInstr::SwLabl(args.0, args.1, args.2.into()).into(),
            
                    IntermediateOp::Addi => MacroInstr::Addi(args.0, args.1, args.2.into(), Part::None).into(),
            
                    op => unreachable!("[Error] Could not map parsed instruction to internal data structure: {:?}", op),
                })
            },
            InstrType::Reg2Imm(interop) => {
                let args = (
                    parse_reg,
                    delimited(parse_seper, parse_reg, parse_seper)
                ).parse_next(input)?;

                let imm = parse_symimm!(input);

                Ok(match interop {
                    IntermediateOp::Slli => Instruction::Slli(args.0, args.1, imm).into(),
                    IntermediateOp::Srli => Instruction::Srli(args.0, args.1, imm).into(),
                    IntermediateOp::Srai => Instruction::Srai(args.0, args.1, imm).into(),
            
                    IntermediateOp::Addi => Instruction::Addi(args.0, args.1, imm).into(),

                    IntermediateOp::Slti => Instruction::Slti(args.0, args.1, imm).into(),
                    IntermediateOp::Sltiu => Instruction::Sltiu(args.0, args.1, imm).into(),
                    IntermediateOp::Xori => Instruction::Xori(args.0, args.1, imm).into(),
                    IntermediateOp::Ori => Instruction::Ori(args.0, args.1, imm).into(),
                    IntermediateOp::Andi => Instruction::Andi(args.0, args.1, imm).into(),

                    IntermediateOp::Srr => MacroInstr::Srr(args.0, args.1, imm).into(),
                    IntermediateOp::Slr => MacroInstr::Slr(args.0, args.1, imm).into(),

                    op => unreachable!("[Error] Could not map parsed instruction to internal data structure: {:?}", op),
                })
            },
            InstrType::Reg3(interop) => {
                let args = (
                    parse_reg,
                    preceded(parse_seper, parse_reg),
                    preceded(parse_seper, parse_reg))
                .parse_next(input)?;

                Ok(match interop {
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
                })
            },
            InstrType::RegVar(interop) => {
                let args = separated(1.., parse_reg, backtrack_err(parse_seper))
                    .context(StrContext::Expected(StrContextValue::Description("one or more registers")))
                    .parse_next(input)?;

                Ok(match interop {
                    IntermediateOp::Push => MacroInstr::Push(args).into(),
                    IntermediateOp::Pop => MacroInstr::Pop(args).into(),

                    op => unreachable!("[Error] Could not map parsed instruction to internal data structure: {:?}", op),
                })
            },
            InstrType::StoreOp(interop) => {
                let treg = terminated(parse_reg, parse_seper).parse_next(input)?;

                let imm = parse_symimm!(opt, input);

                match imm {
                    Some(offset) => { // s{b|w|h} x[0-31], IMM
                        if let Some(mem_reg) = opt(parse_delimited_reg).parse_next(input)? { // s{b|w|h} x[0-31], IMM(x[0-31])
                            Ok(match interop {
                                IntermediateOp::Sb => Instruction::Sb(treg, mem_reg, offset),
                                IntermediateOp::Sh => Instruction::Sh(treg, mem_reg, offset),
                                IntermediateOp::Sw => Instruction::Sw(treg, mem_reg, offset),
            
                                op => unreachable!("[Error] Could not map parsed instruction to internal data structure: {:?}", op),
                            }.into())
                        } else { // s{b|w|h} x[0-31], IMM OR s{b|w|h} x[0-31], IMM, x[0-31]
                            if !(-2048..=0b01_11111_11111_i32).contains(&offset) {
                                let temp_reg = preceded(parse_seper, parse_reg)
                                    .context(StrContext::Expected(StrContextValue::Description("temp register needed for immediate that large")))
                                    .parse_next(input)?;

                                Ok(match interop {
                                    IntermediateOp::Sb => MacroInstr::SbImm(treg, temp_reg, offset),
                                    IntermediateOp::Sh => MacroInstr::ShImm(treg, temp_reg, offset),
                                    IntermediateOp::Sw => MacroInstr::SwImm(treg, temp_reg, offset),
                
                                    op => unreachable!("[Error] Could not map parsed instruction to internal data structure: {:?}", op),
                                }.into())
                            } else {
                                Ok(match interop {
                                    IntermediateOp::Sb => Instruction::Sb(treg, Reg::G0, offset),
                                    IntermediateOp::Sh => Instruction::Sh(treg, Reg::G0, offset),
                                    IntermediateOp::Sw => Instruction::Sw(treg, Reg::G0, offset),
                
                                    op => unreachable!("[Error] Could not map parsed instruction to internal data structure: {:?}", op),
                                }.into())
                            }
                        }
                    },
                    None => { // s{b|w|h} x[0-31], 
                        if let Some(mem_reg) = opt(parse_delimited_reg).parse_next(input)? { // s{b|w|h} x[0-31], (x[0-31])
                            Ok(match interop {
                                IntermediateOp::Sb => Instruction::Sb(treg, mem_reg, 0),
                                IntermediateOp::Sh => Instruction::Sh(treg, mem_reg, 0),
                                IntermediateOp::Sw => Instruction::Sw(treg, mem_reg, 0),
            
                                op => unreachable!("[Error] Could not map parsed instruction to internal data structure: {:?}", op),
                            }.into())
                        } else {
                            let (label, temp_reg) = separated_pair(
                                cut_err(parse_label_name)
                                    .context(StrContext::Expected(StrContextValue::StringLiteral("<Label> OR (<Reg>) OR <Imm>"))),
                                parse_seper,
                                parse_reg
                            ).parse_next(input)?;
                            Ok(match interop {
                                IntermediateOp::Sb => MacroInstr::SbLabl(treg, temp_reg, label.into()),
                                IntermediateOp::Sh => MacroInstr::ShLabl(treg, temp_reg, label.into()),
                                IntermediateOp::Sw => MacroInstr::SwLabl(treg, temp_reg, label.into()),
            
                                op => unreachable!("[Error] Could not map parsed instruction to internal data structure: {:?}", op),
                            }.into()) // s{b|w|h} x[0-31], LABEL, x[0-31]
                        }
                    },
                }
            },
            InstrType::LoadOp(interop) => {
                let treg = terminated(parse_reg, parse_seper).parse_next(input)?;

                let imm = parse_symimm!(opt, input);

                match imm {
                    Some(offset) => { // l{b|w|h|bu|hu} x[0-31], IMM
                        if let Some(mem_reg) = opt(parse_delimited_reg).parse_next(input)? { // l{b|w|h|bu|hu} x[0-31], IMM(x[0-31])
                            Ok(match interop {
                                IntermediateOp::Lb => Instruction::Lb(treg, mem_reg, offset),
                                IntermediateOp::Lbu => Instruction::Lbu(treg, mem_reg, offset),
                                IntermediateOp::Lh => Instruction::Lh(treg, mem_reg, offset),
                                IntermediateOp::Lhu => Instruction::Lhu(treg,mem_reg, offset),
                                IntermediateOp::Lw => Instruction::Lw(treg, mem_reg, offset),
            
                                op => unreachable!("[Error] Could not map parsed instruction to internal data structure: {:?}", op),
                            }.into())
                        } else { // l{b|w|h|bu|hu} x[0-31], IMM
                            if !(-2048..=0b01_11111_11111_i32).contains(&offset) {
                                Ok(match interop {
                                    IntermediateOp::Lb => MacroInstr::LbImm(treg, treg, offset),
                                    IntermediateOp::Lbu => MacroInstr::LbuImm(treg, treg, offset),
                                    IntermediateOp::Lh => MacroInstr::LhImm(treg, treg, offset),
                                    IntermediateOp::Lhu => MacroInstr::LhuImm(treg, treg, offset),
                                    IntermediateOp::Lw => MacroInstr::LwImm(treg, treg, offset),
                
                                    op => unreachable!("[Error] Could not map parsed instruction to internal data structure: {:?}", op),
                                }.into())
                            } else {
                                Ok(match interop {
                                    IntermediateOp::Lb => Instruction::Lb(treg, Reg::G0, offset),
                                    IntermediateOp::Lbu => Instruction::Lbu(treg, Reg::G0, offset),
                                    IntermediateOp::Lh => Instruction::Lh(treg, Reg::G0, offset),
                                    IntermediateOp::Lhu => Instruction::Lhu(treg, Reg::G0, offset),
                                    IntermediateOp::Lw => Instruction::Lw(treg, Reg::G0, offset),
                
                                    op => unreachable!("[Error] Could not map parsed instruction to internal data structure: {:?}", op),
                                }.into())
                            }
                        }
                    },
                    None => { // l{b|w|h|bu|hu} x[0-31], 
                        if let Some(mem_reg) = opt(parse_delimited_reg).parse_next(input)? { // l{b|w|h|bu|hu} x[0-31], (x[0-31])
                            Ok(match interop {
                                IntermediateOp::Lb => Instruction::Lb(treg, mem_reg, 0),
                                IntermediateOp::Lbu => Instruction::Lbu(treg, mem_reg, 0),
                                IntermediateOp::Lh => Instruction::Lh(treg, mem_reg, 0),
                                IntermediateOp::Lhu => Instruction::Lhu(treg,mem_reg, 0),
                                IntermediateOp::Lw => Instruction::Lw(treg, mem_reg, 0),
            
                                op => unreachable!("[Error] Could not map parsed instruction to internal data structure: {:?}", op),
                            }.into())
                        } else if let Some(label) = opt(parse_label_name).parse_next(input)? {
                            Ok(match interop {
                                IntermediateOp::Lb => MacroInstr::LbLabl(treg, treg, label.into(), Part::None),
                                IntermediateOp::Lbu => MacroInstr::LbuLabl(treg, treg, label.into()),
                                IntermediateOp::Lh => MacroInstr::LhLabl(treg, treg, label.into(), Part::None),
                                IntermediateOp::Lhu => MacroInstr::LhuLabl(treg, treg, label.into()),
                                IntermediateOp::Lw => MacroInstr::LwLabl(treg, treg, label.into(), Part::None),
            
                                op => unreachable!("[Error] Could not map parsed instruction to internal data structure: {:?}", op),
                            }.into()) // l{b|w|h|bu|hu} x[0-31], LABEL
                        } else {
                            let (label, mem_reg) = cut_err((
                                parse_low_label,
                                delimited(
                                    '(',
                                    parse_reg,
                                    ')'
                                )
                            ))
                            .context(StrContext::Expected(StrContextValue::StringLiteral("(<Reg>) OR <Label> OR %lo(<Label>)(<Reg>)")))
                            .parse_next(input)?; // l{b|w|h|bu|hu} x[0-31], %lo(LABEL)(x[0-31])

                            match interop {
                                IntermediateOp::Lbu |
                                IntermediateOp::Lhu => {
                                    cut_err(fail)
                                        .context(StrContext::Label("argument: %lo(<Label>)(<Reg>) not allowed"))
                                        .parse_next(input)
                                },
                                IntermediateOp::Lb => Ok(MacroInstr::LbLabl(treg, mem_reg, label.into(), Part::Lower).into()),
                                IntermediateOp::Lh => Ok(MacroInstr::LhLabl(treg, mem_reg, label.into(), Part::Lower).into()),
                                IntermediateOp::Lw => Ok(MacroInstr::LwLabl(treg, mem_reg, label.into(), Part::Lower).into()),

                                op => unreachable!("[Error] Could not map parsed instruction to internal data structure: {:?}", op),
                            }
                        }
                    },
                }
            },
            InstrType::SpecOp(interop) => {
                let reg1 = terminated(parse_reg, parse_seper).parse_next(input)?;

                match interop {
                    IntermediateOp::Addi => {
                        let reg2 = terminated(parse_reg, parse_seper).parse_next(input)?;
        
                        let imm = parse_symimm!(opt, input);

                        if let Some(imm) = imm {
                            Ok(Instruction::Addi(reg1, reg2, imm).into())
                        } else {
                            let label = cut_err(parse_low_label)
                                .context(StrContext::Expected(StrContextValue::StringLiteral("%lo(<Label>) OR <Imm>")))
                                .parse_next(input)?;

                            Ok(MacroInstr::Addi(reg1, reg2, label.into(), Part::None).into())
                        }
                    },
                    IntermediateOp::Lui => {
                        let imm = parse_symimm!(opt, input);

                        if let Some(imm) = imm {
                            Ok(Instruction::Lui(reg1, imm << 12).into())
                        } else {
                            let label = cut_err(parse_high_label)
                                .context(StrContext::Expected(StrContextValue::StringLiteral("%hi(<Label>) OR <Imm>")))
                                .parse_next(input)?;

                            Ok(MacroInstr::Lui(reg1, label.into(), Part::None).into())
                        }
                    },

                    op => unreachable!("[Error] Could not map parsed instruction to internal data structure: {:?}", op),
                }
            },
            InstrType::Oper => {
                separated_pair(cut_err(digit1.try_map(str::parse))
                    .context(StrContext::Expected(StrContextValue::Description("a positive number"))),
                parse_seper,
                cut_err(parse_instruction)
                .context(StrContext::Expected(StrContextValue::StringLiteral("<Macro> OR <Instruction>"))))
                .try_map(
                |(reps, instr)| {
                    match instr {
                        Operation::Namespace(_) |
                        Operation::LablMacro(_, _) |
                        Operation::LablInstr(_, _) |
                        Operation::Labl(_) => unreachable!(),
                        Operation::Instr(instr_in) => Ok(MacroInstr::RepInstr(reps, instr_in).into()),
                        Operation::Macro(macro_in) => {
                            match macro_in {
                                MacroInstr::RepInstr(_, _) |
                                MacroInstr::RepMacro(_, _) => Err(ParserError::NestedRepeat),
                                op => Ok(MacroInstr::RepMacro(reps, Box::new(op)).into()),
                            }
                        },
                    }
                }
                ).parse_next(input)
            },
            InstrType::Reg2LablOrImm(interop) => {
                let regs = (
                    parse_reg,
                    delimited(parse_seper, parse_reg, parse_seper)
                ).parse_next(input)?;

                let imm = parse_symimm!(opt, input);

                match imm {
                    Some(imm) => Ok(match interop {
                        IntermediateOp::Beq => Instruction::Beq(regs.0, regs.1, imm),
                        IntermediateOp::Bne => Instruction::Bne(regs.0, regs.1, imm),
                        IntermediateOp::Blt => Instruction::Blt(regs.0, regs.1, imm),
                        IntermediateOp::Bltu => Instruction::Bltu(regs.0, regs.1, imm),
                        IntermediateOp::Bge => Instruction::Bge(regs.0, regs.1, imm),
                        IntermediateOp::Bgeu => Instruction::Bgeu(regs.0, regs.1, imm),

                        op => unreachable!("[Error] Could not map parsed instruction to internal data structure: {:?}", op),
                    }.into()),
                    None => {
                        let labl = cut_err(parse_label_name)
                            .context(StrContext::Expected(StrContextValue::StringLiteral("<Label> OR <Symbol> OR <Imm>")))
                            .output_into()
                            .parse_next(input)?;

                        Ok(match interop {
                            IntermediateOp::Beq => MacroInstr::Beq(regs.0, regs.1, labl).into(),
                            IntermediateOp::Bne => MacroInstr::Bne(regs.0, regs.1, labl).into(),
                            IntermediateOp::Blt => MacroInstr::Blt(regs.0, regs.1, labl).into(),
                            IntermediateOp::Bltu => MacroInstr::Bltu(regs.0, regs.1, labl).into(),
                            IntermediateOp::Bge => MacroInstr::Bge(regs.0, regs.1, labl).into(),
                            IntermediateOp::Bgeu => MacroInstr::Bgeu(regs.0, regs.1, labl).into(),

                            op => unreachable!("[Error] Could not map parsed instruction to internal data structure: {:?}", op),
                        })
                    },
                }
            },
            // Labl, 1labl1reg, 1imm1reg
            InstrType::Jal => {
                if let Some(base_reg) = opt(terminated(backtrack_err(parse_reg), parse_seper)).parse_next(input)? {
                    let imm = parse_symimm!(opt, input);

                    match imm {
                        Some(val) => Ok(Instruction::Jal(base_reg, val).into()),
                        None => {
                            let labl = cut_err(parse_label_name)
                                .context(StrContext::Expected(StrContextValue::StringLiteral("<Label> OR <Reg>")))
                                .output_into()
                                .parse_next(input)?;

                            Ok(MacroInstr::Jal(base_reg, labl).into())
                        },
                    }
                } else {
                    let labl = cut_err(parse_label_name)
                        .context(StrContext::Expected(StrContextValue::StringLiteral("<Label> OR <Reg>")))
                        .output_into()
                        .parse_next(input)?;

                    Ok(MacroInstr::Jal(Reg::G1, labl).into())
                }
            },
            // Reg, 1imm2reg
            InstrType::Jalr => {
                let (base_reg, test) = (parse_reg, opt(
                    preceded(backtrack_err(parse_seper), terminated(parse_reg, backtrack_err(parse_seper)))
                )).parse_next(input)?;

                match test {
                    Some(reg2) => {
                        let imm = parse_symimm!(input);

                        Ok(Instruction::Jalr(base_reg, reg2, imm).into())
                    },
                    None => Ok(Instruction::Jalr(Reg::G1, base_reg, 0).into()),
                }
            },
            InstrType::NoArg(_) => unreachable!(),
        }
    }
}

#[derive(Clone, Debug)]
enum IntermediateOp {
    Nop, Ret, Ebreak, Ecall,
    Call, Tail,
    J, Jr,
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

pub fn parse_seper<'a>(input: &mut &'a str) -> PResult<&'a str> {
    cut_err((
        ',',
        opt(' ')
    ))
    .recognize()
    .context(StrContext::Expected(StrContextValue::CharLiteral(',')))
    .parse_next(input)
}

fn parse_low_label<'a>(input: &mut &'a str) -> PResult<&'a str> {
    preceded(
        literal("%lo"),
        delimited(
            '(',
            parse_label_name,
            ')'
        )
    ).parse_next(input)
}

fn parse_high_label<'a>(input: &mut &'a str) -> PResult<&'a str> {
    preceded(
        literal("%hi"),
        delimited(
            '(',
            parse_label_name,
            ')'
        )
    ).parse_next(input)
}

pub fn parse_instruction(input: &mut &str) -> PResult<Operation> {
    dispatch!{take_while(1.., AsChar::is_alpha);
        "nop" => empty.value(InstrType::NoArg(IntermediateOp::Nop)),
        "ret" => empty.value(InstrType::NoArg(IntermediateOp::Ret)),
        "ebreak" => empty.value(InstrType::NoArg(IntermediateOp::Ebreak)),
        "ecall" => empty.value(InstrType::NoArg(IntermediateOp::Ecall)),

        "call" => empty.value(InstrType::Labl(IntermediateOp::Call)),
        "tail" => empty.value(InstrType::Labl(IntermediateOp::Tail)),
        "jal" => empty.value(InstrType::Jal),
        "j" => empty.value(InstrType::Labl(IntermediateOp::J)),

        "jr" => empty.value(InstrType::Reg(IntermediateOp::Jr)),
        "jalr" => empty.value(InstrType::Jalr),

        "la" => empty.value(InstrType::RegLabl(IntermediateOp::La)),

        "lui" => empty.value(InstrType::SpecOp(IntermediateOp::Lui)),
        "auipc" => empty.value(InstrType::RegImm(IntermediateOp::Auipc)),
        "li" => empty.value(InstrType::RegImm(IntermediateOp::Li)),

        "mv" => empty.value(InstrType::Reg2(IntermediateOp::Mv)),

        "beq" => empty.value(InstrType::Reg2LablOrImm(IntermediateOp::Beq)),
        "bne" => empty.value(InstrType::Reg2LablOrImm(IntermediateOp::Bne)),
        "bltu" => empty.value(InstrType::Reg2LablOrImm(IntermediateOp::Bltu)),
        "bgeu" => empty.value(InstrType::Reg2LablOrImm(IntermediateOp::Bgeu)),
        "blt" => empty.value(InstrType::Reg2LablOrImm(IntermediateOp::Blt)),
        "bge" => empty.value(InstrType::Reg2LablOrImm(IntermediateOp::Bge)),

        "slli" => empty.value(InstrType::Reg2Imm(IntermediateOp::Slli)),
        "srli" => empty.value(InstrType::Reg2Imm(IntermediateOp::Srli)),
        "srai" => empty.value(InstrType::Reg2Imm(IntermediateOp::Srai)),

        "sltiu" => empty.value(InstrType::Reg2Imm(IntermediateOp::Sltiu)),
        "slti" => empty.value(InstrType::Reg2Imm(IntermediateOp::Slti)),
        "xori" => empty.value(InstrType::Reg2Imm(IntermediateOp::Xori)),
        "ori" => empty.value(InstrType::Reg2Imm(IntermediateOp::Ori)),
        "andi" => empty.value(InstrType::Reg2Imm(IntermediateOp::Andi)),
        "srr" => empty.value(InstrType::Reg2Imm(IntermediateOp::Srr)),
        "slr" => empty.value(InstrType::Reg2Imm(IntermediateOp::Slr)),

        "addi" => empty.value(InstrType::SpecOp(IntermediateOp::Addi)),

        "add" => empty.value(InstrType::Reg3(IntermediateOp::Add)),
        "sub" => empty.value(InstrType::Reg3(IntermediateOp::Sub)),
        "xor" => empty.value(InstrType::Reg3(IntermediateOp::Xor)),
        "or" => empty.value(InstrType::Reg3(IntermediateOp::Or)),
        "and" => empty.value(InstrType::Reg3(IntermediateOp::And)),
        "sltu" => empty.value(InstrType::Reg3(IntermediateOp::Sltu)),
        "slt" => empty.value(InstrType::Reg3(IntermediateOp::Slt)),
        "sll" => empty.value(InstrType::Reg3(IntermediateOp::Sll)),
        "srl" => empty.value(InstrType::Reg3(IntermediateOp::Srl)),
        "sra" => empty.value(InstrType::Reg3(IntermediateOp::Sra)),
        "mulhsu" => empty.value(InstrType::Reg3(IntermediateOp::Mulhsu)),
        "mulhu" => empty.value(InstrType::Reg3(IntermediateOp::Mulhu)),
        "mulh" => empty.value(InstrType::Reg3(IntermediateOp::Mulh)),
        "mul" => empty.value(InstrType::Reg3(IntermediateOp::Mul)),
        "divu" => empty.value(InstrType::Reg3(IntermediateOp::Divu)),
        "div" => empty.value(InstrType::Reg3(IntermediateOp::Div)),
        "remu" => empty.value(InstrType::Reg3(IntermediateOp::Remu)),
        "rem" => empty.value(InstrType::Reg3(IntermediateOp::Rem)),
        "xnor" => empty.value(InstrType::Reg3(IntermediateOp::Xnor)),
        "eq" => empty.value(InstrType::Reg3(IntermediateOp::Equal)),
        "nor" => empty.value(InstrType::Reg3(IntermediateOp::Nor)),

        "sb" => empty.value(InstrType::StoreOp(IntermediateOp::Sb)),
        "sh" => empty.value(InstrType::StoreOp(IntermediateOp::Sh)),
        "sw" => empty.value(InstrType::StoreOp(IntermediateOp::Sw)),

        "lbu" => empty.value(InstrType::LoadOp(IntermediateOp::Lbu)),
        "lhu" => empty.value(InstrType::LoadOp(IntermediateOp::Lhu)),
        "lb" => empty.value(InstrType::LoadOp(IntermediateOp::Lb)),
        "lh" => empty.value(InstrType::LoadOp(IntermediateOp::Lh)),
        "lw" => empty.value(InstrType::LoadOp(IntermediateOp::Lw)),

        "push" => empty.value(InstrType::RegVar(IntermediateOp::Push)),
        "pop" => empty.value(InstrType::RegVar(IntermediateOp::Pop)),

        "rep" => empty.value(InstrType::Oper),

        _ => fail.context(StrContext::Label("unknown instruction")),
    }.parse_next(input)?.translate_parse(input)
}

#[cfg(test)]
mod tests {
    use crate::asm::parser::symbols::Symbols;

    use super::*;

    fn setup_symbols(symbol: smartstring::alias::String, def: i128) {
        Symbols::symbols_clear();
        Symbols::symbols_write(symbol, def);
    }

    #[test]
    fn test_parse_seper() {
        assert_ne!(parse_seper(&mut "invalid"), Ok(""));
        assert_ne!(parse_seper(&mut " "), Ok(""));
        assert_eq!(parse_seper(&mut ", "), Ok(", "));
        assert_eq!(parse_seper(&mut ","), Ok(","));
    }
    
    #[test]
    fn test_parse_instrnoparam() {
        assert_ne!(parse_instruction(&mut "invalid"), Ok(Operation::Instr(Instruction::Addi(Reg::G0, Reg::G0, 0))));
        assert_ne!(parse_instruction(&mut "noop"), Ok(Operation::Instr(Instruction::Addi(Reg::G0, Reg::G0, 0))));
        assert_eq!(parse_instruction(&mut "nop"), Ok(Operation::Instr(Instruction::Addi(Reg::G0, Reg::G0, 0))));
        //assert_ne!(parse_macro_noparm(&mut "nop x1"), Ok(Operation::Instr(Instruction::Addi(Reg::G0, Reg::G0, 0))));
        assert_eq!(parse_instruction(&mut "ret"), Ok(Operation::Instr(Instruction::Jalr(Reg::G0, Reg::G1, 0))));
        //assert_ne!(parse_macro_noparm(&mut "ret nop"), Ok(Operation::Instr(Instruction::Jalr(Reg::G0, Reg::G1, 0))));
    }

    #[test]
    fn test_parse_instr1labl() {
        assert_ne!(parse_instruction(&mut "invalid"), Ok(Instruction::Addi(Reg::G0, Reg::G0, 0).into()));
        assert_ne!(parse_instruction(&mut " "), Ok(Instruction::Addi(Reg::G0, Reg::G0, 0).into()));
        assert_ne!(parse_instruction(&mut ""), Ok(Instruction::Addi(Reg::G0, Reg::G0, 0).into()));
        assert_ne!(parse_instruction(&mut "call"), Ok(MacroInstr::Call("".into()).into()));
        assert_eq!(parse_instruction(&mut "tail test"), Ok(MacroInstr::Tail("test".into()).into()));
        assert_eq!(parse_instruction(&mut "call HANS"), Ok(MacroInstr::Call("HANS".into()).into()));
        //assert_ne!(parse_macro_1labl(&mut "call label  "), Ok(MacroInstr::Call("label".into()).into()));
    }

    #[test]
    fn test_parse_instr1reg() {
        assert_ne!(parse_instruction(&mut "invalid"), Ok(Instruction::Addi(Reg::G0, Reg::G0, 0).into()));
        assert_ne!(parse_instruction(&mut " "), Ok(Instruction::Addi(Reg::G0, Reg::G0, 0).into()));
        assert_ne!(parse_instruction(&mut ""), Ok(Instruction::Addi(Reg::G0, Reg::G0, 0).into()));
        assert_ne!(parse_instruction(&mut "jr"), Ok(Instruction::Jalr(Reg::G0, Reg::G0, 0).into()));
        assert_eq!(parse_instruction(&mut "jalr a2"), Ok(Instruction::Jalr(Reg::G1, Reg::str_to_enum("a2").unwrap(), 0).into()));
        assert_eq!(parse_instruction(&mut "jr x18"), Ok(Instruction::Jalr(Reg::G0, Reg::G18, 0).into()));
        //assert_ne!(parse_macro_1reg(&mut "jalr x19  "), Ok(Instruction::Jalr(Reg::G1, Reg::G19, 0).into()));
    }

    #[test]
    fn test_parse_instr1labl1reg() {
        assert_ne!(parse_instruction(&mut ""), Ok(Instruction::Addi(Reg::G0, Reg::G0, 0).into()));
        assert_ne!(parse_instruction(&mut "auipc s2, helloWorld"), Ok(MacroInstr::Auipc(Reg::G18, "helloWorld".into()).into()));
        assert_eq!(parse_instruction(&mut "jal   x20, test"), Ok(MacroInstr::Jal(Reg::G20, "test".into()).into()));
        //assert_ne!(parse_macro_1labl1reg(&mut "jal x19, train "), Ok(MacroInstr::Jal(Reg::G19, "train".into()).into()));
        assert_eq!(parse_instruction(&mut "la x19, HELLOWORLD"), Ok(MacroInstr::La(Reg::G19, "HELLOWORLD".into()).into()));
    }

    #[test]
    fn test_parse_instr1imm1reg() {
        assert_ne!(parse_instruction(&mut "invalid"), Ok(Instruction::Addi(Reg::G0, Reg::G0, 0).into()));
        assert_ne!(parse_instruction(&mut " "), Ok(Instruction::Addi(Reg::G0, Reg::G0, 0).into()));
        assert_ne!(parse_instruction(&mut ""), Ok(Instruction::Addi(Reg::G0, Reg::G0, 0).into()));
        assert_ne!(parse_instruction(&mut "lb"), Ok(Instruction::Lb(Reg::G0, Reg::G0, 0).into()));
        assert_ne!(parse_instruction(&mut ""), Ok(Instruction::Addi(Reg::G0, Reg::G0, 0).into()));
        assert_ne!(parse_instruction(&mut "lui"), Ok(Instruction::Lui(Reg::G0, 0).into()));
        assert_eq!(parse_instruction(&mut "lui x12, 12"), Ok(Instruction::Lui(Reg::G12, 49152).into()));
        assert_eq!(parse_instruction(&mut "lui a2, %hi(stop)"), Ok(MacroInstr::Lui(Reg::G12, "stop".into(), Part::None).into()));
        assert_eq!(parse_instruction(&mut "auipc x18, 0x20"), Ok(Instruction::Auipc(Reg::G18, 131072).into()));
        assert_eq!(parse_instruction(&mut "jal x20, 5"), Ok(Instruction::Jal(Reg::G20, 5).into()));
        //assert_ne!(parse_inst_1imm1reg(&mut "jal x19, 125 "), Ok(Instruction::Jal(Reg::G19, 125).into()));
        assert_ne!(parse_instruction(&mut "la x19, 0x0F"), Ok(MacroInstr::La(Reg::G19, "0x0F".into()).into()));

        setup_symbols("TEST".into(), 25);

        assert_eq!(parse_instruction(&mut "li  x11, TEST"), Ok(MacroInstr::Li(Reg::G11, 25).into()));
        assert_eq!(parse_instruction(&mut "lui x15, TEST"), Ok(Instruction::Lui(Reg::G15, 102400).into()));
        assert_ne!(parse_instruction(&mut "auipc x15, .TEST"), Ok(Instruction::Auipc(Reg::G15, 102400).into()));
    }

    #[test]
    fn test_parse_inst_2reg() {
        assert_ne!(parse_instruction(&mut "invalid"), Ok(Instruction::Addi(Reg::G0, Reg::G0, 0).into()));
        assert_ne!(parse_instruction(&mut "   "), Ok(Instruction::Addi(Reg::G0, Reg::G0, 0).into()));
        assert_eq!(parse_instruction(&mut "lb x1, 0xAA"), Ok(Instruction::Lb(Reg::G1, Reg::G0, 0xAA).into()));
        assert_ne!(parse_instruction(&mut "mv x1, 0xAA"), Ok(Instruction::Addi(Reg::G1, Reg::G0, 0xAA).into()));
        assert_eq!(parse_instruction(&mut "mv x1, x4"), Ok(Instruction::Addi(Reg::G1, Reg::G4, 0).into()));
        assert_eq!(parse_instruction(&mut "mv x12,x4"), Ok(Instruction::Addi(Reg::G12, Reg::G4, 0).into()));
    }

    #[test]
    fn test_parse_instr1labl2reg() {
        assert_ne!(parse_instruction(&mut "invalid"), Ok(Instruction::Addi(Reg::G0, Reg::G0, 0).into()));
        assert_ne!(parse_instruction(&mut "   "), Ok(Instruction::Addi(Reg::G0, Reg::G0, 0).into()));
        assert_eq!(parse_instruction(&mut "bgeu  x1, x4, sTaRt"), Ok(MacroInstr::Bgeu(Reg::G1, Reg::G4, "sTaRt".into()).into()));
        assert_eq!(parse_instruction(&mut "blt x10,x10, last"), Ok(MacroInstr::Blt(Reg::G10, Reg::G10, "last".into()).into()));
        assert_ne!(parse_instruction(&mut "jalr  x6,  x8,test"), Ok(MacroInstr::Jalr(Reg::G6, Reg::G8, "test".into(), Part::None).into()));
        assert_ne!(parse_instruction(&mut "beqx1x15,start"), Ok(MacroInstr::Beq(Reg::G1, Reg::G15, "start".into()).into()));
    }

    #[test]
    fn test_parse_inst_1imm2reg() {
        assert_ne!(parse_instruction(&mut "invalid"), Ok(Instruction::Addi(Reg::G0, Reg::G0, 0).into()));
        assert_ne!(parse_instruction(&mut "   "), Ok(Instruction::Addi(Reg::G0, Reg::G0, 0).into()));
        assert_ne!(parse_instruction(&mut "addi x1, x6"), Ok(Instruction::Addi(Reg::G1, Reg::G6, 0).into()));
        assert_eq!(parse_instruction(&mut "blt x1, x4, 5"), Ok(Instruction::Blt(Reg::G1, Reg::G4, 5).into()));
        assert_ne!(parse_instruction(&mut "bge x6,  x8,5"), Ok(Instruction::Bge(Reg::G6, Reg::G8, 5).into()));

        assert_eq!(parse_instruction(&mut "addi x1, x2, 0xAA"), Ok(Instruction::Addi(Reg::G1, Reg::G2, 0xAA).into()));
        assert_eq!(parse_instruction(&mut "srr   x13,x15,6"), Ok(MacroInstr::Srr(Reg::G13, Reg::G15, 6).into()));
        assert_ne!(parse_instruction(&mut "sltix1x15,6"), Ok(Instruction::Slti(Reg::G1, Reg::G15, 6).into()));
        assert_ne!(parse_instruction(&mut "slli x12, x15,  6"), Ok(Instruction::Slli(Reg::G12, Reg::G15, 6).into()));
        // TODO: More tests

        setup_symbols("HelloWorld".into(), 404);

        assert_eq!(parse_instruction(&mut "blt x1, x4, HelloWorld"), Ok(Instruction::Blt(Reg::G1, Reg::G4, 404).into()));
    }

    #[test]
    fn test_parse_mem_ops() {
        assert_eq!(parse_instruction(&mut "sb x10,51(x10)"), Ok(Instruction::Sb(Reg::G10, Reg::G10, 51).into()));
        assert_eq!(parse_instruction(&mut "sh x1, (x10)"), Ok(Instruction::Sh(Reg::G1, Reg::G10, 0).into()));
        assert_eq!(parse_instruction(&mut "sw x12, 12(x16)"), Ok(Instruction::Sw(Reg::G12, Reg::G16, 12).into()));

        assert_eq!(parse_instruction(&mut "sb x17, -12"), Ok(Instruction::Sb(Reg::G17, Reg::G0, -12).into()));
        assert_eq!(parse_instruction(&mut "sh x11, 1506"), Ok(Instruction::Sh(Reg::G11, Reg::G0, 1506).into()));
        assert_ne!(parse_instruction(&mut "sh x12, 15060"), Ok(Instruction::Sh(Reg::G12, Reg::G0, 15060).into()));
        assert_ne!(parse_instruction(&mut "sw x15, -12515"), Ok(Instruction::Sw(Reg::G15, Reg::G0, -12515).into()));
        
        assert_eq!(parse_instruction(&mut "sb x17, 151500, x10"), Ok(MacroInstr::SbImm(Reg::G17, Reg::G10, 151500).into()));
        assert_eq!(parse_instruction(&mut "sh x11, -151251, x17"), Ok(MacroInstr::ShImm(Reg::G11, Reg::G17, -151251).into()));
        assert_eq!(parse_instruction(&mut "sw x10, 15006, x2"), Ok(MacroInstr::SwImm(Reg::G10, Reg::G2, 15006).into()));
        
        assert_ne!(parse_instruction(&mut "sb x1, x6"), Ok(MacroInstr::SbLabl(Reg::G1, Reg::G6, "".into()).into()));
        assert_eq!(parse_instruction(&mut "sb x17, dasLabel, x10"), Ok(MacroInstr::SbLabl(Reg::G17, Reg::G10, "dasLabel".into()).into()));
        assert_eq!(parse_instruction(&mut "sh x13,loading,x15"), Ok(MacroInstr::ShLabl(Reg::G13, Reg::G15, "loading".into()).into()));
        assert_eq!(parse_instruction(&mut "sw x10, randa, x2"), Ok(MacroInstr::SwLabl(Reg::G10, Reg::G2, "randa".into()).into()));

        assert_eq!(parse_instruction(&mut "lbu x1, 0xAA"), Ok(Instruction::Lbu(Reg::G1, Reg::G0, 0xAA).into()));
        assert_ne!(parse_instruction(&mut "lb x1x4,0x6"), Ok(Instruction::Lb(Reg::G1, Reg::G4, 6).into()));
        assert_eq!(parse_instruction(&mut "lb x10,(x10)"), Ok(Instruction::Lb(Reg::G10, Reg::G10, 0).into()));
        assert_eq!(parse_instruction(&mut "lw x10, -1(x10)"), Ok(Instruction::Lw(Reg::G10, Reg::G10, -1).into()));
        assert_eq!(parse_instruction(&mut "lb x26, 0x500(zero)"), Ok(Instruction::Lb(Reg::G26, Reg::G0, 0x500).into()));
        assert_ne!(parse_instruction(&mut "lw x10,x10)"), Ok(Instruction::Lw(Reg::G10, Reg::G10, 0).into()));

        assert_ne!(parse_instruction(&mut "lb x1, total"), Ok(MacroInstr::LbLabl(Reg::G1, Reg::G0, "total".into(), Part::None).into()));
        assert_ne!(parse_instruction(&mut "lhu x1, x2, hans"), Ok(MacroInstr::LhuLabl(Reg::G1, Reg::G2, "hans".into()).into()));
        assert_ne!(parse_instruction(&mut "lbu x12, x15,  dasletzte"), Ok(MacroInstr::LbuLabl(Reg::G12, Reg::G15, "dasletzte".into()).into()));

        setup_symbols("HelloWorld".into(), 404);

        assert_eq!(parse_instruction(&mut "sw x10,HelloWorld(x10)"), Ok(Instruction::Sw(Reg::G10, Reg::G10, 404).into()));

        assert_eq!(parse_instruction(&mut "lbu x1, HelloWorld"), Ok(Instruction::Lbu(Reg::G1, Reg::G0, 404).into()));
        assert_ne!(parse_instruction(&mut "lb x1x4,0x6"), Ok(Instruction::Lb(Reg::G1, Reg::G4, 6).into()));
    }

    #[test]
    fn test_parse_inst_3reg() {
        assert_ne!(parse_instruction(&mut "invalid"), Ok(Instruction::Addi(Reg::G0, Reg::G0, 0).into()));
        assert_ne!(parse_instruction(&mut "   "), Ok(Instruction::Addi(Reg::G0, Reg::G0, 0).into()));
        assert_eq!(parse_instruction(&mut "addi x1, x6, 0xAA"), Ok(Instruction::Addi(Reg::G1, Reg::G6, 0xAA).into()));
        assert_ne!(parse_instruction(&mut "add x1, x2"), Ok(Instruction::Add(Reg::G1, Reg::G2, Reg::G0).into()));
        assert_eq!(parse_instruction(&mut "mul x1, x4, x6"), Ok(Instruction::Mul(Reg::G1, Reg::G4, Reg::G6).into()));
        assert_ne!(parse_instruction(&mut "div x10x14,x7"), Ok(Instruction::Div(Reg::G10, Reg::G14, Reg::G7).into()));
        assert_eq!(parse_instruction(&mut "xor x10,x11, x10"), Ok(Instruction::Xor(Reg::G10, Reg::G11, Reg::G10).into()));
        assert_ne!(parse_instruction(&mut "xnor x6,  x8,x5"), Ok(Instruction::Xnor(Reg::G6, Reg::G8, Reg::G5).into()));
        assert_eq!(parse_instruction(&mut "and x6, x8, x14"), Ok(Instruction::And(Reg::G6, Reg::G8, Reg::G14).into()));
        assert_ne!(parse_instruction(&mut "sll x6,  x8, x14"), Ok(Instruction::Sll(Reg::G6, Reg::G8, Reg::G14).into()));
    }

    #[test]
    fn test_parse_inst_multiarg() {
        assert_ne!(parse_instruction(&mut "push"), Ok(MacroInstr::Push(vec![]).into()));
        assert_eq!(parse_instruction(&mut "push x12"), Ok(MacroInstr::Push(Vec::from([
            Reg::G12
        ])).into()));
        assert_eq!(parse_instruction(&mut "push x12, x13, x14"), Ok(MacroInstr::Push(Vec::from([
            Reg::G12,
            Reg::G13,
            Reg::G14
        ])).into()));
        assert_eq!(parse_instruction(&mut "push   x12,x13,x14"), Ok(MacroInstr::Push(Vec::from([
            Reg::G12,
            Reg::G13,
            Reg::G14
        ])).into()));
        assert_ne!(parse_instruction(&mut "pop"), Ok(MacroInstr::Pop(vec![]).into()));
        assert_eq!(parse_instruction(&mut "pop x12"), Ok(MacroInstr::Pop(Vec::from([
            Reg::G12
        ])).into()));
        assert_eq!(parse_instruction(&mut "pop x12, x13, x14"), Ok(MacroInstr::Pop(Vec::from([
            Reg::G12,
            Reg::G13,
            Reg::G14
        ])).into()));
        assert_eq!(parse_instruction(&mut "pop   x12,x13,x14"), Ok(MacroInstr::Pop(Vec::from([
            Reg::G12,
            Reg::G13,
            Reg::G14
        ])).into()));
    }

    #[test]
    fn test_parse_instruction() {
        assert_eq!(parse_instruction(&mut "mv x1, x6"), Ok(Instruction::Addi(Reg::G1, Reg::G6, 0).into()));
        assert_ne!(parse_instruction(&mut "addi x1, 0xAA"), Ok(Instruction::Addi(Reg::G1, Reg::G0, 0xAA).into()));
        assert_eq!(parse_instruction(&mut "mul x1, x4, x6"), Ok(Instruction::Mul(Reg::G1, Reg::G4, Reg::G6).into()));
        assert_ne!(parse_instruction(&mut "xor x10x14,x7"), Ok(Instruction::Xor(Reg::G10, Reg::G14, Reg::G7).into()));
        assert_eq!(parse_instruction(&mut "add x10,x11, x10"), Ok(Instruction::Add(Reg::G10, Reg::G11, Reg::G10).into()));
        assert_ne!(parse_instruction(&mut "xnor x6,  x8,x5"), Ok(Instruction::Xnor(Reg::G6, Reg::G8, Reg::G5).into()));
        assert_eq!(parse_instruction(&mut "srr x5, x8, 7"), Ok(MacroInstr::Srr(Reg::G5, Reg::G8, 7).into()));
        assert_eq!(parse_instruction(&mut "rep 20, nop"), Ok(MacroInstr::RepInstr(20, Instruction::Addi(Reg::G0, Reg::G0, 0)).into()));
        // More tests & seperate
    }
}
