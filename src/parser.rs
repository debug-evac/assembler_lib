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
        map_res,
    },
    character::complete::{
        alphanumeric1,
        digit1,
        hex_digit1,
        multispace0,
        multispace1,
    },
    sequence::{
        tuple,
        separated_pair,
    },
};
use std::collections::{
    HashMap,
    HashSet
};

pub type Label = str;
pub type LabelStr = String;
pub type Imm = i32; // always less than 32

#[repr(u8)]
#[derive(Debug, Clone, PartialEq)]
pub enum Reg {
    G0, G1, G2, G3, G4, G5, G6, G7, G8, G9, G10, G11, G12, G13, G14, G15,
    NA,
}

impl Reg {
    pub fn match_to_enum(reg: &u8) -> Reg {
        match *reg {
            reg if reg == Reg::G0 as u8 => Reg::G0,
            reg if reg == Reg::G1 as u8 => Reg::G1,
            reg if reg == Reg::G2 as u8 => Reg::G2,
            reg if reg == Reg::G3 as u8 => Reg::G3,
            reg if reg == Reg::G4 as u8 => Reg::G4,
            reg if reg == Reg::G5 as u8 => Reg::G5,
            reg if reg == Reg::G6 as u8 => Reg::G6,
            reg if reg == Reg::G7 as u8 => Reg::G7,
            reg if reg == Reg::G8 as u8 => Reg::G8,
            reg if reg == Reg::G9 as u8 => Reg::G9,
            reg if reg == Reg::G10 as u8 => Reg::G10,
            reg if reg == Reg::G11 as u8 => Reg::G11,
            reg if reg == Reg::G12 as u8 => Reg::G12,
            reg if reg == Reg::G13 as u8 => Reg::G13,
            reg if reg == Reg::G14 as u8 => Reg::G14,
            reg if reg == Reg::G15 as u8 => Reg::G15,
            _ => Reg::NA,
        }
    }
}

// Possibly split Instruction to instruction enums with 1 imm, 1 reg and 1 imm and so on
// and implement a trait Instruction that must be implemented by all enums
// Then could parse only the instruction and the args separately thus greatly reducing
// code length and multiplication.
#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
    NA,

    // 1 Imm
    Jmp(Imm),
    Bt(Imm),
    Bf(Imm),

    VJmp(LabelStr),
    VBt(LabelStr),
    VBf(LabelStr),

    // 1 Reg & 1 Imm
    // Need VLd and VSt as well for variables
    Ld(Reg, Imm),
    St(Reg, Imm),

    Movu(Reg, Imm),
    Movl(Reg, Imm),

    // Shift left|right logical|arithmetic|rotate
    Sll(Reg, Imm),
    Srl(Reg, Imm),
    Sra(Reg, Imm),
    Sla(Reg, Imm),

    Srr(Reg, Imm),
    Slr(Reg, Imm),

    Sb(Reg, Imm),
    Ub(Reg, Imm),

    // 2 Regs
    Cmpe(Reg, Reg),
    Cmpg(Reg, Reg),
    Cmpl(Reg, Reg),

    Mov(Reg, Reg),

    // 2 Regs & 1 Imm
    Addi(Reg, Reg, Imm),
    Addci(Reg, Reg, Imm),

    Subi(Reg, Reg, Imm),
    Subci(Reg, Reg, Imm),

    Muli(Reg, Reg, Imm),
    Mulci(Reg, Reg, Imm),

    Divi(Reg, Reg, Imm),
    Divci(Reg, Reg, Imm),

    // 3 Regs
    Addn(Reg, Reg, Reg),
    Addcn(Reg, Reg, Reg),

    Subn(Reg, Reg, Reg),
    Subcn(Reg, Reg, Reg),

    Muln(Reg, Reg, Reg),
    Mulcn(Reg, Reg, Reg),

    Divn(Reg, Reg, Reg),
    Divcn(Reg, Reg, Reg),

    Xor(Reg, Reg, Reg),
    Or(Reg, Reg, Reg),
    And(Reg, Reg, Reg),
    Xnor(Reg, Reg, Reg),
    Nor(Reg, Reg, Reg),
}

pub struct LabelRecog {
    // local labels are preceded with a dot (.)
    label_map: Box<HashMap<LabelStr, u128>>,
    label_res_map: Box<HashMap<LabelStr, Vec<usize>>>,
    local_definitions: Box<HashSet<LabelStr>>,
    global_definitions: Box<HashSet<LabelStr>>,
}
/* 
impl LabelRecog {
    pub fn new() -> LabelRecog {
        let label_map: Box<HashMap<LabelStr, u128>> =
        Box::new(HashMap::new());
        let label_res_map: Box<HashMap<LabelStr, Vec<usize>>> =
        Box::new(HashMap::new());
        let local_definitions: Box<HashSet<LabelStr>> =
        Box::new(HashSet::new());
        let global_definitions: Box<HashSet<LabelStr>> =
        Box::new(HashSet::new());

        LabelRecog {
            label_map,
            label_res_map,
            local_definitions,
            global_definitions
        }
    }

    pub fn merge(&self, other: &LabelRecog) -> Result<LabelRecog, _> {
        let global_disjoint = self.global_definitions.is_disjoint(
            &*other.global_definitions);
        let gl_local_disjoint = self.global_definitions.is_disjoint(
            &*other.local_definitions);
        let local_disjoint = self.local_definitions.is_disjoint(
            &*other.local_definitions);

        if global_disjoint && gl_local_disjoint && local_disjoint {
            // no conflict
            let mut new_labelmap: HashMap<LabelStr, u128> = HashMap::new();
            let mut new_labelresmap: HashMap<LabelStr, Vec<usize>> = HashMap::new();

            for (local1, local2) in self.local_definitions.iter().zip(other.local_definitions.iter()) {
                let line = self.label_map.get(local1).expect("[Self] Label map does not include local label!");
                new_labelmap.insert(*local1, *line);
                let vec = self.label_res_map.get(local1);
                match vec {
                    Some(val) => {
                        new_labelresmap.insert(*local1, *val);
                    },
                    None => (),
                };
                let line = other.label_map.get(local2).expect("[Other] Label map does not include local label!");
                new_labelmap.insert(*local2, *line);
                let vec = other.label_res_map.get(local2);
                match vec {
                    Some(val) => {
                        new_labelresmap.insert(*local2, *val);
                    },
                    None => (),
                };
            };

            let label_map = Box::new(new_labelmap);
            let label_res_map = Box::new(new_labelresmap);

        }
       Ok(*self)
        //if self.label_definitions.is_disjoint(&*other.label_definitions) {

        //} else {

        //}
    }
}
*/


fn parse_label_name(input: &str) -> IResult<&str, &str> {
    alphanumeric1(input)
}

fn parse_label_definition(input: &str) -> IResult<&str, &str> {
    let (rest, parsed) = parse_label_name(input)?;
    let (rest, _) = tag(":")(rest)?;

    Ok((rest, parsed))
}

fn from_hex(input: &str) -> Result<Imm, std::num::ParseIntError> {
    Imm::from_str_radix(input, 16)
}

fn parse_imm(input: &str) -> IResult<&str, Imm> {
    let (rest, parsed) = opt(tag_no_case("0x"))(input)?;
    if parsed.is_none() {
        // Decimal
        map_res(digit1, str::parse)(rest)
    } else {
        // Hex
        map_res(hex_digit1, from_hex)(rest)
    }
}

fn parse_reg(input: &str) -> IResult<&str, Reg> {
    let (rest, _) = tag("$")(input)?;
    let (rest, reg) = map_res(digit1, str::parse)(rest)?;

    let real_reg = Reg::match_to_enum(&reg);

    if real_reg == Reg::NA {
        todo!("Implement own error!");
    } else {
        Ok((rest, real_reg))
    }
}

fn parse_seper(input: &str) -> IResult<&str, &str> {
    let (rest, not_needed) = tag(",")(input)?;
    let (rest, _) = opt(tag(" "))(rest)?;

    Ok((rest, not_needed))
}

fn parse_inst_1imm(input: &str) -> IResult<&str, Instruction> {
    let (rest, instr) = alt((
        value(Instruction::Jmp(0), tag("jmp")),
        value(Instruction::Bt(0), tag("bt")),
        value(Instruction::Bf(0), tag("bf")),
    ))(input)?;
    let (rest, _) = tag(" ")(rest)?;
    let (rest, label) = parse_label_name(rest)?;

    // TODO: Maybe also allow addresses and not only labels

    //let (rest, imm) = parse_imm(rest)?;

    let instr = match instr {
        Instruction::Jmp(_) => Instruction::VJmp(label.to_string()),
        Instruction::Bt(_) => Instruction::VBt(label.to_string()),
        Instruction::Bf(_) => Instruction::VBf(label.to_string()),
        _ => Instruction::NA
    };

    Ok((rest, instr))
}

fn parse_inst_1imm1reg(input: &str) -> IResult<&str, Instruction> {
    let (rest, instr) = alt((
        value(Instruction::Ld(Reg::NA, 0), tag("ld")),
        value(Instruction::St(Reg::NA, 0), tag("st")),
        value(Instruction::Movu(Reg::NA, 0), tag("movu")),
        value(Instruction::Movl(Reg::NA, 0), tag("movl")),

        value(Instruction::Sll(Reg::NA, 0), tag("sll")),
        value(Instruction::Srl(Reg::NA, 0), tag("srl")),
        value(Instruction::Sra(Reg::NA, 0), tag("sra")),
        value(Instruction::Sla(Reg::NA, 0), tag("sla")),

        value(Instruction::Srr(Reg::NA, 0), tag("srr")),
        value(Instruction::Slr(Reg::NA, 0), tag("slr")),
        value(Instruction::Sb(Reg::NA, 0), tag("sb")),
        value(Instruction::Ub(Reg::NA, 0), tag("ub")),
    ))(input)?;
    let (rest, _) = tag(" ")(rest)?;
    let (rest, args) = separated_pair(parse_reg, parse_seper, parse_imm)(rest)?;

    let instr = match instr {
        Instruction::Ld(_, _) => Instruction::Ld(args.0, args.1),
        Instruction::St(_, _) => Instruction::St(args.0, args.1),
        Instruction::Movu(_, _) => Instruction::Movu(args.0, args.1),
        Instruction::Movl(_, _) => Instruction::Movl(args.0, args.1),

        Instruction::Sll(_, _) => Instruction::Sll(args.0, args.1),
        Instruction::Srl(_, _) => Instruction::Srl(args.0, args.1),
        Instruction::Sra(_, _) => Instruction::Sra(args.0, args.1),
        Instruction::Sla(_, _) => Instruction::Sla(args.0, args.1),

        Instruction::Srr(_, _) => Instruction::Srr(args.0, args.1),
        Instruction::Slr(_, _) => Instruction::Slr(args.0, args.1),
        Instruction::Sb(_, _) => Instruction::Sb(args.0, args.1),
        Instruction::Ub(_, _) => Instruction::Ub(args.0, args.1),
        _ => Instruction::NA
    };

    Ok((rest, instr))
}

fn parse_inst_2reg(input: &str) -> IResult<&str, Instruction> {
    let (rest, instr) = alt((
        value(Instruction::Cmpe(Reg::NA, Reg::NA), tag("cmpe")),
        value(Instruction::Cmpg(Reg::NA, Reg::NA), tag("cmpg")),
        value(Instruction::Cmpl(Reg::NA, Reg::NA), tag("cmpl")),
        value(Instruction::Mov(Reg::NA, Reg::NA), tag("mov")),
    ))(input)?;
    let (rest, _) = tag(" ")(rest)?;
    let (rest, args) = separated_pair(parse_reg, parse_seper, parse_reg)(rest)?;

    let instr = match instr {
        Instruction::Cmpe(_, _) => Instruction::Cmpe(args.0, args.1),
        Instruction::Cmpg(_, _) => Instruction::Cmpg(args.0, args.1),
        Instruction::Cmpl(_, _) => Instruction::Cmpl(args.0, args.1),
        Instruction::Mov(_, _) => Instruction::Mov(args.0, args.1),
        _ => Instruction::NA
    };

    Ok((rest, instr))
}

fn parse_inst_1imm2reg(input: &str) -> IResult<&str, Instruction> {
    let (rest, instr) = alt((
        value(Instruction::Addi(Reg::NA, Reg::NA, 0), tag("addi")),
        value(Instruction::Addci(Reg::NA, Reg::NA, 0), tag("addci")),
        value(Instruction::Subi(Reg::NA, Reg::NA, 0), tag("subi")),
        value(Instruction::Subci(Reg::NA, Reg::NA, 0), tag("subci")),

        value(Instruction::Muli(Reg::NA, Reg::NA, 0), tag("muli")),
        value(Instruction::Mulci(Reg::NA, Reg::NA, 0), tag("mulci")),
        value(Instruction::Divi(Reg::NA, Reg::NA, 0), tag("divi")),
        value(Instruction::Divci(Reg::NA, Reg::NA, 0), tag("divci")),
    ))(input)?;
    let (rest, _) = tag(" ")(rest)?;
    let (rest, args) = tuple((parse_reg, parse_seper, parse_reg, parse_seper, parse_imm))(rest)?;

    let instr = match instr {
        Instruction::Addi(_, _, _) => Instruction::Addi(args.0, args.2, args.4),
        Instruction::Addci(_, _, _) => Instruction::Addci(args.0, args.2, args.4),
        Instruction::Subi(_, _, _) => Instruction::Subi(args.0, args.2, args.4),
        Instruction::Subci(_, _, _) => Instruction::Subci(args.0, args.2, args.4),

        Instruction::Muli(_, _, _) => Instruction::Muli(args.0, args.2, args.4),
        Instruction::Mulci(_, _, _) => Instruction::Mulci(args.0, args.2, args.4),
        Instruction::Divi(_, _, _) => Instruction::Divi(args.0, args.2, args.4),
        Instruction::Divci(_, _, _) => Instruction::Divci(args.0, args.2, args.4),
        _ => Instruction::NA
    };

    Ok((rest, instr))
}

fn parse_inst_3reg(input: &str) -> IResult<&str, Instruction> {
    let (rest, instr) = alt((
        value(Instruction::Addn(Reg::NA, Reg::NA, Reg::NA), tag("add")),
        value(Instruction::Addcn(Reg::NA, Reg::NA, Reg::NA), tag("addc")),

        value(Instruction::Subn(Reg::NA, Reg::NA, Reg::NA), tag("sub")),
        value(Instruction::Subcn(Reg::NA, Reg::NA, Reg::NA), tag("subc")),

        value(Instruction::Muln(Reg::NA, Reg::NA, Reg::NA), tag("mul")),
        value(Instruction::Mulcn(Reg::NA, Reg::NA, Reg::NA), tag("mulc")),

        value(Instruction::Divn(Reg::NA, Reg::NA, Reg::NA), tag("div")),
        value(Instruction::Divcn(Reg::NA, Reg::NA, Reg::NA), tag("divc")),

        value(Instruction::Xor(Reg::NA, Reg::NA, Reg::NA), tag("xor")),
        value(Instruction::Or(Reg::NA, Reg::NA, Reg::NA), tag("or")),
        value(Instruction::And(Reg::NA, Reg::NA, Reg::NA), tag("and")),
        value(Instruction::Xnor(Reg::NA, Reg::NA, Reg::NA), tag("xnor")),
        value(Instruction::Nor(Reg::NA, Reg::NA, Reg::NA), tag("nor")),
    ))(input)?;
    let (rest, _) = tag(" ")(rest)?;
    let (rest, args) = tuple((parse_reg, parse_seper, parse_reg, parse_seper, parse_reg))(rest)?;

    let instr = match instr {
        Instruction::Addn(_, _, _) => Instruction::Addn(args.0, args.2, args.4),
        Instruction::Addcn(_, _, _) => Instruction::Addcn(args.0, args.2, args.4),

        Instruction::Subn(_, _, _) => Instruction::Subn(args.0, args.2, args.4),
        Instruction::Subcn(_, _, _) => Instruction::Subcn(args.0, args.2, args.4),

        Instruction::Muln(_, _, _) => Instruction::Muln(args.0, args.2, args.4),
        Instruction::Mulcn(_, _, _) => Instruction::Mulcn(args.0, args.2, args.4),

        Instruction::Divn(_, _, _) => Instruction::Divn(args.0, args.2, args.4),
        Instruction::Divcn(_, _, _) => Instruction::Divcn(args.0, args.2, args.4),

        Instruction::Xor(_, _, _) => Instruction::Xor(args.0, args.2, args.4),
        Instruction::Or(_, _, _) => Instruction::Or(args.0, args.2, args.4),
        Instruction::And(_, _, _) => Instruction::And(args.0, args.2, args.4),
        Instruction::Xnor(_, _, _) => Instruction::Xnor(args.0, args.2, args.4),
        Instruction::Nor(_, _, _) => Instruction::Nor(args.0, args.2, args.4),
        _ => Instruction::NA
    };

    Ok((rest, instr))
}

/*
fn parse_instr(input: &str) -> IResult<&str, Instruction> {
    let (rest, parsed) = alt((
        value(Instr1Imm::Jmp(0), tag("jmp")),
        value(Instr1Reg1Imm::Ld(Reg::NA, 0), tag("ld"))
    ))(input)?;

    match parsed {
        Instruction::Muli(_, _, _) | Instruction::Muln(_, _, _) |
        Instruction::Mulcn(_, _, _) | Instruction::Mulci(_, _, _)
        => eprintln!("Warning! Mul and derivatives are not implemented yet!"),

        Instruction::Divi(_, _, _) | Instruction::Divn(_, _, _) |
        Instruction::Divcn(_, _, _) | Instruction::Divci(_, _, _)
        => eprintln!("Warning! Div and derivatives are not implemented yet!"),

        _ => (),
    };

    Ok((rest, parsed))
}*/

fn parse_instruction(input: &str) -> IResult<&str, Instruction> {
    alt((
        parse_inst_1imm,
        parse_inst_1imm1reg,
        parse_inst_1imm2reg,
        parse_inst_2reg,
        parse_inst_3reg
    ))(input)
}

fn parse_line(input: &str) -> IResult<&str, (Option<&Label>, Option<Instruction>)> {
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

fn correct_label_instr<'a>(
    label_map: &HashMap<String, u128>,
    label_res_map: &mut HashMap<LabelStr, Vec<usize>>,
    stack_pos: &usize,
    instr: &'a Instruction)
-> (Option<Instruction>, Option<&'a Instruction>) {
    match instr {
        Instruction::VJmp(label) => {
            let addr = label_map.get(label);
            match addr {
                Some(addr) => (Some(Instruction::Jmp(*addr as i32)), None),
                None => {
                    match label_res_map.get_mut(label) {
                        Some(val) => val.push(*stack_pos),
                        None => {
                            let mut list: Vec<usize> = vec![];
                            list.push(*stack_pos);
                            label_res_map.insert(label.clone(), list);
                        },
                    };
                    (None, Some(instr))
                },
            }
        },
        Instruction::VBt(label) => {
            let addr = label_map.get(label);
            match addr {
                Some(addr) => (Some(Instruction::Bt(*addr as i32)), None),
                None => {
                    match label_res_map.get_mut(label) {
                        Some(val) => val.push(*stack_pos),
                        None => {
                            let mut list: Vec<usize> = vec![];
                            list.push(*stack_pos);
                            label_res_map.insert(label.clone(), list);
                        },
                    };
                    (None, Some(instr))
                },
            }
        },
        Instruction::VBf(label) => {
            let addr = label_map.get(label);
            match addr {
                Some(addr) => (Some(Instruction::Bf(*addr as i32)), None),
                None => {
                    match label_res_map.get_mut(label) {
                        Some(val) => val.push(*stack_pos),
                        None => {
                            let mut list: Vec<usize> = vec![];
                            list.push(*stack_pos);
                            label_res_map.insert(label.clone(), list);
                        },
                    };
                    (None, Some(instr))
                },
            }
        },
        _ => (None, Some(instr)),
    }
    // TODO: Refactor
}

pub fn parse(input: &str) -> IResult<&str, Vec<Instruction>> {

    let mut label_res_map: HashMap<LabelStr, Vec<usize>> = HashMap::new();
    let mut label_map: HashMap<LabelStr, u128> = HashMap::new();
    let mut instr_list: Vec<Instruction> = vec![];
    let mut rest = input;

    loop {
        let res = parse_line(rest);

        let parsed = match res {
            Ok(line) => {
                rest = line.0;
                line.1
            },
            Err(_) => todo!("Custom parser error!"),
        };

        match parsed {
            (Some(label), Some(instr)) => {
                let addr = instr_list.len() as u128 * 4;
                label_map.insert(label.to_string(), addr);
                match label_res_map.get_mut(label) {
                    Some(val) => {
                        for pos in val.as_slice() {
                            match instr_list[*pos] {
                                Instruction::VJmp(_) => instr_list[*pos] = Instruction::Jmp(addr as i32),
                                Instruction::VBt(_) => instr_list[*pos] = Instruction::Bt(addr as i32),
                                Instruction::VBf(_) => instr_list[*pos] = Instruction::Bf(addr as i32),
                                _ => (),
                            }
                        };
                        val.clear();
                    }
                    None => (),
                }
                let option_instr = correct_label_instr(&label_map, &mut label_res_map, &instr_list.len(), &instr);
                if option_instr.0.is_some() {
                    instr_list.push(option_instr.0.expect("Instruction 0 was None!"));
                } else {
                    instr_list.push(option_instr.1.expect("Instruction 1 was None!").clone());
                }
            },
            (None, Some(instr)) => {
                let option_instr = correct_label_instr(&label_map, &mut label_res_map, &instr_list.len(), &instr);
                if option_instr.0.is_some() {
                    instr_list.push(option_instr.0.expect("Instruction 0 was None!"));
                } else {
                    instr_list.push(option_instr.1.expect("Instruction 1 was None!").clone());
                }
            },
            (Some(label), None) => {
                let addr = instr_list.len() as u128 * 4 + 4;
                label_map.insert(label.to_string(), addr);
                match label_res_map.get_mut(label) {
                    Some(val) => {
                        for pos in val.as_slice() {
                            match instr_list[*pos] {
                                Instruction::VJmp(_) => instr_list[*pos] = Instruction::Jmp(addr as i32),
                                Instruction::VBt(_) => instr_list[*pos] = Instruction::Bt(addr as i32),
                                Instruction::VBf(_) => instr_list[*pos] = Instruction::Bf(addr as i32),
                                _ => (),
                            }
                        };
                        val.clear();
                    }
                    None => (),
                }
            },
            (None, None) => (),
        }

        if rest.is_empty() {
            break;
        }
    }

    // TODO: If labels are still in the hashmap, return a custom parser error!

    Ok(("", instr_list))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_label() {
        assert_ne!(parse_label_definition("invalid"), Ok(("", "invalid")));
        assert_ne!(parse_label_definition("invalid0:"), Ok(("", "invalid0")));
        assert_ne!(parse_label_definition("invalid :"), Ok(("", "invalid")));
        assert_ne!(parse_label_definition(" "), Ok(("", "")));
        assert_eq!(parse_label_definition("valid:"), Ok(("", "valid")));
    }

    #[test]
    fn test_parse_imm() {
        assert_ne!(parse_imm("invalid"), Ok(("", 0)));
        assert_ne!(parse_imm(" "), Ok(("", 0)));
        assert_eq!(parse_imm("10"), Ok(("", 10)));
        assert_eq!(parse_imm("0xA"), Ok(("", 10)));
    }

    #[test]
    fn test_parse_reg() {
        assert_ne!(parse_reg("invalid"), Ok(("", Reg::NA)));
        assert_ne!(parse_reg(" "), Ok(("", Reg::NA)));
        assert_ne!(parse_reg("  "), Ok(("", Reg::NA)));
        assert_eq!(parse_reg("$3"), Ok(("", Reg::G3)));
    }

    #[test]
    fn test_parse_seper() {
        assert_ne!(parse_seper("invalid"), Ok(("", "")));
        assert_ne!(parse_seper(" "), Ok(("", "")));
        assert_eq!(parse_seper(", "), Ok(("", ",")));
        assert_eq!(parse_seper(","), Ok(("", ",")));
    }

    #[test]
    fn test_parse_instr1imm() {
        assert_ne!(parse_inst_1imm("invalid"), Ok(("", Instruction::NA)));
        assert_ne!(parse_inst_1imm(" "), Ok(("", Instruction::NA)));
        assert_ne!(parse_inst_1imm(""), Ok(("", Instruction::NA)));
        assert_ne!(parse_inst_1imm("jmp"), Ok(("", Instruction::NA)));
        assert_eq!(parse_inst_1imm("jmp label"), Ok(("", Instruction::VJmp("label".to_string()))));
        assert_eq!(parse_inst_1imm("bf label"), Ok(("", Instruction::VBf("label".to_string()))));
        assert_ne!(parse_inst_1imm("jmp label    "), Ok(("", Instruction::VJmp("label".to_string()))));
    }

    #[test]
    fn test_parse_instr1imm1reg() {
        assert_ne!(parse_inst_1imm1reg("invalid"), Ok(("", Instruction::NA)));
        assert_ne!(parse_inst_1imm1reg(" "), Ok(("", Instruction::NA)));
        assert_ne!(parse_inst_1imm1reg(""), Ok(("", Instruction::NA)));
        assert_ne!(parse_inst_1imm1reg("ld"), Ok(("", Instruction::NA)));
        assert_eq!(parse_inst_1imm1reg("st $1, 0xAA"), Ok(("", Instruction::St(Reg::G1, 170))));
        assert_eq!(parse_inst_1imm1reg("sb $15,6"), Ok(("", Instruction::Sb(Reg::G15, 6))));
        assert_ne!(parse_inst_1imm1reg("sb$15,6"), Ok(("", Instruction::Sb(Reg::G15, 6))));
        assert_ne!(parse_inst_1imm1reg("sb $15,  6"), Ok(("", Instruction::Sb(Reg::G15, 6))));
        assert_ne!(parse_inst_1imm1reg("jmp label    "), Ok(("", Instruction::VJmp("label".to_string()))));
    }

    #[test]
    fn test_parse_inst_2reg() {
        assert_ne!(parse_inst_2reg("invalid"), Ok(("", Instruction::NA)));
        assert_ne!(parse_inst_2reg("   "), Ok(("", Instruction::NA)));
        assert_ne!(parse_inst_2reg("ld $1, 0xAA"), Ok(("", Instruction::NA)));
        assert_ne!(parse_inst_2reg("cmpe $1, 0xAA"), Ok(("", Instruction::NA)));
        assert_eq!(parse_inst_2reg("cmpe $1, $4"), Ok(("", Instruction::Cmpe(Reg::G1, Reg::G4))));
        assert_ne!(parse_inst_2reg("cmpl $1$4"), Ok(("", Instruction::Cmpl(Reg::G1, Reg::G4))));
        assert_eq!(parse_inst_2reg("mov $1,$4"), Ok(("", Instruction::Mov(Reg::G1, Reg::G4))));
        assert_ne!(parse_inst_2reg("cmpg $1,  $4"), Ok(("", Instruction::Cmpg(Reg::G1, Reg::G4))));
    }

    #[test]
    fn test_parse_inst_1imm2reg() {
        assert_ne!(parse_inst_1imm2reg("invalid"), Ok(("", Instruction::NA)));
        assert_ne!(parse_inst_1imm2reg("   "), Ok(("", Instruction::NA)));
        assert_ne!(parse_inst_1imm2reg("cmpe $1, $6"), Ok(("", Instruction::NA)));
        assert_ne!(parse_inst_1imm2reg("addi $1, 0xAA"), Ok(("", Instruction::NA)));
        assert_eq!(parse_inst_1imm2reg("muli $1, $4, 5"), Ok(("", Instruction::Muli(Reg::G1, Reg::G4, 5))));
        assert_ne!(parse_inst_1imm2reg("mulci $1$4,0x6"), Ok(("", Instruction::Mulci(Reg::G1, Reg::G4, 6))));
        assert_eq!(parse_inst_1imm2reg("divi $10,$10, 51"), Ok(("", Instruction::Divi(Reg::G10, Reg::G10, 51))));
        assert_ne!(parse_inst_1imm2reg("mulci $6,  $8,5"), Ok(("", Instruction::Mulci(Reg::G6, Reg::G8, 5))));
    }

    #[test]
    fn test_parse_inst_3reg() {
        assert_ne!(parse_inst_3reg("invalid"), Ok(("", Instruction::NA)));
        assert_ne!(parse_inst_3reg("   "), Ok(("", Instruction::NA)));
        assert_ne!(parse_inst_3reg("cmpe $1, $6"), Ok(("", Instruction::NA)));
        assert_ne!(parse_inst_3reg("addi $1, 0xAA"), Ok(("", Instruction::NA)));
        assert_eq!(parse_inst_3reg("mul $1, $4, $6"), Ok(("", Instruction::Muln(Reg::G1, Reg::G4, Reg::G6))));
        assert_ne!(parse_inst_3reg("xor $10$14,$7"), Ok(("", Instruction::Xor(Reg::G10, Reg::G14, Reg::G7))));
        assert_eq!(parse_inst_3reg("add $10,$11, $10"), Ok(("", Instruction::Addn(Reg::G10, Reg::G11, Reg::G10))));
        assert_ne!(parse_inst_3reg("xnor $6,  $8,$5"), Ok(("", Instruction::Xnor(Reg::G6, Reg::G8, Reg::G5))));
    }

    #[test]
    fn test_parse_instruction() {
        assert_eq!(parse_instruction("cmpe $1, $6"), Ok(("", Instruction::Cmpe(Reg::G1, Reg::G6))));
        assert_ne!(parse_instruction("addi $1, 0xAA"), Ok(("", Instruction::NA)));
        assert_eq!(parse_instruction("mul $1, $4, $6"), Ok(("", Instruction::Muln(Reg::G1, Reg::G4, Reg::G6))));
        assert_ne!(parse_instruction("xor $10$14,$7"), Ok(("", Instruction::Xor(Reg::G10, Reg::G14, Reg::G7))));
        assert_eq!(parse_instruction("add $10,$11, $10"), Ok(("", Instruction::Addn(Reg::G10, Reg::G11, Reg::G10))));
        assert_ne!(parse_instruction("xnor $6,  $8,$5"), Ok(("", Instruction::Xnor(Reg::G6, Reg::G8, Reg::G5))));
        assert_eq!(parse_instruction("srr $5, 7"), Ok(("", Instruction::Srr(Reg::G5, 7))));
    }

    #[test]
    fn test_parse_line() {
        assert_eq!(parse_line("label: add $1, $5, $6"),
                   Ok(("", (Some("label"), Some(Instruction::Addn(Reg::G1, Reg::G5, Reg::G6))))));
        assert_eq!(parse_line("\ntest:\n\nsub $6, $5, $11"),
                   Ok(("", (Some("test"), Some(Instruction::Subn(Reg::G6, Reg::G5, Reg::G11))))));
        assert_eq!(parse_line("\n\n\nreturn:\n"),
                   Ok(("", (Some("return"), None))));
        assert_eq!(parse_line("movu $16, 0x05\nmovl $16, 0x05"),
                   Ok(("\nmovl $14, 0x05", (None, Some(Instruction::Movu(Reg::G14, 5))))));
        assert_eq!(parse_line("label:\ndiv $14, $13, $10"),
                   Ok(("", (Some("label"), Some(Instruction::Divn(Reg::G14, Reg::G13, Reg::G10))))));
    }

    #[test]
    fn test_parse() {
        let source_code = r#"START:
    movu $3, 16
    movl $3, 16
    cmpe $3, $4
    bt END
    movu $4, 16
    movl $4, 16
    jmp START
END:
"#;
        let correct_vec: Vec<Instruction> = vec![
                                                 Instruction::Movu(Reg::G3, 16),
                                                 Instruction::Movl(Reg::G3, 16),
                                                 Instruction::Cmpe(Reg::G3, Reg::G4),
                                                 Instruction::Bt(32),
                                                 Instruction::Movu(Reg::G4, 16),
                                                 Instruction::Movl(Reg::G4, 16),
                                                 Instruction::Jmp(0)
                                                ];

        assert_eq!(parse(source_code),
                   Ok(("", correct_vec)));

        // TODO: Probably more test cases!
    }
}
