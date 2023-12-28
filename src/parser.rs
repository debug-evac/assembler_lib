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
        multispace1, alphanumeric0,
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
use std::borrow::Cow;

pub type Label = str;
pub type LabelStr = String;
pub type Imm = i32; // always less than 32

#[derive(Debug, Clone)]
pub struct LabelInsertError {
    label: LabelStr,
}

impl LabelInsertError {
    pub fn new(label: LabelStr) -> LabelInsertError {
        LabelInsertError { label }
    }
}

impl std::fmt::Display for LabelInsertError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{} already exists!", self.label)
    }
}

#[repr(u8)]
#[derive(Debug, Clone, PartialEq)]
pub enum Reg {
    G0, G1, G2, G3, G4, G5, G6, G7, G8, G9, G10, G11, G12, G13, G14, G15,
    G16, G17, G18, G19, G20, G21, G22, G23, G24, G25, G26, G27, G28, G29,
    G30, G31,
    NA,
}

impl Reg {
    pub fn num_to_enum(reg: &u8) -> Reg {
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
            reg if reg == Reg::G16 as u8 => Reg::G16,
            reg if reg == Reg::G17 as u8 => Reg::G17,
            reg if reg == Reg::G18 as u8 => Reg::G18,
            reg if reg == Reg::G19 as u8 => Reg::G19,
            reg if reg == Reg::G20 as u8 => Reg::G20,
            reg if reg == Reg::G21 as u8 => Reg::G21,
            reg if reg == Reg::G22 as u8 => Reg::G22,
            reg if reg == Reg::G23 as u8 => Reg::G23,
            reg if reg == Reg::G24 as u8 => Reg::G24,
            reg if reg == Reg::G25 as u8 => Reg::G25,
            reg if reg == Reg::G26 as u8 => Reg::G26,
            reg if reg == Reg::G27 as u8 => Reg::G27,
            reg if reg == Reg::G28 as u8 => Reg::G28,
            reg if reg == Reg::G29 as u8 => Reg::G29,
            reg if reg == Reg::G30 as u8 => Reg::G30,
            reg if reg == Reg::G31 as u8 => Reg::G31,
            _ => Reg::NA,
        }
    }

    pub fn str_to_enum(reg: &str) -> Reg {
        todo!("Special register types!");
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum MacroInstr {
    Beq(Reg, Reg, LabelStr),
    Bne(Reg, Reg, LabelStr),
    Blt(Reg, Reg, LabelStr),
    Bltu(Reg, Reg, LabelStr),
    Bge(Reg, Reg, LabelStr),
    Bgeu(Reg, Reg, LabelStr),

    Mov(Reg, Reg),

    // If there is time and someone has nothing to do..
    //Subi(Reg, Reg, Imm),
    //Muli(Reg, Reg, Imm),
    //Divi(Reg, Reg, Imm),

    // Not strictly needed, but nice to have
    Srr(Reg, Reg, Imm),
    Slr(Reg, Reg, Imm),
    
    // Should be done.
    Divn(Reg, Reg, Reg),
    Muln(Reg, Reg, Reg),

    // Not needed. But can be done, for user experience
    //Divcn(Reg, Reg, Reg),
    //Mulcn(Reg, Reg, Reg),
    //Subcn(Reg, Reg, Reg),
    //Addcn(Reg, Reg, Reg),

    Xnor(Reg, Reg, Reg),
    Nor(Reg, Reg, Reg),

    Lb(Reg, Reg, LabelStr), //Load byte
    Lh(Reg, Reg, LabelStr), //Load half
    Lw(Reg, Reg, LabelStr), //Load word

    Lbu(Reg, Reg, LabelStr),
    Lhu(Reg, Reg, LabelStr),

    Sb(Reg, Reg, LabelStr), //Store byte
    Sh(Reg, Reg, LabelStr), //Store half
    Sw(Reg, Reg, LabelStr), //Store word
}

// Possibly split Instruction to instruction enums with 1 imm, 1 reg and 1 imm and so on
// and implement a trait Instruction that must be implemented by all enums
// Then could parse only the instruction and the args separately thus greatly reducing
// code length and multiplication.
#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
    NA,

    Addn(Reg, Reg, Reg),
    Subn(Reg, Reg, Reg),
    
    Xor(Reg, Reg, Reg),
    Or(Reg, Reg, Reg),
    And(Reg, Reg, Reg),

    Sll(Reg, Reg, Reg),
    Srl(Reg, Reg, Reg),
    Sra(Reg, Reg, Reg),

    Slt(Reg, Reg, Reg),
    Sltu(Reg, Reg, Reg),
    // ------------------
    Addi(Reg, Reg, Imm),

    XorI(Reg, Reg, Imm),
    OrI(Reg, Reg, Imm),
    AndI(Reg, Reg, Imm),

    // Shift left|right logical|arithmetic|rotate
    Slli(Reg, Reg, Imm),
    Srli(Reg, Reg, Imm),
    Srai(Reg, Reg, Imm),

    Slti(Reg, Reg, Imm),
    Sltiu(Reg, Reg, Imm),
    // ------------------
    Lb(Reg, Reg, Imm), //Load byte
    Lh(Reg, Reg, Imm), //Load half
    Lw(Reg, Reg, Imm), //Load word

    Lbu(Reg, Reg, Imm),
    Lhu(Reg, Reg, Imm),
    // ------------------
    Sb(Reg, Reg, Imm), //Store byte
    Sh(Reg, Reg, Imm), //Store half
    Sw(Reg, Reg, Imm), //Store word
    // ------------------
    // Imm is the address!
    Beq(Reg, Reg, Imm), // Branch if equal 
    Bne(Reg, Reg, Imm), // Branch if not equal
    Blt(Reg, Reg, Imm), // Branch if less than
    Bltu(Reg, Reg, Imm),
    Bge(Reg, Reg, Imm), // Branch if greater or equal
    Bgeu(Reg, Reg, Imm),
    // ------------------    
    Jal(Reg, Imm),
    Jalr(Reg, Reg, Imm),
    // ------------------
    Lui(Reg, Imm),
    Auipc(Reg, Imm),
    
    // 1 Imm
    //Jmp(Imm),
    //Bt(Imm),
    //Bf(Imm),

    //VJmp(LabelStr),
    //VBt(LabelStr),
    //VBf(LabelStr),

    // 1 Reg & 1 Imm

    //Movu(Reg, Imm),
    //Movl(Reg, Imm),

    // 2 Regs
    //Cmpe(Reg, Reg),

    // 2 Regs & 1 Imm

    // Not needed.
    //Addci(Reg, Reg, Imm),
    //Subci(Reg, Reg, Imm),
    //Mulci(Reg, Reg, Imm),
    //Divci(Reg, Reg, Imm),

    // What is that? Shift left arithmetic immediate? Not needed.
    //Slai(Reg, Reg, Imm),

    // 3 Regs

    // Compare is branching
    //Cmpg(Reg, Reg, Reg),
    //Cmpl(Reg, Reg, Reg),
    // Shift left arithmetic? Not needed. Only difference to logical shift is 
    // that it triggers an arithmetic overflow
    //Sla(Reg, Reg, Reg),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Operation <'a> {
    Namespace(usize),
    Instr(Instruction),
    Macro(MacroInstr),
    LablInstr(Cow<'a, Label>, Instruction),
    Labl(Cow<'a, Label>)
}

impl <'a> From<Instruction> for Operation <'a> {
    fn from(item: Instruction) -> Self {
        Operation::Instr(item)
    }
}

#[derive(Debug, Clone, PartialEq)]
// Scope for locality: true for global; false for local
pub struct LabelElem {
    name: String,
    definition: i128,
    scope: bool,
    references: Box<HashSet<usize>>,
}

impl LabelElem {
    pub fn new() -> LabelElem {
        let name = String::new();
        let definition: i128 = -1;
        let scope = false;
        let references: Box<HashSet<usize>> = Box::from(HashSet::new());

        LabelElem {
            name,
            definition,
            scope, 
            references 
        }
    }

    pub fn new_def(name: String, definition: i128) -> LabelElem {
        let mut elem = LabelElem::new();
        elem.set_name(name);
        elem.set_def(definition);
        elem
    }

    pub fn new_ref(name: String, reference: usize) -> LabelElem {
        let mut elem = LabelElem::new();
        elem.set_name(name);
        elem.add_ref(reference);
        elem
    }

    // TODO: Custom error struct
    pub fn combine(&mut self, other: &LabelElem, offset: usize) -> Result<&str, &str> {
        assert!(self.name.eq(&other.name));
        assert!(self.scope == other.scope);

        if self.definition != -1 && other.definition != -1 {
            return Err("Global label defined in two files!");
        } else if self.definition == -1 && other.definition != -1 {
            self.definition = other.definition;
        }

        let other_refs: HashSet<usize> = other.references.iter()
                .map(|line| line + offset).collect();

        let union = self.references.union(&other_refs).map(|&val| val);
        self.references = Box::from(union.collect::<HashSet<usize>>());
        Ok("Labels combined!")
    }

    pub fn set_name(&mut self, name: String) -> () {
        self.name = name;
    }

    pub fn get_name(&self) -> &String {
        &self.name
    }

    pub fn set_scope(&mut self, scope: bool) -> () {
        self.scope = scope;
    }

    pub fn get_scope(&self) -> bool {
        self.scope
    }

    pub fn set_def(&mut self, definition: i128) -> () {
        self.definition = definition;
    }

    pub fn get_def(&self) -> &i128 {
        &self.definition
    }

    pub fn add_ref(&mut self, reference: usize) -> () {
        self.references.insert(reference);
    }

    pub fn rem_ref(&mut self, reference: usize) -> () {
        self.references.remove(&reference);
    }

    pub fn replace_ref(&mut self, old: &usize, new: usize) -> bool {
        if self.references.remove(old) {
            self.references.insert(new);
            return true;
        }
        return false;
    }
    /*
    pub fn get_refs(&self) -> Vec<usize> {
        self.references.clone().into_iter().collect()
    }*/
}

#[derive(Debug, PartialEq, Clone)]
pub struct LabelRecog {
    offset: usize,
    label_map: Box<HashMap<String, usize>>,
    label_list: Box<Vec<LabelElem>>,
}

impl LabelRecog {
    pub fn new() -> LabelRecog {
        let offset = 0;
        let label_list: Box<Vec<LabelElem>> = Box::from(vec![]);
        let label_map: Box<HashMap<String, usize>> =
        Box::from(HashMap::new());

        LabelRecog {
            offset,
            label_list,
            label_map,
        }
    }

    pub fn insert_label(&mut self, label: LabelElem) -> Result<&str, &str> {
        if self.label_map.contains_key(label.get_name()) {
            Err("Label already exists!")
        } else {
            let elem = self.label_list.len();
            self.label_map.insert(label.get_name().clone(), elem);
            self.label_list.push(label);
            Ok("Added label!")
        }
    }

    pub fn get_label(&mut self, label_str: &String) -> Option<&mut LabelElem> {
        match self.label_map.get(label_str) {
            Some(val) => self.label_list.get_mut(*val),
            None => None,
        }
    }

    pub fn crt_or_def_label(&mut self, label_str: &String, scope: bool, definition: i128) -> () {
        match self.get_label(label_str) {
            Some(label) => {
                label.set_def(definition);
                label.set_scope(scope);
            },
            None => {
                let mut label = LabelElem::new();
                label.set_name(label_str.clone());
                label.set_def(definition);
                label.set_scope(scope);
                let _ = self.insert_label(label);
            },
        }
    }

    // Creates a label, if it does not exist already with the name label_str, scope and the reference.
    // Returns true, if there is already a definition, else false.
    pub fn crt_or_ref_label(&mut self, label_str: &String, scope: bool, reference: usize) -> bool {
        match self.get_label(label_str) {
            Some(label) => {
                label.add_ref(reference);
                if !label.get_scope() {
                    *label.get_def() != -1
                } else {
                    false
                }
            },
            None => {
                let mut label = LabelElem::new();
                label.set_name(label_str.clone());
                label.add_ref(reference);
                label.set_scope(scope);
                let _ = self.insert_label(label);
                false
            },
        }
    }

    pub fn get_local_labels(&self) -> Box<Vec<&LabelElem>> {
        let mut local_labels: Box<Vec<&LabelElem>> = Box::from(vec![]);
        for label in self.label_list.iter() {
            if !label.get_scope() {
                local_labels.push(label);
            }
        }
        local_labels
    }

    pub fn get_global_labels(&self) -> Box<Vec<&LabelElem>> {
        let mut global_labels: Box<Vec<&LabelElem>> = Box::from(vec![]);
        for label in self.label_list.iter() {
            if label.get_scope() {
                global_labels.push(label);
            }
        }
        global_labels
    }

    pub fn set_offset(&mut self, offset: usize) -> () {
        self.offset = offset;
    }

    pub fn get_offset(&self) -> usize {
        self.offset
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

// $0 - $15
fn parse_reg(input: &str) -> IResult<&str, Reg> {
    let (rest, _) = tag("$")(input)?;
    let (rest, reg) = map_res(digit1, str::parse)(rest)?;

    let real_reg = Reg::num_to_enum(&reg);

    if real_reg == Reg::NA {
        println!("WARNING! Reg::NA RECEIVED! Rest = {}", rest);
        todo!("Implement own error!");
    } else {
        Ok((rest, real_reg))
    }
}

// ld $1,0x01 OR ld $1, 0x01
fn parse_seper(input: &str) -> IResult<&str, &str> {
    let (rest, not_needed) = tag(",")(input)?;
    let (rest, _) = opt(tag(" "))(rest)?;

    Ok((rest, not_needed))
}

// jmp label
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

// ld $3, 0x30
fn parse_inst_1imm1reg(input: &str) -> IResult<&str, Instruction> {
    let (rest, instr) = alt((
        value(Instruction::Movu(Reg::NA, 0), tag("movu")),
        value(Instruction::Movl(Reg::NA, 0), tag("movl")),

        value(Instruction::Lui(Reg::NA, 0), tag("lui")),
        value(Instruction::Auipc(Reg::NA, 0), tag("auipc")),
    ))(input)?;
    let (rest, _) = tag(" ")(rest)?;
    let (rest, args) = separated_pair(parse_reg, parse_seper, parse_imm)(rest)?;

    let instr = match instr {
        Instruction::Movu(_, _) => Instruction::Movu(args.0, args.1),
        Instruction::Movl(_, _) => Instruction::Movl(args.0, args.1),

        Instruction::Lui(_, _) => Instruction::Lui(args.0, args.1),
        Instruction::Auipc(_, _) => Instruction::Auipc(args.0, args.1),
        _ => Instruction::NA
    };

    Ok((rest, instr))
}

fn parse_inst_2reg(input: &str) -> IResult<&str, Instruction> {
    let (rest, instr) = alt((
        value(Instruction::Cmpe(Reg::NA, Reg::NA), tag("cmpe")),
        value(Instruction::Mov(Reg::NA, Reg::NA), tag("mov")),
    ))(input)?;
    let (rest, _) = tag(" ")(rest)?;
    let (rest, args) = separated_pair(parse_reg, parse_seper, parse_reg)(rest)?;

    let instr = match instr {
        Instruction::Cmpe(_, _) => Instruction::Cmpe(args.0, args.1),
        Instruction::Mov(_, _) => Instruction::Mov(args.0, args.1),
        _ => Instruction::NA
    };

    Ok((rest, instr))
}

fn parse_inst_1imm2reg_lw(input: &str) -> IResult<&str, Instruction> {
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
        value(Instruction::Slai(Reg::NA, Reg::NA, 0), tag("slai")),

        value(Instruction::Srr(Reg::NA, Reg::NA, 0), tag("srr")),
        value(Instruction::Slr(Reg::NA, Reg::NA, 0), tag("slr")),

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
        Instruction::Slai(_, _, _) => Instruction::Slai(args.0, args.2, args.4),

        Instruction::Srr(_, _, _) => Instruction::Srr(args.0, args.2, args.4,),
        Instruction::Slr(_, _, _) => Instruction::Slr(args.0, args.2, args.4),

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

    Ok((rest, instr))
}

fn parse_inst_1imm2reg_up(input: &str) -> IResult<&str, Instruction> {
    let (rest, instr) = alt((
        value(Instruction::Addi(Reg::NA, Reg::NA, 0), tag("addi")),
        //value(Instruction::Addci(Reg::NA, Reg::NA, 0), tag("addci")),
        value(Instruction::Subi(Reg::NA, Reg::NA, 0), tag("subi")),
        //value(Instruction::Subci(Reg::NA, Reg::NA, 0), tag("subci")),

        value(Instruction::Muli(Reg::NA, Reg::NA, 0), tag("muli")),
        //value(Instruction::Mulci(Reg::NA, Reg::NA, 0), tag("mulci")),
        value(Instruction::Divi(Reg::NA, Reg::NA, 0), tag("divi")),
        //value(Instruction::Divci(Reg::NA, Reg::NA, 0), tag("divci")),

        value(Instruction::Slti(Reg::NA, Reg::NA, 0), tag("slti")),
        value(Instruction::Sltiu(Reg::NA, Reg::NA, 0), tag("sltiu")),
        value(Instruction::XorI(Reg::NA, Reg::NA, 0), tag("xorI")),
        value(Instruction::OrI(Reg::NA, Reg::NA, 0), tag("orI")),
        value(Instruction::AndI(Reg::NA, Reg::NA, 0), tag("andI")),
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

        Instruction::Slti(_, _, _) => Instruction::Slti(args.0, args.2, args.4),
        Instruction::Sltiu(_, _, _) => Instruction::Sltiu(args.0, args.2, args.4),
        Instruction::XorI(_, _, _) => Instruction::XorI(args.0, args.2, args.4),
        Instruction::OrI(_, _, _) => Instruction::OrI(args.0, args.2, args.4),
        Instruction::AndI(_, _, _) => Instruction::AndI(args.0, args.2, args.4),

        _ => Instruction::NA
    };

    Ok((rest, instr))
}

fn parse_inst_1lbl2reg(input: &str)-> IResult<&str, Instruction> {
    let (rest, instr) = alt((
        value(Instruction::VBeq(Reg::NA, Reg::NA, 0.to_string()), tag("beq")),
        value(Instruction::VBne(Reg::NA, Reg::NA, 0.to_string()), tag("bne")),
        value(Instruction::VBlt(Reg::NA, Reg::NA, 0.to_string()), tag("blt")),
        value(Instruction::VBltu(Reg::NA, Reg::NA, 0.to_string()), tag("bltu")),
        value(Instruction::VBge(Reg::NA, Reg::NA, 0.to_string()), tag("bge")),
        value(Instruction::VBgeu(Reg::NA, Reg::NA, 0.to_string()), tag("bgeu")),
    ))(input)?;
    let (rest, _) = tag(" ")(rest)?;
    let (rest, args) = tuple((parse_reg, parse_seper, parse_reg, parse_seper, parse_label_name))(rest)?;

    let instr = match instr {
        Instruction::VBeq(_, _, _) => Instruction::VBeq(args.0, args.2, args.4.to_string()),
        Instruction::VBne(_, _, _) => Instruction::VBne(args.0, args.2, args.4.to_string()),
        Instruction::VBlt(_, _, _) => Instruction::VBlt(args.0, args.2, args.4.to_string()),
        Instruction::VBltu(_, _, _) => Instruction::VBltu(args.0, args.2, args.4.to_string()),
        Instruction::VBge(_, _, _) => Instruction::VBge(args.0, args.2, args.4.to_string()),
        Instruction::VBgeu(_, _, _) => Instruction::VBgeu(args.0, args.2, args.4.to_string()),

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

        value(Instruction::Slt(Reg::NA, Reg::NA, Reg::NA),tag("slt")),
        value(Instruction::Sltu(Reg::NA, Reg::NA, Reg::NA),tag("sltu")),

        value(Instruction::Cmpg(Reg::NA, Reg::NA, Reg::NA), tag("cmpg")),
        value(Instruction::Cmpl(Reg::NA, Reg::NA, Reg::NA), tag("cmpl")),

        value(Instruction::Sll(Reg::NA, Reg::NA, Reg::NA), tag("sll")),
        value(Instruction::Srl(Reg::NA, Reg::NA, Reg::NA), tag("srl")),
        value(Instruction::Sra(Reg::NA, Reg::NA, Reg::NA), tag("sra")),
        value(Instruction::Sla(Reg::NA, Reg::NA, Reg::NA), tag("sla")),
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
        Instruction::Slt(_, _, _) => Instruction::Slt(args.0, args.2, args.4),
        Instruction::Sltu(_, _, _) => Instruction::Sltu(args.0, args.2, args.4),

        Instruction::Cmpg(_, _, _) => Instruction::Cmpg(args.0, args.2, args.4),
        Instruction::Cmpl(_, _, _) => Instruction::Cmpl(args.0, args.2, args.4),

        Instruction::Sll(_, _, _) => Instruction::Sll(args.0, args.2, args.4),
        Instruction::Srl(_, _, _) => Instruction::Srl(args.0, args.2, args.4),
        Instruction::Sra(_, _, _) => Instruction::Sra(args.0, args.2, args.4),
        Instruction::Sla(_, _, _) => Instruction::Sla(args.0, args.2, args.4),
    
        _ => Instruction::NA
    };

    Ok((rest, instr))
}

fn parse_instruction(input: &str) -> IResult<&str, Instruction> {
    alt((
        parse_inst_1imm,
        parse_inst_1imm1reg,
        parse_inst_1imm2reg_up,
        parse_inst_1imm2reg_lw,
        parse_inst_2reg,
        parse_inst_3reg
    ))(input)
}

fn parse_line(input: &str) -> IResult<&str, (Option<Cow<Label>>, Option<Instruction>)> {
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

pub fn parse(input: &str) -> IResult<&str, (LabelRecog, Vec<Operation>)> {
    let mut local_ref_not_def: HashSet<String> = HashSet::new();
    let mut symbol_map = LabelRecog::new();
    let mut instr_list: Vec<Operation> = vec![];
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

        let instr_counter = instr_list.len();

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
                    Instruction::VJmp(labl) => {
                        if !symbol_map.crt_or_ref_label(labl, false, instr_counter) {
                            local_ref_not_def.insert(labl.clone());
                        }
                    },
                    Instruction::VBt(labl) => {
                        if !symbol_map.crt_or_ref_label(labl, false, instr_counter) {
                            local_ref_not_def.insert(labl.clone());
                        }
                    },
                    Instruction::VBf(labl) => {
                        if !symbol_map.crt_or_ref_label(labl, false, instr_counter) {
                            local_ref_not_def.insert(labl.clone());
                        }
                    },
                    _ => (),
                }

                instr_list.push(Operation::LablInstr(label, instr));
            },
            (None, Some(instr)) => {
                match &instr {
                    Instruction::VJmp(labl) => {
                        if !symbol_map.crt_or_ref_label(labl, false, instr_counter) {
                            local_ref_not_def.insert(labl.clone());
                        }
                    },
                    Instruction::VBt(labl) => {
                        if !symbol_map.crt_or_ref_label(labl, false, instr_counter) {
                            local_ref_not_def.insert(labl.clone());
                        }
                    },
                    Instruction::VBf(labl) => {
                        if !symbol_map.crt_or_ref_label(labl, false, instr_counter) {
                            local_ref_not_def.insert(labl.clone());
                        }
                    },
                    _ => (),
                }

                instr_list.push(Operation::Instr(instr));
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
        assert_ne!(parse_inst_1imm1reg("jmp label    "), Ok(("", Instruction::VJmp("label".to_string()))));
        // TODO: More tests
    }

    #[test]
    fn test_parse_inst_2reg() {
        assert_ne!(parse_inst_2reg("invalid"), Ok(("", Instruction::NA)));
        assert_ne!(parse_inst_2reg("   "), Ok(("", Instruction::NA)));
        assert_ne!(parse_inst_2reg("ld $1, 0xAA"), Ok(("", Instruction::NA)));
        assert_ne!(parse_inst_2reg("cmpe $1, 0xAA"), Ok(("", Instruction::NA)));
        assert_eq!(parse_inst_2reg("cmpe $1, $4"), Ok(("", Instruction::Cmpe(Reg::G1, Reg::G4))));
        assert_eq!(parse_inst_2reg("mov $1,$4"), Ok(("", Instruction::Mov(Reg::G1, Reg::G4))));
    }

    #[test]
    fn test_parse_inst_1imm2reg() {
        assert_ne!(parse_inst_1imm2reg_up("invalid"), Ok(("", Instruction::NA)));
        assert_ne!(parse_inst_1imm2reg_up("   "), Ok(("", Instruction::NA)));
        assert_ne!(parse_inst_1imm2reg_up("cmpe $1, $6"), Ok(("", Instruction::NA)));
        assert_ne!(parse_inst_1imm2reg_up("addi $1, 0xAA"), Ok(("", Instruction::NA)));
        assert_eq!(parse_inst_1imm2reg_up("muli $1, $4, 5"), Ok(("", Instruction::Muli(Reg::G1, Reg::G4, 5))));
        assert_ne!(parse_inst_1imm2reg_up("mulci $1$4,0x6"), Ok(("", Instruction::Mulci(Reg::G1, Reg::G4, 6))));
        assert_eq!(parse_inst_1imm2reg_up("divi $10,$10, 51"), Ok(("", Instruction::Divi(Reg::G10, Reg::G10, 51))));
        assert_ne!(parse_inst_1imm2reg_up("mulci $6,  $8,5"), Ok(("", Instruction::Mulci(Reg::G6, Reg::G8, 5))));

        assert_eq!(parse_inst_1imm2reg_lw("sw $1, $2, 0xAA"), Ok(("", Instruction::Sw(Reg::G1, Reg::G2, 170))));
        assert_eq!(parse_inst_1imm2reg_lw("sb $13,$15,6"), Ok(("", Instruction::Sb(Reg::G13, Reg::G15, 6))));
        assert_ne!(parse_inst_1imm2reg_lw("sb$1$15,6"), Ok(("", Instruction::Sb(Reg::G1, Reg::G15, 6))));
        assert_ne!(parse_inst_1imm2reg_lw("sb $12, $15,  6"), Ok(("", Instruction::Sb(Reg::G12, Reg::G15, 6))));
        // TODO: More tests
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
        assert_eq!(parse_inst_3reg("cmpl $6, $8, $14"), Ok(("", Instruction::Cmpl(Reg::G6, Reg::G8, Reg::G14))));
        assert_ne!(parse_inst_3reg("cmpg $6,  $8, $14"), Ok(("", Instruction::Cmpg(Reg::G6, Reg::G8, Reg::G14))));
    }

    #[test]
    fn test_parse_instruction() {
        assert_eq!(parse_instruction("cmpe $1, $6"), Ok(("", Instruction::Cmpe(Reg::G1, Reg::G6))));
        assert_ne!(parse_instruction("addi $1, 0xAA"), Ok(("", Instruction::NA)));
        assert_eq!(parse_instruction("mul $1, $4, $6"), Ok(("", Instruction::Muln(Reg::G1, Reg::G4, Reg::G6))));
        assert_ne!(parse_instruction("xor $10$14,$7"), Ok(("", Instruction::Xor(Reg::G10, Reg::G14, Reg::G7))));
        assert_eq!(parse_instruction("add $10,$11, $10"), Ok(("", Instruction::Addn(Reg::G10, Reg::G11, Reg::G10))));
        assert_ne!(parse_instruction("xnor $6,  $8,$5"), Ok(("", Instruction::Xnor(Reg::G6, Reg::G8, Reg::G5))));
        assert_eq!(parse_instruction("srr $5, $8, 7"), Ok(("", Instruction::Srr(Reg::G5, Reg::G8, 7))));
    }

    #[test]
    fn test_parse_line() {
        assert_eq!(parse_line("label: add $1, $5, $6"),
                   Ok(("", (Some(Cow::from("label")), Some(Instruction::Addn(Reg::G1, Reg::G5, Reg::G6))))));
        assert_eq!(parse_line("\ntest:\n\nsub $6, $5, $11"),
                   Ok(("", (Some(Cow::from("test")), Some(Instruction::Subn(Reg::G6, Reg::G5, Reg::G11))))));
        assert_eq!(parse_line("\n\n\nreturn:\n"),
                   Ok(("", (Some(Cow::from("return")), None))));
        assert_eq!(parse_line("movu $15, 0x05\nmovl $15, 0x05"),
                   Ok(("\nmovl $15, 0x05", (None, Some(Instruction::Movu(Reg::G15, 5))))));
        assert_eq!(parse_line("label:\ndiv $14, $13, $10"),
                   Ok(("", (Some(Cow::from("label")), Some(Instruction::Divn(Reg::G14, Reg::G13, Reg::G10))))));
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

        let mut symbols = LabelRecog::new();
        let mut label = LabelElem::new();
        label.set_name("START".to_string());
        label.set_scope(true);
        label.set_def(0);
        label.add_ref(6);
        let _ = symbols.insert_label(label);

        label = LabelElem::new();
        label.set_name("END".to_string());
        label.set_scope(true);
        label.set_def(7);
        label.add_ref(3);
        let _ = symbols.insert_label(label);

        let correct_vec: Vec<Operation> = vec![
                                                 Operation::LablInstr(Cow::from("START"), Instruction::Movu(Reg::G3, 16)),
                                                 Operation::from(Instruction::Movl(Reg::G3, 16)),
                                                 Operation::from(Instruction::Cmpe(Reg::G3, Reg::G4)),
                                                 Operation::from(Instruction::VBt("END".to_string())),
                                                 Operation::from(Instruction::Movu(Reg::G4, 16)),
                                                 Operation::from(Instruction::Movl(Reg::G4, 16)),
                                                 Operation::from(Instruction::VJmp("START".to_string())),
                                                 Operation::Labl(Cow::from("END"))
                                                ];

        assert_eq!(parse(source_code),
                   Ok(("", (symbols, correct_vec))));

        // TODO: Probably more test cases!
    }

    /*
    #[test]
    fn test_labelrecog() {
        let mut symbols = LabelRecog::new();
        let mut label = LabelElem::new();

        let label_name = "HELLO".to_string();
        label.set_name(label_name.clone());

        let _ = symbols.insert_label(label.clone());

        assert_eq!(symbols.label_map.get(&label_name), Some(&0));
    }*/
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
