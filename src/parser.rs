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
    NA,

    NOP,

    Beq(Reg, Reg, LabelStr),
    Bne(Reg, Reg, LabelStr),
    Blt(Reg, Reg, LabelStr),
    Bltu(Reg, Reg, LabelStr),
    Bge(Reg, Reg, LabelStr),
    Bgeu(Reg, Reg, LabelStr),

    Mv(Reg, Reg),
    Li(Reg, Imm),

    Jal(Reg, LabelStr),
    Jalr(Reg, Reg, LabelStr),

    J(Imm),
    Jl(LabelStr),
    Jali(Imm),
    Jall(LabelStr),
    Jr(Reg),
    Jalrs(Reg), 
    Ret,
    Calli(Imm),
    Calll(LabelStr),
    Taili(Imm),
    Taill(LabelStr),

    Lui(Reg, LabelStr),
    Auipc(Reg, LabelStr),

    Slli(Reg, Reg, LabelStr),
    Srli(Reg, Reg, LabelStr),
    Srai(Reg, Reg, LabelStr),

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

    Xori(Reg, Reg, Imm),
    Ori(Reg, Reg, Imm),
    Andi(Reg, Reg, Imm),

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
    LablMacro(Cow<'a, Label>, MacroInstr),
    LablInstr(Cow<'a, Label>, Instruction),
    Labl(Cow<'a, Label>)
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

        value(MacroInstr::Jl("".to_string()), tag("j")),
        value(MacroInstr::Jall("".to_string()), tag("jal")),
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
        value(MacroInstr::J(0), tag("j")),
        value(MacroInstr::Jali(0), tag("jal")),
        value(MacroInstr::Calli(0), tag("call")),
        value(MacroInstr::Taili(0), tag("tail")),
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

// ld $3, 0x30
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
        value(MacroInstr::Blt(Reg::NA, Reg::NA, String::new()), tag("blt")),
        value(MacroInstr::Bltu(Reg::NA, Reg::NA, String::new()), tag("bltu")),
        value(MacroInstr::Bge(Reg::NA, Reg::NA, String::new()), tag("bge")),
        value(MacroInstr::Bgeu(Reg::NA, Reg::NA, String::new()), tag("bgeu")),

        value(MacroInstr::Jalr(Reg::NA, Reg::NA, String::new()), tag("jalr")),

        value(MacroInstr::Slli(Reg::NA, Reg::NA, String::new()), tag("slli")),
        value(MacroInstr::Srli(Reg::NA, Reg::NA, String::new()), tag("srli")),
        value(MacroInstr::Srai(Reg::NA, Reg::NA, String::new()), tag("srai")),

        value(MacroInstr::Sb(Reg::NA, Reg::NA, String::new()), tag("sb")),
        value(MacroInstr::Sh(Reg::NA, Reg::NA, String::new()), tag("sh")),
        value(MacroInstr::Sw(Reg::NA, Reg::NA, String::new()), tag("sw")),

        value(MacroInstr::Lb(Reg::NA, Reg::NA, String::new()), tag("lb")),
        value(MacroInstr::Lbu(Reg::NA, Reg::NA, String::new()), tag("lbu")),
        value(MacroInstr::Lh(Reg::NA, Reg::NA, String::new()), tag("lh")),
        value(MacroInstr::Lhu(Reg::NA, Reg::NA, String::new()), tag("lhu")),
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

fn parse_line(input: &str) -> IResult<&str, (Option<Cow<Label>>, Option<Operation>)> {
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
                    Operation::Macro(macro_in) => {
                        match macro_in {
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
                                if !symbol_map.crt_or_ref_label(labl, false, instr_counter) {
                                    local_ref_not_def.insert(labl.clone());
                                }
                            },
                            _ => (),
                        }

                        instr_list.push(Operation::LablMacro(label, macro_in.to_owned()));
                    },
                    Operation::Instr(instr_in) => {
                        instr_list.push(Operation::LablInstr(label, instr_in.to_owned()));
                    }
                    _ => (),
                }
            },
            (None, Some(instr)) => {
                match &instr {
                    Operation::Macro(macro_in) => {
                        match macro_in {
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
                                if !symbol_map.crt_or_ref_label(labl, false, instr_counter) {
                                    local_ref_not_def.insert(labl.clone());
                                }
                            },
                            _ => (),
                        }
                    },
                    _ => (),
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
    fn test_parse_instrnoparam() {
        assert_ne!(parse_inst_noparm("invalid"), Ok(("", Operation::Macro(MacroInstr::NOP))));
        assert_ne!(parse_inst_noparm("noop"), Ok(("", Operation::Macro(MacroInstr::NOP))));
        assert_eq!(parse_inst_noparm("nop"), Ok(("", Operation::Macro(MacroInstr::NOP))));
        assert_ne!(parse_inst_noparm("nop $1"), Ok(("", Operation::Macro(MacroInstr::NOP))));
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
        assert_eq!(parse_inst_1reg("jalr $12"), Ok(("", MacroInstr::Jalrs(Reg::G12).into())));
        assert_eq!(parse_inst_1reg("jr $18"), Ok(("", MacroInstr::Jr(Reg::G18).into())));
        assert_ne!(parse_inst_1reg("jalr $19  "), Ok(("", MacroInstr::Jalrs(Reg::G19).into())));
    }

    #[test]
    fn test_parse_instr1labl1reg() {
        assert_ne!(parse_inst_1labl1reg(""), Ok(("", MacroInstr::NA.into())));
        assert_ne!(parse_inst_1labl1reg("lui"), Ok(("", MacroInstr::Lui(Reg::NA, "".to_string()).into())));
        assert_eq!(parse_inst_1labl1reg("lui $12, stop"), Ok(("", MacroInstr::Lui(Reg::G12, "stop".to_string()).into())));
        assert_eq!(parse_inst_1labl1reg("auipc $18, helloWorld"), Ok(("", MacroInstr::Auipc(Reg::G18, "helloWorld".to_string()).into())));
        assert_eq!(parse_inst_1labl1reg("jal $20, test"), Ok(("", MacroInstr::Jal(Reg::G20, "test".to_string()).into())));
        assert_ne!(parse_inst_1labl1reg("jal $19, train "), Ok(("", MacroInstr::Jal(Reg::G19, "train".to_string()).into())));
    }

    #[test]
    fn test_parse_instr1imm1reg() {
        assert_ne!(parse_inst_1imm1reg("invalid"), Ok(("", Instruction::NA.into())));
        assert_ne!(parse_inst_1imm1reg(" "), Ok(("", Instruction::NA.into())));
        assert_ne!(parse_inst_1imm1reg(""), Ok(("", Instruction::NA.into())));
        assert_ne!(parse_inst_1imm1reg("ld"), Ok(("", Instruction::NA.into())));
        assert_ne!(parse_inst_1imm1reg(""), Ok(("", Instruction::NA.into())));
        assert_ne!(parse_inst_1imm1reg("lui"), Ok(("", Instruction::Lui(Reg::NA, 0).into())));
        assert_eq!(parse_inst_1imm1reg("lui $12, 12"), Ok(("", Instruction::Lui(Reg::G12, 12).into())));
        assert_eq!(parse_inst_1imm1reg("auipc $18, 0x20"), Ok(("", Instruction::Auipc(Reg::G18, 32).into())));
        assert_eq!(parse_inst_1imm1reg("jal $20, 5"), Ok(("", Instruction::Jal(Reg::G20, 5).into())));
        assert_ne!(parse_inst_1imm1reg("jal $19, 125 "), Ok(("", Instruction::Jal(Reg::G19, 125).into())));
    }

    #[test]
    fn test_parse_inst_2reg() {
        assert_ne!(parse_inst_2reg("invalid"), Ok(("", MacroInstr::NA.into())));
        assert_ne!(parse_inst_2reg("   "), Ok(("", MacroInstr::NA.into())));
        assert_ne!(parse_inst_2reg("ld $1, 0xAA"), Ok(("", MacroInstr::NA.into())));
        assert_ne!(parse_inst_2reg("mv $1, 0xAA"), Ok(("", MacroInstr::NA.into())));
        assert_eq!(parse_inst_2reg("mv $1, $4"), Ok(("", MacroInstr::Mv(Reg::G1, Reg::G4).into())));
        assert_eq!(parse_inst_2reg("mv $12,$4"), Ok(("", MacroInstr::Mv(Reg::G12, Reg::G4).into())));
    }

    fn test_parse_instr1labl2reg() {
        assert_ne!(parse_inst_1labl2reg("invalid"), Ok(("", MacroInstr::NA.into())));
        assert_ne!(parse_inst_1labl2reg("   "), Ok(("", MacroInstr::NA.into())));
        assert_ne!(parse_inst_1labl2reg("sb $1, $6"), Ok(("", MacroInstr::Sb(Reg::G1, Reg::G6, "".to_string()).into())));
        assert_ne!(parse_inst_1labl2reg("lb $1, total"), Ok(("", MacroInstr::Lb(Reg::G1, Reg::NA, "total".to_string()).into())));
        assert_eq!(parse_inst_1labl2reg("bgeu $1, $4, sTaRt"), Ok(("", MacroInstr::Bgeu(Reg::G1, Reg::G4, "sTaRt".to_string()).into())));
        assert_ne!(parse_inst_1labl2reg("slli $1$4,eNND"), Ok(("", MacroInstr::Slli(Reg::G1, Reg::G4, "eNND".to_string()).into())));
        assert_eq!(parse_inst_1labl2reg("Blt $10,$10, last"), Ok(("", MacroInstr::Blt(Reg::G10, Reg::G10, "last".to_string()).into())));
        assert_ne!(parse_inst_1labl2reg("jalr $6,  $8,test"), Ok(("", MacroInstr::Jalr(Reg::G6, Reg::G8, "test".to_string()).into())));
        assert_eq!(parse_inst_1labl2reg("lhu $1, $2, hans"), Ok(("", MacroInstr::Lhu(Reg::G1, Reg::G2, "hans".to_string()).into())));
        assert_eq!(parse_inst_1labl2reg("sb $13,$15,loading"), Ok(("", MacroInstr::Sb(Reg::G13, Reg::G15, "loading".to_string()).into())));
        assert_ne!(parse_inst_1labl2reg("beq$1$15,start"), Ok(("", MacroInstr::Beq(Reg::G1, Reg::G15, "start".to_string()).into())));
        assert_ne!(parse_inst_1labl2reg("lbu $12, $15,  dasletzte"), Ok(("", MacroInstr::Lbu(Reg::G12, Reg::G15, "dasletzte".to_string()).into())));
    }

    #[test]
    fn test_parse_inst_1imm2reg() {
        assert_ne!(parse_inst_1imm2reg_lw("invalid"), Ok(("", Instruction::NA.into())));
        assert_ne!(parse_inst_1imm2reg_lw("   "), Ok(("", Instruction::NA.into())));
        assert_ne!(parse_inst_1imm2reg_lw("addi $1, $6"), Ok(("", Instruction::NA.into())));
        assert_ne!(parse_inst_1imm2reg_lw("lbu $1, 0xAA"), Ok(("", Instruction::Lbu(Reg::G1, Reg::NA, 0xAA).into())));
        assert_eq!(parse_inst_1imm2reg_lw("blt $1, $4, 5"), Ok(("", Instruction::Blt(Reg::G1, Reg::G4, 5).into())));
        assert_ne!(parse_inst_1imm2reg_lw("lb $1$4,0x6"), Ok(("", Instruction::Lb(Reg::G1, Reg::G4, 6).into())));
        assert_eq!(parse_inst_1imm2reg_lw("sb $10,$10, 51"), Ok(("", Instruction::Sb(Reg::G10, Reg::G10, 51).into())));
        assert_ne!(parse_inst_1imm2reg_lw("bge $6,  $8,5"), Ok(("", Instruction::Bge(Reg::G6, Reg::G8, 5).into())));

        assert_eq!(parse_inst_1imm2reg_up("addi $1, $2, 0xAA"), Ok(("", Instruction::Addi(Reg::G1, Reg::G2, 0xAA).into())));
        assert_eq!(parse_inst_1imm2reg_up("srr $13,$15,6"), Ok(("", MacroInstr::Srr(Reg::G13, Reg::G15, 6).into())));
        assert_ne!(parse_inst_1imm2reg_up("slti$1$15,6"), Ok(("", Instruction::Slti(Reg::G1, Reg::G15, 6).into())));
        assert_ne!(parse_inst_1imm2reg_up("slli $12, $15,  6"), Ok(("", Instruction::Slli(Reg::G12, Reg::G15, 6).into())));
        // TODO: More tests
    }

    #[test]
    fn test_parse_inst_3reg() {
        assert_ne!(parse_inst_3reg("invalid"), Ok(("", Instruction::NA.into())));
        assert_ne!(parse_inst_3reg("   "), Ok(("", Instruction::NA.into())));
        assert_ne!(parse_inst_3reg("addi $1, $6, 0xAA"), Ok(("", Instruction::Addi(Reg::G1, Reg::G6, 0xAA).into())));
        assert_ne!(parse_inst_3reg("add $1, $2"), Ok(("", Instruction::Addn(Reg::G1, Reg::G2, Reg::NA).into())));
        assert_eq!(parse_inst_3reg("mul $1, $4, $6"), Ok(("", MacroInstr::Muln(Reg::G1, Reg::G4, Reg::G6).into())));
        assert_ne!(parse_inst_3reg("div $10$14,$7"), Ok(("", MacroInstr::Divn(Reg::G10, Reg::G14, Reg::G7).into())));
        assert_eq!(parse_inst_3reg("xor $10,$11, $10"), Ok(("", Instruction::Xor(Reg::G10, Reg::G11, Reg::G10).into())));
        assert_ne!(parse_inst_3reg("xnor $6,  $8,$5"), Ok(("", MacroInstr::Xnor(Reg::G6, Reg::G8, Reg::G5).into())));
        assert_eq!(parse_inst_3reg("and $6, $8, $14"), Ok(("", Instruction::And(Reg::G6, Reg::G8, Reg::G14).into())));
        assert_ne!(parse_inst_3reg("sll $6,  $8, $14"), Ok(("", Instruction::Sll(Reg::G6, Reg::G8, Reg::G14).into())));
    }

    #[test]
    fn test_parse_instruction() {
        assert_eq!(parse_instruction("mv $1, $6"), Ok(("", MacroInstr::Mv(Reg::G1, Reg::G6).into())));
        assert_ne!(parse_instruction("addi $1, 0xAA"), Ok(("", Instruction::Addi(Reg::G1, Reg::NA, 0xAA).into())));
        assert_eq!(parse_instruction("mul $1, $4, $6"), Ok(("", MacroInstr::Muln(Reg::G1, Reg::G4, Reg::G6).into())));
        assert_ne!(parse_instruction("xor $10$14,$7"), Ok(("", Instruction::Xor(Reg::G10, Reg::G14, Reg::G7).into())));
        assert_eq!(parse_instruction("add $10,$11, $10"), Ok(("", Instruction::Addn(Reg::G10, Reg::G11, Reg::G10).into())));
        assert_ne!(parse_instruction("xnor $6,  $8,$5"), Ok(("", MacroInstr::Xnor(Reg::G6, Reg::G8, Reg::G5).into())));
        assert_eq!(parse_instruction("srr $5, $8, 7"), Ok(("", MacroInstr::Srr(Reg::G5, Reg::G8, 7).into())));
        // More tests?
    }

    #[test]
    fn test_parse_line() {
        assert_eq!(parse_line("label: add $1, $5, $6"),
                   Ok(("", (Some(Cow::from("label")), Some(Instruction::Addn(Reg::G1, Reg::G5, Reg::G6).into())))));
        assert_eq!(parse_line("\ntest:\n\nsub $6, $5, $11"),
                   Ok(("", (Some(Cow::from("test")), Some(Instruction::Subn(Reg::G6, Reg::G5, Reg::G11).into())))));
        assert_eq!(parse_line("\n\n\nreturn:\n"),
                   Ok(("", (Some(Cow::from("return")), None))));
        assert_eq!(parse_line("mv $15, $12\naddi $12, $10, 0x05"),
                   Ok(("\naddi $12, $10, 0x05", (None, Some(MacroInstr::Mv(Reg::G15, Reg::G12).into())))));
        assert_eq!(parse_line("label:\ndiv $14, $13, $10"),
                   Ok(("", (Some(Cow::from("label")), Some(MacroInstr::Divn(Reg::G14, Reg::G13, Reg::G10).into())))));
    }

    #[test]
    fn test_parse() {
        let source_code = r#"START:
    li $4, 16
    mv $3, $4
MUL: beq $3, $4, END
    mul $6, $4, $3
    lui $4, 0x16
    j MUL
END:
"#;

        let mut symbols = LabelRecog::new();
        let mut label = LabelElem::new();
        label.set_name("START".to_string());
        label.set_scope(true);
        label.set_def(0);
        let _ = symbols.insert_label(label);

        label = LabelElem::new();
        label.set_name("MUL".to_string());
        label.set_scope(true);
        label.set_def(2);
        label.add_ref(5);
        let _ = symbols.insert_label(label);

        label = LabelElem::new();
        label.set_name("END".to_string());
        label.set_scope(true);
        label.set_def(6);
        label.add_ref(2);
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
