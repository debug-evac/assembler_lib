/*
 * Copyright (c) 2023 Steven Becker
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

//
// This module defines types, functions, ... that are commonly used in
// other modules. This module was created since the parser module is 
// quite big.

pub mod errors;

use std::collections::HashMap;
use std::fmt::Display;

use crate::linker::Namespaces;
use crate::parser::Subroutines;

use self::errors::CommonError;

pub type Imm = i32; // always less than 32

#[repr(u8)]
#[derive(Debug, Clone, PartialEq)]
pub enum Reg {
    G0, G1, G2, G3, G4, G5, G6, G7, G8, G9, G10, G11, G12, G13, G14, G15,
    G16, G17, G18, G19, G20, G21, G22, G23, G24, G25, G26, G27, G28, G29,
    G30, G31
}

impl Reg {
    pub fn num_to_enum(reg: u8) -> Result<Reg, &'static str> {
        match reg {
            reg if reg == Reg::G0 as u8 => Ok(Reg::G0),
            reg if reg == Reg::G1 as u8 => Ok(Reg::G1),
            reg if reg == Reg::G2 as u8 => Ok(Reg::G2),
            reg if reg == Reg::G3 as u8 => Ok(Reg::G3),
            reg if reg == Reg::G4 as u8 => Ok(Reg::G4),
            reg if reg == Reg::G5 as u8 => Ok(Reg::G5),
            reg if reg == Reg::G6 as u8 => Ok(Reg::G6),
            reg if reg == Reg::G7 as u8 => Ok(Reg::G7),
            reg if reg == Reg::G8 as u8 => Ok(Reg::G8),
            reg if reg == Reg::G9 as u8 => Ok(Reg::G9),
            reg if reg == Reg::G10 as u8 => Ok(Reg::G10),
            reg if reg == Reg::G11 as u8 => Ok(Reg::G11),
            reg if reg == Reg::G12 as u8 => Ok(Reg::G12),
            reg if reg == Reg::G13 as u8 => Ok(Reg::G13),
            reg if reg == Reg::G14 as u8 => Ok(Reg::G14),
            reg if reg == Reg::G15 as u8 => Ok(Reg::G15),
            reg if reg == Reg::G16 as u8 => Ok(Reg::G16),
            reg if reg == Reg::G17 as u8 => Ok(Reg::G17),
            reg if reg == Reg::G18 as u8 => Ok(Reg::G18),
            reg if reg == Reg::G19 as u8 => Ok(Reg::G19),
            reg if reg == Reg::G20 as u8 => Ok(Reg::G20),
            reg if reg == Reg::G21 as u8 => Ok(Reg::G21),
            reg if reg == Reg::G22 as u8 => Ok(Reg::G22),
            reg if reg == Reg::G23 as u8 => Ok(Reg::G23),
            reg if reg == Reg::G24 as u8 => Ok(Reg::G24),
            reg if reg == Reg::G25 as u8 => Ok(Reg::G25),
            reg if reg == Reg::G26 as u8 => Ok(Reg::G26),
            reg if reg == Reg::G27 as u8 => Ok(Reg::G27),
            reg if reg == Reg::G28 as u8 => Ok(Reg::G28),
            reg if reg == Reg::G29 as u8 => Ok(Reg::G29),
            reg if reg == Reg::G30 as u8 => Ok(Reg::G30),
            reg if reg == Reg::G31 as u8 => Ok(Reg::G31),
            _ => Err("No Register with that name found!"),
        }
    }

    pub fn str_to_enum(reg: &str) -> Result<Reg, &str> {
        match reg {
            "zero" => Ok(Reg::G0),
            "ra" => Ok(Reg::G1),
            "sp" => Ok(Reg::G2),
            "gp" => Ok(Reg::G3),
            "tp" => Ok(Reg::G4),
            "t0" => Ok(Reg::G5),
            "t1" => Ok(Reg::G6),
            "t2" => Ok(Reg::G7),
            "s0" | "fp" => Ok(Reg::G8),
            "s1" => Ok(Reg::G9),
            "a0" => Ok(Reg::G10),
            "a1" => Ok(Reg::G11),
            "a2" => Ok(Reg::G12),
            "a3" => Ok(Reg::G13),
            "a4" => Ok(Reg::G14),
            "a5" => Ok(Reg::G15),
            "a6" => Ok(Reg::G16),
            "a7" => Ok(Reg::G17),
            "s2" => Ok(Reg::G18),
            "s3" => Ok(Reg::G19),
            "s4" => Ok(Reg::G20),
            "s5" => Ok(Reg::G21),
            "s6" => Ok(Reg::G22),
            "s7" => Ok(Reg::G23),
            "s8" => Ok(Reg::G24),
            "s9" => Ok(Reg::G25),
            "s10" => Ok(Reg::G26),
            "s11" => Ok(Reg::G27),
            "t3" => Ok(Reg::G28),
            "t4" => Ok(Reg::G29),
            "t5" => Ok(Reg::G30),
            "t6" => Ok(Reg::G31),

            _ => Err("No Register with that name found!"),
        }
    }
}

impl Display for Reg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            Reg::G0 => "zero",
            Reg::G1 => "ra",
            Reg::G2 => "sp",
            Reg::G3 => "gp",
            Reg::G4 => "tp",
            Reg::G5 => "t0",
            Reg::G6 => "t1",
            Reg::G7 => "t2",
            Reg::G8 => "s0",
            Reg::G9 => "s1",
            Reg::G10 => "a0",
            Reg::G11 => "a1",
            Reg::G12 => "a2",
            Reg::G13 => "a3",
            Reg::G14 => "a4",
            Reg::G15 => "a5",
            Reg::G16 => "a6",
            Reg::G17 => "a7",
            Reg::G18 => "s2",
            Reg::G19 => "s3",
            Reg::G20 => "s4",
            Reg::G21 => "s5",
            Reg::G22 => "s6",
            Reg::G23 => "s7",
            Reg::G24 => "s8",
            Reg::G25 => "s9",
            Reg::G26 => "s10",
            Reg::G27 => "s11",
            Reg::G28 => "t3",
            Reg::G29 => "t4",
            Reg::G30 => "t5",
            Reg::G31 => "t6",
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Part {
    Upper,
    Lower,
    None
}

#[derive(Debug, Clone, PartialEq)]
pub enum MacroInstr {
    Beq(Reg, Reg, smartstring::alias::String),
    Bne(Reg, Reg, smartstring::alias::String),
    Blt(Reg, Reg, smartstring::alias::String),
    Bltu(Reg, Reg, smartstring::alias::String),
    Bge(Reg, Reg, smartstring::alias::String),
    Bgeu(Reg, Reg, smartstring::alias::String),

    Jal(Reg, smartstring::alias::String),
    Jalr(Reg, Reg, smartstring::alias::String, Part),

    Lui(Reg, smartstring::alias::String, Part),
    Auipc(Reg, smartstring::alias::String, Part),

    Slli(Reg, Reg, smartstring::alias::String),
    Srli(Reg, Reg, smartstring::alias::String),
    Srai(Reg, Reg, smartstring::alias::String),
    
    Lb(Reg, Reg, smartstring::alias::String, Part), //Load byte
    Lh(Reg, Reg, smartstring::alias::String, Part), //Load half
    Lw(Reg, Reg, smartstring::alias::String, Part), //Load word
    
    Lbu(Reg, Reg, smartstring::alias::String),
    Lhu(Reg, Reg, smartstring::alias::String),
    
    Sb(Reg, Reg, smartstring::alias::String, Part), //Store byte
    Sh(Reg, Reg, smartstring::alias::String, Part), //Store half
    Sw(Reg, Reg, smartstring::alias::String, Part), //Store word

    Addi(Reg, Reg, smartstring::alias::String, Part),

    Srr(Reg, Reg, Imm),
    Slr(Reg, Reg, Imm),

    LiImm(Reg, Imm),
    LiLabl(Reg, smartstring::alias::String),
    LaImm(Reg, Imm),
    LaLabl(Reg, smartstring::alias::String),

    CallImm(Imm),
    TailImm(Imm),

    CallLabl(smartstring::alias::String),
    TailLabl(smartstring::alias::String),

    Push(Vec<Reg>),
    Pop(Vec<Reg>),

    RepMacro(u32, Box<MacroInstr>),
    RepInstr(u32, Instruction)

    // If there is time and someone has nothing to do..
    //Subi(Reg, Reg, Imm),
    //Muli(Reg, Reg, Imm),
    //Divi(Reg, Reg, Imm),
}

impl Display for MacroInstr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MacroInstr::Beq(reg1, reg2, label) => write!(f, "beq {reg1}, {reg2}, {label}"),
            MacroInstr::Bne(reg1, reg2, label) => write!(f, "bne {reg1}, {reg2}, {label}"),
            MacroInstr::Blt(reg1, reg2, label) => write!(f, "blt {reg1}, {reg2}, {label}"),
            MacroInstr::Bltu(reg1, reg2, label) => write!(f, "bltu {reg1}, {reg2}, {label}"),
            MacroInstr::Bge(reg1, reg2, label) => write!(f, "bge {reg1}, {reg2}, {label}"),
            MacroInstr::Bgeu(reg1, reg2, label) => write!(f, "bgeu {reg1}, {reg2}, {label}"),

            MacroInstr::Jal(reg, label) => write!(f, "jal {reg}, {label}"),
            MacroInstr::Jalr(reg1, reg2, label, _) => write!(f, "jalr {reg1}, {reg2}, {label}"),

            MacroInstr::Lui(reg, label, _) => write!(f, "lui {reg}, {label}"),
            MacroInstr::Auipc(reg, label, _) => write!(f, "auipc {reg}, {label}"),

            MacroInstr::Slli(reg1, reg2, label) => write!(f, "slli {reg1}, {reg2}, {label}"),
            MacroInstr::Srli(reg1, reg2, label) => write!(f, "srli {reg1}, {reg2}, {label}"),
            MacroInstr::Srai(reg1, reg2, label) => write!(f, "srai {reg1}, {reg2}, {label}"),
            
            MacroInstr::Lb(reg1, reg2, label, _) => write!(f, "lb {reg1}, {reg2}, {label}"),
            MacroInstr::Lh(reg1, reg2, label, _) => write!(f, "lh {reg1}, {reg2}, {label}"),
            MacroInstr::Lw(reg1, reg2, label, _) => write!(f, "lw {reg1}, {reg2}, {label}"),
            
            MacroInstr::Lbu(reg1, reg2, label) => write!(f, "lbu {reg1}, {reg2}, {label}"),
            MacroInstr::Lhu(reg1, reg2, label) => write!(f, "lhu {reg1}, {reg2}, {label}"),
            
            MacroInstr::Sb(reg1, reg2, label, _) => write!(f, "sb {reg1}, {reg2}, {label}"),
            MacroInstr::Sh(reg1, reg2, label, _) => write!(f, "sh {reg1}, {reg2}, {label}"),
            MacroInstr::Sw(reg1, reg2, label, _) => write!(f, "sw {reg1}, {reg2}, {label}"),

            MacroInstr::Addi(reg1, reg2, label, _) => write!(f, "addi {reg1}, {reg2}, {label}"),

            MacroInstr::Srr(reg1, reg2, imm) => write!(f, "srr {reg1}, {reg2}, {imm}"),
            MacroInstr::Slr(reg1, reg2, imm) => write!(f, "slr {reg1}, {reg2}, {imm}"),

            MacroInstr::LiImm(reg, imm) => write!(f, "li {reg}, {imm}"),
            MacroInstr::LiLabl(reg, label) => write!(f, "li {reg}, {label}"),
            MacroInstr::LaImm(reg, imm) => write!(f, "la {reg}, {imm}"),
            MacroInstr::LaLabl(reg, label) => write!(f, "la {reg}, {label}"),

            MacroInstr::CallImm(imm) => write!(f, "call {imm}"),
            MacroInstr::TailImm(imm) => write!(f, "tail {imm}"),

            MacroInstr::CallLabl(label) => write!(f, "call {label}"),
            MacroInstr::TailLabl(label) => write!(f, "tail {label}"),

            MacroInstr::Push(vec_regs) => {
                write!(f, "push {}", vec_regs[0])?;
                for reg in &vec_regs[1..] {
                    write!(f, ", {}", reg)?;
                }
                Ok(())
            },
            MacroInstr::Pop(vec_regs) => {
                write!(f, "pop {}", vec_regs[0])?;
                for reg in &vec_regs[1..] {
                    write!(f, ", {}", reg)?;
                }
                Ok(())
            },

            MacroInstr::RepMacro(imm, macro_in) => write!(f, "rep {imm}, {macro_in}"),
            MacroInstr::RepInstr(imm, instr) => write!(f, "rep {imm}, {:?}", instr)
        }
    }
}

// Possibly split Instruction to instruction enums with 1 imm, 1 reg and 1 imm and so on
// and implement a trait Instruction that must be implemented by all enums
// Then could parse only the instruction and the args separately thus greatly reducing
// code length and multiplication.
#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
    Add(Reg, Reg, Reg),
    Sub(Reg, Reg, Reg),
    
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
    
    // ------------------
    // Custom commands implemented by the Hardware people
    Xnor(Reg, Reg, Reg),
    Equal(Reg, Reg, Reg),

    // ------------------
    // To cover all commands of RV32I
    Ecall,
    Ebreak,

    // ------------------
    // All commands from RV32M
    Mul(Reg, Reg, Reg),
    Mulh(Reg, Reg, Reg),
    Mulhu(Reg, Reg, Reg),
    Mulhsu(Reg, Reg, Reg),

    Div(Reg, Reg, Reg),
    Divu(Reg, Reg, Reg),
    Rem(Reg, Reg, Reg),
    Remu(Reg, Reg, Reg)
}

impl Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instruction::Add(reg1, reg2, reg3) => write!(f, "add {reg1}, {reg2}, {reg3}"),
            Instruction::Sub(reg1, reg2, reg3) => write!(f, "sub {reg1}, {reg2}, {reg3}"),
            Instruction::Xor(reg1, reg2, reg3) => write!(f, "xor {reg1}, {reg2}, {reg3}"),
            Instruction::Or(reg1, reg2, reg3) => write!(f, "or {reg1}, {reg2}, {reg3}"),
            Instruction::And(reg1, reg2, reg3) => write!(f, "and {reg1}, {reg2}, {reg3}"),
            Instruction::Sll(reg1, reg2, reg3) => write!(f, "sll {reg1}, {reg2}, {reg3}"),
            Instruction::Srl(reg1, reg2, reg3) => write!(f, "srl {reg1}, {reg2}, {reg3}"),
            Instruction::Sra(reg1, reg2, reg3) => write!(f, "sra {reg1}, {reg2}, {reg3}"),
            Instruction::Slt(reg1, reg2, reg3) => write!(f, "slt {reg1}, {reg2}, {reg3}"),
            Instruction::Sltu(reg1, reg2, reg3) => write!(f, "sltu {reg1}, {reg2}, {reg3}"),

            Instruction::Addi(reg1, reg2, imm) => write!(f, "addi {reg1}, {reg2}, {imm}"),
            Instruction::Xori(reg1, reg2, imm) => write!(f, "xori {reg1}, {reg2}, {imm}"),
            Instruction::Ori(reg1, reg2, imm) => write!(f, "ori {reg1}, {reg2}, {imm}"),
            Instruction::Andi(reg1, reg2, imm) => write!(f, "andi {reg1}, {reg2}, {imm}"),
            Instruction::Slli(reg1, reg2, imm) => write!(f, "slli {reg1}, {reg2}, {imm}"),
            Instruction::Srli(reg1, reg2, imm) => write!(f, "srli {reg1}, {reg2}, {imm}"),
            Instruction::Srai(reg1, reg2, imm) => write!(f, "srai {reg1}, {reg2}, {imm}"),
            Instruction::Slti(reg1, reg2, imm) => write!(f, "slti {reg1}, {reg2}, {imm}"),
            Instruction::Sltiu(reg1, reg2, imm) => write!(f, "sltiu {reg1}, {reg2}, {imm}"),

            Instruction::Lb(reg1, reg2, imm) => write!(f, "lb {reg1}, {reg2}, {imm}"),
            Instruction::Lh(reg1, reg2, imm) => write!(f, "lh {reg1}, {reg2}, {imm}"),
            Instruction::Lw(reg1, reg2, imm) => write!(f, "lw {reg1}, {reg2}, {imm}"),
            Instruction::Lbu(reg1, reg2, imm) => write!(f, "lbu {reg1}, {reg2}, {imm}"),
            Instruction::Lhu(reg1, reg2, imm) => write!(f, "lhu {reg1}, {reg2}, {imm}"),

            Instruction::Sb(reg1, reg2, imm) => write!(f, "sb {reg1}, {reg2}, {imm}"),
            Instruction::Sh(reg1, reg2, imm) => write!(f, "sh {reg1}, {reg2}, {imm}"),
            Instruction::Sw(reg1, reg2, imm) => write!(f, "sw {reg1}, {reg2}, {imm}"),

            Instruction::Beq(reg1, reg2, imm) => write!(f, "beq {reg1}, {reg2}, {imm}"),
            Instruction::Bne(reg1, reg2, imm) => write!(f, "bne {reg1}, {reg2}, {imm}"),
            Instruction::Blt(reg1, reg2, imm) => write!(f, "blt {reg1}, {reg2}, {imm}"),
            Instruction::Bltu(reg1, reg2, imm) => write!(f, "bltu {reg1}, {reg2}, {imm}"),
            Instruction::Bge(reg1, reg2, imm) => write!(f, "bge {reg1}, {reg2}, {imm}"),
            Instruction::Bgeu(reg1, reg2, imm) => write!(f, "bgeu {reg1}, {reg2}, {imm}"),

            Instruction::Jal(reg, imm) => write!(f, "jal {reg}, {imm}"),
            Instruction::Jalr(reg1, reg2, imm) => write!(f, "jalr {reg1}, {reg2}, {imm}"),
            Instruction::Lui(reg, imm) => write!(f, "lui {reg}, {imm}"),
            Instruction::Auipc(reg, imm) => write!(f, "auipc {reg}, {imm}"),

            Instruction::Xnor(reg1, reg2, reg3) => write!(f, "xnor {reg1}, {reg2}, {reg3}"),
            Instruction::Equal(reg1, reg2, reg3) => write!(f, "equal {reg1}, {reg2}, {reg3}"),
            Instruction::Mul(reg1, reg2, reg3) => write!(f, "mul {reg1}, {reg2}, {reg3}"),
            Instruction::Mulh(reg1, reg2, reg3) => write!(f, "mulh {reg1}, {reg2}, {reg3}"),
            Instruction::Mulhu(reg1, reg2, reg3) => write!(f, "mulhu {reg1}, {reg2}, {reg3}"),
            Instruction::Mulhsu(reg1, reg2, reg3) => write!(f, "mulhsu {reg1}, {reg2}, {reg3}"),
            Instruction::Div(reg1, reg2, reg3) => write!(f, "div {reg1}, {reg2}, {reg3}"),
            Instruction::Divu(reg1, reg2, reg3) => write!(f, "divu {reg1}, {reg2}, {reg3}"),
            Instruction::Rem(reg1, reg2, reg3) => write!(f, "rem {reg1}, {reg2}, {reg3}"),
            Instruction::Remu(reg1, reg2, reg3) => write!(f, "remu {reg1}, {reg2}, {reg3}"),

            Instruction::Ecall => write!(f, "ecall"),
            Instruction::Ebreak => write!(f, "ebreak"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Operation {
    Namespace(usize),
    Instr(Instruction),
    Macro(MacroInstr),
    LablMacro(smartstring::alias::String, MacroInstr),
    LablInstr(smartstring::alias::String, Instruction),
    Labl(smartstring::alias::String)
}

impl Display for Operation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Operation::Namespace(space) => write!(f, "Namespace({space})"),
            Operation::Instr(instr) => write!(f, "{instr}"),
            Operation::Macro(macro_in) => write!(f, "{macro_in}"),
            Operation::LablMacro(label, macro_in) =>  write!(f, "{label}: {macro_in}"),
            Operation::LablInstr(label, instr) => write!(f, "{label}: {instr}"),
            Operation::Labl(label) => write!(f, "{label}:")
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum ByteData {
    Byte(i16),
    String(smartstring::alias::String)
}

impl From<i32> for ByteData {
    fn from(value: i32) -> Self {
        ByteData::Byte((value & (2_i32.pow(17) - 1)).try_into().expect("Could not cast number to byte!"))
    }
}

impl From<smartstring::alias::String> for ByteData {
    fn from(value: smartstring::alias::String) -> Self {
        ByteData::String(value)
    }
}

impl Display for ByteData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ByteData::Byte(imm) => write!(f, "{imm}"),
            ByteData::String(label) => write!(f, "{label}"),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum HalfData {
    Half(i32),
    String(smartstring::alias::String)
}

impl From<i32> for HalfData {
    fn from(value: i32) -> Self {
        HalfData::Half(value)
    }
}

impl From<smartstring::alias::String> for HalfData {
    fn from(value: smartstring::alias::String) -> Self {
        HalfData::String(value)
    }
}

impl Display for HalfData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            HalfData::Half(imm) => write!(f, "{imm}"),
            HalfData::String(label) => write!(f, "{label}"),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum WordData {
    Word(i64),
    String(smartstring::alias::String)
}

impl From<i128> for WordData {
    fn from(value: i128) -> Self {
        WordData::Word((value & (2_i128.pow(65) - 1)).try_into().expect("Could not cast number to word!"))
    }
}

impl From<smartstring::alias::String> for WordData {
    fn from(value: smartstring::alias::String) -> Self {
        WordData::String(value)
    }
}

impl Display for WordData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            WordData::Word(imm) => write!(f, "{imm}"),
            WordData::String(label) => write!(f, "{label}"),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum DWordData {
    DWord(i128),
    String(smartstring::alias::String)
}

impl From<i128> for DWordData {
    fn from(value: i128) -> Self {
        DWordData::DWord(value)
    }
}

impl From<smartstring::alias::String> for DWordData {
    fn from(value: smartstring::alias::String) -> Self {
        DWordData::String(value)
    }
}

impl Display for DWordData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DWordData::DWord(imm) => write!(f, "{imm}"),
            DWordData::String(label) => write!(f, "{label}"),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum MemData {
    Bytes(Vec<ByteData>, bool),
    Halfs(Vec<HalfData>),
    Words(Vec<WordData>),
    DWords(Vec<DWordData>),
    Namespace(usize)
}

impl Display for MemData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MemData::Bytes(vec_data, _) => {
                write!(f, ".byte {}", vec_data[0])?;
                for byte in &vec_data[1..] {
                    write!(f, ", {}", byte)?;
                }
                Ok(())
            },
            MemData::Halfs(vec_data) => {
                write!(f, ".half {}", vec_data[0])?;
                for half in &vec_data[1..] {
                    write!(f, ", {}", half)?;
                }
                Ok(())
            },
            MemData::Words(vec_data) => {
                write!(f, ".word {}", vec_data[0])?;
                for word in &vec_data[1..] {
                    write!(f, ", {}", word)?;
                }
                Ok(())
            },
            MemData::DWords(vec_data) => {
                write!(f, ".dword {}", vec_data[0])?;
                for dword in &vec_data[1..] {
                    write!(f, ", {}", dword)?;
                }
                Ok(())
            },
            MemData::Namespace(space) => write!(f, "Namespace({space})"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum LabelType {
    Data,
    Address,
    Uninit
}

#[derive(Debug, Clone, PartialEq)]
// Scope for locality: true for global; false for local
pub struct LabelElem {
    name: smartstring::alias::String,
    definition: i128,
    ltype: LabelType,
    scope: bool,
    referenced: bool
}

impl LabelElem {
    pub fn new() -> LabelElem {
        let name = smartstring::alias::String::new();
        let definition: i128 = 0;
        let ltype = LabelType::Uninit;
        let scope = false;
        let referenced = false;

        LabelElem {
            name,
            definition,
            ltype,
            scope, 
            referenced 
        }
    }

    #[allow(dead_code)]
    pub fn new_def(name: smartstring::alias::String, definition: i128) -> LabelElem {
        let mut elem = LabelElem::new();
        elem.set_name(name);
        elem.set_def(definition);
        elem
    }

    pub fn new_refd(name: smartstring::alias::String) -> LabelElem {
        let mut elem = LabelElem::new();
        elem.set_name(name);
        elem.set_refd();
        elem
    }

    pub fn combine(&mut self, other: &LabelElem) -> Result<&str, CommonError> {
        if self.name.ne(&other.name) || self.scope != other.scope {
            return Err(CommonError::LabelsNameNotEqual(self.clone(), other.clone()));
        }

        if self.ltype != LabelType::Uninit && other.ltype != LabelType::Uninit {
            return Err(CommonError::MultipleGlobalDefined(self.clone()));
        } else if self.ltype == LabelType::Uninit && other.ltype != LabelType::Uninit {
            self.definition = other.definition;
            self.ltype = other.ltype.clone();
        }

        if self.referenced || other.referenced {
            self.referenced = true;
        }

        Ok("Labels combined!")
    }

    pub fn set_name(&mut self, name: smartstring::alias::String) {
        self.name = name;
    }

    pub fn get_name(&self) -> &smartstring::alias::String {
        &self.name
    }

    pub fn set_scope(&mut self, scope: bool) {
        self.scope = scope;
    }

    pub fn get_scope(&self) -> bool {
        self.scope
    }

    pub fn set_def(&mut self, definition: i128) {
        self.definition = definition;
    }

    pub fn add_def(&mut self, offset: i128) {
        self.definition += offset;
    }

    pub fn get_def(&self) -> &i128 {
        &self.definition
    }

    pub fn set_refd(&mut self) {
        self.referenced = true;
    }

    pub fn set_type(&mut self, ltype: LabelType) {
        self.ltype = ltype;
    }

    #[allow(dead_code)]
    pub fn get_type(&mut self) -> &LabelType {
        &self.ltype
    }
}

impl Default for LabelElem {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct LabelRecog {
    label_map: HashMap<smartstring::alias::String, usize>,
    label_list: Vec<LabelElem>,
}

impl LabelRecog {
    pub fn new() -> LabelRecog {
        let label_list: Vec<LabelElem> = vec![];
        let label_map: HashMap<smartstring::alias::String, usize> = HashMap::new();

        LabelRecog {
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

    pub fn get_label(&mut self, label_str: &smartstring::alias::String) -> Option<&mut LabelElem> {
        match self.label_map.get(label_str) {
            Some(val) => self.label_list.get_mut(*val),
            None => None,
        }
    }

    pub fn crt_or_def_label(&mut self, label_str: &smartstring::alias::String, scope: bool, ltype: LabelType, definition: i128) -> Result<(), CommonError> {
        match self.get_label(label_str) {
            Some(label) => {
                if *label.get_type() != LabelType::Uninit {
                    return Err(CommonError::LabelAlreadyDefined(label.clone()))
                }
                label.set_def(definition);
                label.set_type(ltype);
                label.set_scope(scope);
            },
            None => {
                let mut label = LabelElem::new();
                label.set_name(label_str.clone());
                label.set_def(definition);
                label.set_type(ltype);
                label.set_scope(scope);
                let _ = self.insert_label(label);
            },
        }
        Ok(())
    }

    // Creates a label, if it does not exist already with the name label_str, scope and the reference.
    // Returns true, if there is already a definition, else false.
    pub fn crt_or_ref_label(&mut self, label_str: &smartstring::alias::String) {
        match self.get_label(label_str) {
            Some(label) => label.set_refd(),
            None => {
                let mut label = LabelElem::new_refd(label_str.clone());
                label.set_scope(true);
                let _ = self.insert_label(label);
            },
        }
    }

    pub fn crt_def_ref(&mut self, label_str: &smartstring::alias::String, scope: bool, ltype: LabelType, definition: i128) {
        if self.get_label(label_str).is_none() {
            let mut label = LabelElem::new();
            label.set_name(label_str.clone());
            label.set_def(definition);
            label.set_type(ltype);
            label.set_refd();
            label.set_scope(scope);
            let _ = self.insert_label(label);
        }
    }

    pub fn set_refd_label(&mut self, label_str: &smartstring::alias::String) {
        if let Some(label) = self.get_label(label_str) {
            label.set_refd();
        }
    }

    #[allow(dead_code)]
    pub fn get_local_labels(&self) -> Vec<&LabelElem> {
        let mut local_labels: Vec<&LabelElem> = vec![];
        for label in self.label_list.iter() {
            if !label.get_scope() {
                local_labels.push(label);
            }
        }
        local_labels
    }

    pub fn get_global_labels(&self) -> Vec<&LabelElem> {
        let mut global_labels: Vec<&LabelElem> = vec![];
        for label in self.label_list.iter() {
            if label.get_scope() {
                global_labels.push(label);
            }
        }
        global_labels
    }

    pub fn add_offset(&mut self, offset: i128, ltype: LabelType) {
        if ltype == LabelType::Uninit {
            return;
        }
        for lblelm in self.label_list.iter_mut().filter(|e| e.ltype == ltype) {
            lblelm.add_def(offset);
        }
    }
}

impl Display for LabelRecog {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "LabelRecog {{ label_list: {:?} }}", self.label_list)
    }
}

impl Default for LabelRecog {
    fn default() -> Self {
        Self::new()
    }
}

pub trait AssemblyCode {
    type Labels;

    fn new(labels: Self::Labels) -> Self;
    fn get_labels_refmut(&mut self) -> &mut Self::Labels;
    fn get_data_refmut(&mut self) -> &mut Vec<MemData>;
    fn get_text_refmut(&mut self) -> &mut Vec<Operation>;
    fn get_all_refmut(&mut self) -> (&mut Self::Labels, &mut Vec<Operation>, &mut Vec<MemData>);
}

pub struct AssemblyCodeRecog {
    labels: LabelRecog,
    data: Vec<MemData>,
    text: Vec<Operation>,
    subroutine: Subroutines
}

impl AssemblyCodeRecog {
    pub fn set_text(&mut self, other: Vec<Operation>) {
        self.text = other
    }

    pub fn set_data(&mut self, other: Vec<MemData>) {
        self.data = other
    }

    pub fn get_subroutine_refmut(&mut self) -> &mut Subroutines {
        &mut self.subroutine
    }
}

impl AssemblyCode for AssemblyCodeRecog {
    type Labels = LabelRecog;

    fn new(labels: Self::Labels) -> Self {
        AssemblyCodeRecog { labels, data: vec![], text: vec![], subroutine: Subroutines::new() }
    }

    fn get_labels_refmut(&mut self) -> &mut Self::Labels {
        &mut self.labels
    }

    fn get_data_refmut(&mut self) -> &mut Vec<MemData> {
        &mut self.data
    }

    fn get_text_refmut(&mut self) -> &mut Vec<Operation> {
        &mut self.text
    }

    fn get_all_refmut(&mut self) -> (&mut Self::Labels, &mut Vec<Operation>, &mut Vec<MemData>) {
        (&mut self.labels, &mut self.text, &mut self.data)
    }
}

#[derive(Debug, PartialEq)]
pub struct AssemblyCodeNamespaces {
    labels: Namespaces,
    data: Vec<MemData>,
    text: Vec<Operation>
}

impl AssemblyCode for AssemblyCodeNamespaces {
    type Labels = Namespaces;

    fn new(labels: Self::Labels) -> Self {
        AssemblyCodeNamespaces { labels, data: vec![], text: vec![] }
    }

    fn get_labels_refmut(&mut self) -> &mut Self::Labels {
        &mut self.labels
    }

    fn get_data_refmut(&mut self) -> &mut Vec<MemData> {
        &mut self.data
    }

    fn get_text_refmut(&mut self) -> &mut Vec<Operation> {
        &mut self.text
    }

    fn get_all_refmut(&mut self) -> (&mut Self::Labels, &mut Vec<Operation>, &mut Vec<MemData>) {
        (&mut self.labels, &mut self.text, &mut self.data)
    }
}

#[derive(Debug, PartialEq)]
pub struct TranslatableCode {
    data: Vec<MemData>,
    text: Vec<Instruction>
}

impl TranslatableCode {
    #[allow(dead_code)]
    pub fn new() -> Self {
        TranslatableCode { data: vec![], text: vec![] }
    }

    pub fn new_with_data(data: Vec<MemData>) -> Self {
        TranslatableCode { data, text: vec![] }
    }

    #[allow(dead_code)]
    pub fn get_data_refmut(&mut self) -> &mut Vec<MemData> {
        &mut self.data
    }

    pub fn get_text_refmut(&mut self) -> &mut Vec<Instruction> {
        &mut self.text
    }

    pub fn get_all_ref(&self) -> (&Vec<Instruction>, &Vec<MemData>) {
        (&self.text, &self.data)
    }
}

impl Default for TranslatableCode {
    fn default() -> Self {
        Self::new()
    }
}
