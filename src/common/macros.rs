/*
 * Copyright (c) 2023 Steven Becker
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

//
// This module defines the data structure for macros.

use std::fmt::Display;

use super::Reg;
use super::Instruction;
use super::Imm;

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
    Auipc(Reg, smartstring::alias::String),
    
    LbLabl(Reg, Reg, smartstring::alias::String, Part), //Load byte
    LhLabl(Reg, Reg, smartstring::alias::String, Part), //Load half
    LwLabl(Reg, Reg, smartstring::alias::String, Part), //Load word
    LbImm(Reg, Reg, Imm), //Load byte
    LhImm(Reg, Reg, Imm), //Load half
    LwImm(Reg, Reg, Imm), //Load word
    
    LbuLabl(Reg, Reg, smartstring::alias::String),
    LhuLabl(Reg, Reg, smartstring::alias::String),
    LbuImm(Reg, Reg, Imm),
    LhuImm(Reg, Reg, Imm),
    
    SbLabl(Reg, Reg, smartstring::alias::String), //Store byte
    ShLabl(Reg, Reg, smartstring::alias::String), //Store half
    SwLabl(Reg, Reg, smartstring::alias::String), //Store word

    SbImm(Reg, Reg, Imm), //Store byte
    ShImm(Reg, Reg, Imm), //Store half
    SwImm(Reg, Reg, Imm), //Store word

    Addi(Reg, Reg, smartstring::alias::String, Part),

    Srr(Reg, Reg, Imm),
    Slr(Reg, Reg, Imm),

    Li(Reg, Imm),
    La(Reg, smartstring::alias::String),

    Call(smartstring::alias::String),
    Tail(smartstring::alias::String),

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
            MacroInstr::Auipc(reg, label) => write!(f, "auipc {reg}, {label}"),
            
            MacroInstr::LbLabl(reg1, reg2, label, part) => {
                if let Part::None = part {
                    write!(f, "lb {reg1}, {label}")
                } else {
                    write!(f, "lb {reg1}, %lo({label})({reg2})")
                }
            },
            MacroInstr::LhLabl(reg1, reg2, label, part) => {
                if let Part::None = part {
                    write!(f, "lh {reg1}, {label}")
                } else {
                    write!(f, "lh {reg1}, %lo({label})({reg2})")
                }
            },
            MacroInstr::LwLabl(reg1, reg2, label, part) => {
                if let Part::None = part {
                    write!(f, "lw {reg1}, {label}")
                } else {
                    write!(f, "lw {reg1}, %lo({label})({reg2})")
                }
            },
            
            MacroInstr::LbuLabl(reg1, _, label) => write!(f, "lbu {reg1}, {label}"),
            MacroInstr::LhuLabl(reg1, _, label) => write!(f, "lhu {reg1}, {label}"),
            
            MacroInstr::SbLabl(reg1, reg2, label) => write!(f, "sb {reg1}, {label}, {reg2}"),
            MacroInstr::ShLabl(reg1, reg2, label) => write!(f, "sh {reg1}, {label}, {reg2}"),
            MacroInstr::SwLabl(reg1, reg2, label) => write!(f, "sw {reg1}, {label}, {reg2}"),

            MacroInstr::SbImm(reg1, reg2, imm) => write!(f, "sb {reg1}, {imm}, {reg2}"),
            MacroInstr::ShImm(reg1, reg2, imm) => write!(f, "sh {reg1}, {imm}, {reg2}"),
            MacroInstr::SwImm(reg1, reg2, imm) => write!(f, "sw {reg1}, {imm}, {reg2}"),

            MacroInstr::Addi(reg1, reg2, label, _) => write!(f, "addi {reg1}, {reg2}, {label}"),

            MacroInstr::Srr(reg1, reg2, imm) => write!(f, "srr {reg1}, {reg2}, {imm}"),
            MacroInstr::Slr(reg1, reg2, imm) => write!(f, "slr {reg1}, {reg2}, {imm}"),

            MacroInstr::Li(reg, imm) => write!(f, "li {reg}, {imm}"),
            MacroInstr::La(reg, label) => write!(f, "la {reg}, {label}"),

            MacroInstr::Call(label) => write!(f, "call {label}"),
            MacroInstr::Tail(label) => write!(f, "tail {label}"),

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
            MacroInstr::RepInstr(imm, instr) => write!(f, "rep {imm}, {:?}", instr),

            MacroInstr::LbImm(reg1, _, imm) => write!(f, "lb {reg1}, {imm}"),
            MacroInstr::LhImm(reg1, _, imm) => write!(f, "lh {reg1}, {imm}"),
            MacroInstr::LwImm(reg1, _, imm) => write!(f, "lw {reg1}, {imm}"),
            MacroInstr::LbuImm(reg1, _, imm) => write!(f, "lbu {reg1}, {imm}"),
            MacroInstr::LhuImm(reg1, _, imm) => write!(f, "lhu {reg1}, {imm}"),
        }
    }
}