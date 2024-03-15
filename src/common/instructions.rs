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
use super::Imm;

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