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

use std::collections::HashMap;
use std::borrow::Cow;

pub type Imm = i32; // always less than 32

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

#[derive(Debug, Clone, PartialEq)]
pub enum MacroInstr {
    Beq(Reg, Reg, String),
    Bne(Reg, Reg, String),
    Blt(Reg, Reg, String),
    Bltu(Reg, Reg, String),
    Bge(Reg, Reg, String),
    Bgeu(Reg, Reg, String),

    Jal(Reg, String),
    Jalr(Reg, Reg, String),

    Lui(Reg, String),
    Auipc(Reg, String),

    Slli(Reg, Reg, String),
    Srli(Reg, Reg, String),
    Srai(Reg, Reg, String),
    
    Lb(Reg, Reg, String), //Load byte
    Lh(Reg, Reg, String), //Load half
    Lw(Reg, Reg, String), //Load word
    
    Lbu(Reg, Reg, String),
    Lhu(Reg, Reg, String),
    
    Sb(Reg, Reg, String), //Store byte
    Sh(Reg, Reg, String), //Store half
    Sw(Reg, Reg, String), //Store word

    // If there is time and someone has nothing to do..
    //Subi(Reg, Reg, Imm),
    //Muli(Reg, Reg, Imm),
    //Divi(Reg, Reg, Imm),
    
    // Not needed. But can be done, for user experience
    //Divcn(Reg, Reg, Reg),
    //Mulcn(Reg, Reg, Reg),
    //Subcn(Reg, Reg, Reg),
    //Addcn(Reg, Reg, Reg),
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

    //VJmp(String),
    //VBt(String),
    //VBf(String),

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
    LablMacro(Cow<'a, str>, MacroInstr),
    LablInstr(Cow<'a, str>, Instruction),
    Labl(Cow<'a, str>)
}

#[derive(Debug, Clone, PartialEq)]
// Scope for locality: true for global; false for local
pub struct LabelElem {
    name: String,
    definition: i128,
    scope: bool,
    referenced: bool
}

impl LabelElem {
    pub fn new() -> LabelElem {
        let name = String::new();
        let definition: i128 = -1;
        let scope = false;
        //let references: Box<HashSet<usize>> = Box::from(HashSet::new());
        let referenced = false;

        LabelElem {
            name,
            definition,
            scope, 
            referenced 
        }
    }

    #[allow(dead_code)]
    pub fn new_def(name: String, definition: i128) -> LabelElem {
        let mut elem = LabelElem::new();
        elem.set_name(name);
        elem.set_def(definition);
        elem
    }

    pub fn new_refd(name: String) -> LabelElem {
        let mut elem = LabelElem::new();
        elem.set_name(name);
        elem.set_refd();
        elem
    }

    // TODO: Custom error struct
    pub fn combine(&mut self, other: &LabelElem) -> Result<&str, &str> {
        if self.name.ne(&other.name) || self.scope != other.scope {
            return Err("Labels are not the same!");
        }

        if self.definition != -1 && other.definition != -1 {
            return Err("Global label defined in two files!");
        } else if self.definition == -1 && other.definition != -1 {
            self.definition = other.definition;
        }

        if self.referenced || other.referenced {
            self.referenced = true;
        }

        Ok("Labels combined!")
    }

    pub fn set_name(&mut self, name: String) {
        self.name = name;
    }

    pub fn get_name(&self) -> &String {
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
}

#[derive(Debug, PartialEq, Clone)]
pub struct LabelRecog {
    label_map: HashMap<String, usize>,
    label_list: Vec<LabelElem>,
}

impl LabelRecog {
    pub fn new() -> LabelRecog {
        let label_list: Vec<LabelElem> = vec![];
        let label_map: HashMap<String, usize> = HashMap::new();

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

    pub fn get_label(&mut self, label_str: &String) -> Option<&mut LabelElem> {
        match self.label_map.get(label_str) {
            Some(val) => self.label_list.get_mut(*val),
            None => None,
        }
    }

    pub fn crt_or_def_label(&mut self, label_str: &String, scope: bool, definition: i128) {
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
    pub fn crt_or_ref_label(&mut self, label_str: &String, scope: bool) -> bool {
        match self.get_label(label_str) {
            Some(label) => {
                label.set_refd();
                if !label.get_scope() {
                    *label.get_def() != -1
                } else {
                    false
                }
            },
            None => {
                let mut label = LabelElem::new_refd(label_str.clone());
                label.set_scope(scope);
                let _ = self.insert_label(label);
                false
            },
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

    pub fn add_offset(&mut self, offset: i128) {
        for lblelm in self.label_list.iter_mut().filter(|e| *e.get_def() != -1) {
            lblelm.add_def(offset);
        }
    }
}