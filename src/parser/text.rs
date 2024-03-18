/*
 * Copyright (c) 2023 Steven Becker
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

use nom::{
    IResult,
    branch::alt,
    combinator::{
        map,
        success,
        fail,
        into
    },
    sequence::{
        pair,
        separated_pair
    },
    error::context
};
use std::{any::Any, cmp::Ordering};
use std::collections::BTreeMap;
use log::{debug, error, log_enabled};

use crate::parser::{
    handle_label_defs, instructions::parse_instruction, literals::{
        parse_label_definition,
        parse_label_definition_priv
    }, Subroutines
};
use crate::common::*;

use self::errors::ParserError;

use super::parse_multiline_comments;

impl Subroutines {
    const SRR_SUB: &'static str = r#"
_SRR:
    sub a4, zero, a1
    srl a2, a0, a1
    sll a3, a0, a4
    or a0, a2, a3
    ret
"#;
    const SLR_SUB: &'static str = r#"
_SLR:
    sub a4, zero, a1
    sll a2, a0, a1
    srl a3, a0, a4
    or a0, a2, a3
    ret
"#;

    pub fn srr_defined(&mut self) {
        self.code_str_vec.insert(Self::SRR_SUB.to_string());
    }

    pub fn slr_defined(&mut self) {
        self.code_str_vec.insert(Self::SLR_SUB.to_string());
    }
}

#[derive(Debug, Clone)]
pub struct LabelInsertError {
    label: String,
}

impl LabelInsertError {
    #[allow(dead_code)]
    pub fn new(label: String) -> LabelInsertError {
        LabelInsertError { label }
    }
}

impl std::fmt::Display for LabelInsertError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{} already exists!", self.label)
    }
}

fn parse_line(input: &str) -> IResult<&str, Box<dyn LineHandle>> {
    let (rest, early) = parse_multiline_comments(input)?;
    if early {
        return Ok((rest, Box::from(NoData {})))
    }
    into(
        alt((
        separated_pair(
            map(parse_label_definition, Some),
            parse_multiline_comments,
            map(
                parse_instruction,
                Some
            )
        ),
        pair(
            map(parse_label_definition, Some), 
            success(None)
        ),
        pair(
            success(None),
            map(
                parse_instruction,
                Some
            )
        ),
    )))(rest)
}

fn parse_line_priv(input: &str) -> IResult<&str, Box<dyn LineHandle>> {
    let (rest, early) = parse_multiline_comments(input)?;
    if early {
        return Ok((rest, Box::from(NoData {})))
    }
    into(
        alt((
        separated_pair(
            map(parse_label_definition_priv, Some),
            parse_multiline_comments,
            map(
                parse_instruction,
                Some
            )
        ),
        pair(
            map(parse_label_definition_priv, Some), 
            success(None)
        ),
        pair(
            success(None),
            map(
                parse_instruction,
                Some
            )
        ),
    )))(rest)
}

fn handle_label_refs(macro_in: &MacroInstr, subroutines: &mut Option<&mut Subroutines>, symbol_map: &mut LabelRecog) {
    match macro_in {
        MacroInstr::Addi(_, _, labl, _) |

        MacroInstr::Beq(_, _, labl) | 
        MacroInstr::Bne(_, _, labl) |
        MacroInstr::Blt(_, _, labl) |
        MacroInstr::Bltu(_, _, labl) |
        MacroInstr::Bge(_, _, labl) |
        MacroInstr::Bgeu(_, _, labl) |

        MacroInstr::Jal(_, labl) |
        MacroInstr::Jalr(_, _, labl, _) |

        MacroInstr::Lui(_, labl, _) |
        MacroInstr::Auipc(_, labl, _) |

        MacroInstr::Slli(_, _, labl) |
        MacroInstr::Srli(_, _, labl) |
        MacroInstr::Srai(_, _, labl) |

        MacroInstr::Lb(_, _, labl, _) |
        MacroInstr::Lh(_, _, labl, _) |
        MacroInstr::Lw(_, _, labl, _) |

        MacroInstr::Lbu(_, _, labl) |
        MacroInstr::Lhu(_, _, labl) |

        MacroInstr::Sb(_, _, labl, _) |
        MacroInstr::Sh(_, _, labl, _) |
        MacroInstr::Sw(_, _, labl, _) | 
        
        MacroInstr::Call(labl) |
        MacroInstr::Tail(labl) |
        MacroInstr::La(_, labl) => {
            symbol_map.crt_or_ref_label(labl);
        },
        MacroInstr::Srr(_, _, _) => {
            if let Some(subs) = subroutines {
                subs.srr_defined();
            };
            static LABEL: &str = "_SRR";
            symbol_map.crt_or_ref_label(&LABEL.into());
        },
        MacroInstr::Slr(_, _, _) => {
            if let Some(subs) = subroutines {
                subs.slr_defined();
            };
            static LABEL: &str = "_SLR";
            symbol_map.crt_or_ref_label(&LABEL.into());
        },

        _ => (),
    }
}

fn handle_abs_addr_label_conv(
    instr_counter: usize,
    abs_to_label_queue: &mut BTreeMap<usize, Vec<usize>>, 
    instr_list: &mut [Operation],
    symbol_map: &mut LabelRecog,
    imm: &Imm
) -> Option<smartstring::alias::String> {
    let mut jump_line: usize = match instr_counter as i128 + (*imm / 4) as i128 {
        x if x < 0 => 0,
        x => x.try_into().unwrap()
    };

    match imm.cmp(&0) {
        Ordering::Greater => {
            // cannot look ahead, delegate to later
            jump_line += 1;
            match abs_to_label_queue.get_mut(&jump_line) {
                Some(list) => list.push(instr_counter),
                None => {
                    abs_to_label_queue.insert(jump_line, Vec::from([instr_counter]));
                },
            }
            None
        },
        Ordering::Less => {
            // looking back
            let jump_label: smartstring::alias::String;
            match &instr_list[jump_line] {
                Operation::Instr(instr) => {
                    jump_label = smartstring::alias::String::from("__") + jump_line.to_string().as_str();
                    symbol_map.crt_def_ref(&jump_label, false, LabelType::Address, jump_line as i128);
                    instr_list[jump_line] = Operation::LablInstr(jump_label.clone(), instr.to_owned());
                },
                Operation::Macro(macro_in) => {
                    jump_label = smartstring::alias::String::from("__") + jump_line.to_string().as_str();
                    symbol_map.crt_def_ref(&jump_label, false, LabelType::Address, jump_line as i128);
                    instr_list[jump_line] = Operation::LablMacro(jump_label.clone(), macro_in.to_owned());
                },
                Operation::LablInstr(labl, _) |
                Operation::LablMacro(labl, _) |
                Operation::Labl(labl) => {
                    jump_label = labl.clone();
                    symbol_map.set_refd_label(labl);
                },
                Operation::Namespace(_) => unreachable!(),
            };
            Some(jump_label)
        },
        Ordering::Equal => None,
    }
}

fn handle_instr_substitution(instr_list: &mut [Operation], elem: &[usize], jump_label: &str) {
    for origin in elem.iter() {
        match &instr_list[*origin] {
            Operation::Instr(instr) => {
                match instr {
                    Instruction::Beq(reg1, reg2, _) => instr_list[*origin] = Operation::Macro(MacroInstr::Beq(reg1.to_owned(), reg2.to_owned(), jump_label.into())),
                    Instruction::Bne(reg1, reg2, _) => instr_list[*origin] = Operation::Macro(MacroInstr::Bne(reg1.to_owned(), reg2.to_owned(), jump_label.into())),
                    Instruction::Blt(reg1, reg2, _) => instr_list[*origin] = Operation::Macro(MacroInstr::Blt(reg1.to_owned(), reg2.to_owned(), jump_label.into())),
                    Instruction::Bltu(reg1, reg2, _) => instr_list[*origin] = Operation::Macro(MacroInstr::Bltu(reg1.to_owned(), reg2.to_owned(), jump_label.into())),
                    Instruction::Bge(reg1, reg2, _) => instr_list[*origin] = Operation::Macro(MacroInstr::Bge(reg1.to_owned(), reg2.to_owned(), jump_label.into())),
                    Instruction::Bgeu(reg1, reg2, _) => instr_list[*origin] = Operation::Macro(MacroInstr::Bgeu(reg1.to_owned(), reg2.to_owned(), jump_label.into())),
                    Instruction::Jal(reg, _) => instr_list[*origin] = Operation::Macro(MacroInstr::Jal(reg.to_owned(), jump_label.into())),
                    Instruction::Jalr(reg1, reg2, _) => instr_list[*origin] = Operation::Macro(MacroInstr::Jalr(reg1.to_owned(), reg2.to_owned(), jump_label.into(), Part::None)),
                    op => {
                        println!("Matched instr: {:?}", op);
                        unreachable!()
                    },
                }
            },
            Operation::LablInstr(labl, instr) => {
                match instr {
                    Instruction::Beq(reg1, reg2, _) => instr_list[*origin] = Operation::LablMacro(labl.clone(), MacroInstr::Beq(reg1.to_owned(), reg2.to_owned(), jump_label.into())),
                    Instruction::Bne(reg1, reg2, _) => instr_list[*origin] = Operation::LablMacro(labl.clone(), MacroInstr::Bne(reg1.to_owned(), reg2.to_owned(), jump_label.into())),
                    Instruction::Blt(reg1, reg2, _) => instr_list[*origin] = Operation::LablMacro(labl.clone(), MacroInstr::Blt(reg1.to_owned(), reg2.to_owned(), jump_label.into())),
                    Instruction::Bltu(reg1, reg2, _) => instr_list[*origin] = Operation::LablMacro(labl.clone(), MacroInstr::Bltu(reg1.to_owned(), reg2.to_owned(), jump_label.into())),
                    Instruction::Bge(reg1, reg2, _) => instr_list[*origin] = Operation::LablMacro(labl.clone(), MacroInstr::Bge(reg1.to_owned(), reg2.to_owned(), jump_label.into())),
                    Instruction::Bgeu(reg1, reg2, _) => instr_list[*origin] = Operation::LablMacro(labl.clone(), MacroInstr::Bgeu(reg1.to_owned(), reg2.to_owned(), jump_label.into())),
                    Instruction::Jal(reg, _) => instr_list[*origin] = Operation::LablMacro(labl.clone(), MacroInstr::Jal(reg.to_owned(), jump_label.into())),
                    Instruction::Jalr(reg1, reg2, _) => instr_list[*origin] = Operation::LablMacro(labl.clone(), MacroInstr::Jalr(reg1.to_owned(), reg2.to_owned(), jump_label.into(), Part::None)),
                    op => {
                        println!("Matched labl: {}, matched instr: {:?}", labl, op);
                        unreachable!()
                    },
                }
            },
            op => {
                println!("Matched operation: {:?}", op);
                unreachable!()
            },
        }
    }
}

fn split_list(instr_list: &mut Vec<Operation>, pointer: &usize) -> (Vec<Operation>, Vec<Operation>) {
    let mut right_list = instr_list.split_off(*pointer);
    if !right_list.is_empty() {
        right_list.remove(0);
    }
    (right_list, vec![])
}

fn incorporate_changes(
    instr_list: &mut Vec<Operation>,
    mid_list: &mut Vec<Operation>,
    right_list: &mut Vec<Operation>,
    accumulator: &mut i128,
    pointer: &mut usize,
    label: Option<smartstring::alias::String>
) {
    *accumulator += (mid_list.len() - 1) as i128;
    *pointer += mid_list.len();
    if let Some(labl) = label {
        match mid_list.first().unwrap() {
            Operation::Instr(instr_in_sec) => mid_list[0] = Operation::LablInstr(labl, instr_in_sec.to_owned()),
            Operation::Macro(macro_in_sec) => mid_list[0] = Operation::LablMacro(labl, macro_in_sec.to_owned()),
            _ => unreachable!(),
        }
    }
    instr_list.append(mid_list);
    instr_list.append(right_list);
}

fn handle_multiline_immediate(
    imm: &mut i32, 
    label: Option<smartstring::alias::String>,
    pointer: &mut usize, 
    instr_list: &mut Vec<Operation>,
    instr: &Instruction
) -> bool {

    if *imm & 0x800 == 2048 {
        if imm.leading_ones() >= 19 {
            // just addi
            match label {
                Some(labl) => instr_list.insert(*pointer,
                                Operation::LablInstr(labl, instr.to_owned())),
                None => instr_list.insert(*pointer, instr.to_owned().into()),
            }
            *pointer += 1;
            return true
        }

        let mut mask: i32 = 4096;
        for _ in 12..32 {
            let is_unset = *imm & mask == 0;
            if is_unset {
                *imm |= mask;
                break;
            }
            mask <<= 1;
        }
        return false
    }
    
    if imm.leading_zeros() >= 19 {
        // If imm fits into 12 bits, then only use addi
        match label {
            Some(labl) => instr_list.insert(*pointer,
                            Operation::LablInstr(labl, instr.to_owned())),
            None => instr_list.insert(*pointer, instr.to_owned().into()),
        }
        *pointer += 1;
        return true
    }

    false
}

#[inline]
fn debug_operation_vec(macro_in: &MacroInstr, pointer: &usize, mid_list: &[Operation]) {
    let mut print_string = format!("Expanded '{macro_in}' at {} into '[{}", *pointer, mid_list[0]);

    for op in &mid_list[1..] {
        print_string.push_str(&("; ".to_owned() + op.to_string().as_str()));
    }

    print_string.push_str("]'");

    debug!("{print_string}");
}

fn translate_macros(
    macro_in: &MacroInstr,
    instr_list: &mut Vec<Operation>,
    accumulator: &mut i128,
    pointer: &mut usize,
    label: Option<smartstring::alias::String>
) {
    match &macro_in {
        MacroInstr::Srr(reg1, reg2, imm) => {
            let (mut right_list, mut mid_list) = split_list(instr_list, pointer);

            if *reg2 != Reg::G10 {
                mid_list.push(Instruction::Addi(Reg::G10, reg2.to_owned(), 0).into());
            }
            mid_list.push(Instruction::Addi(Reg::G11, Reg::G0, *imm).into());
            mid_list.push(MacroInstr::Jal(Reg::G1, "_SRR".into()).into());
            if *reg1 != Reg::G10 {
                mid_list.push(Instruction::Addi(reg1.to_owned(), Reg::G10, 0).into());
            }

            if log_enabled!(log::Level::Debug) {
                debug_operation_vec(macro_in, pointer, &mid_list);
            }

            incorporate_changes(instr_list, &mut mid_list, &mut right_list, accumulator, pointer, label);
        },
        MacroInstr::Slr(reg1, reg2, imm) => {
            let (mut right_list, mut mid_list) = split_list(instr_list, pointer);

            if *reg2 != Reg::G10 {
                mid_list.push(Instruction::Addi(Reg::G10, reg2.to_owned(), 0).into());
            }
            mid_list.push(Instruction::Addi(Reg::G11, Reg::G0, *imm).into());
            mid_list.push(MacroInstr::Jal(Reg::G1, "_SLR".into()).into());
            if *reg1 != Reg::G10 {
                mid_list.push(Instruction::Addi(reg1.to_owned(), Reg::G10, 0).into());
            }

            if log_enabled!(log::Level::Debug) {
                debug_operation_vec(macro_in, pointer, &mid_list);
            }

            incorporate_changes(instr_list, &mut mid_list, &mut right_list, accumulator, pointer, label);
        },
        MacroInstr::Li(reg, imm) => {
            instr_list.remove(*pointer);
            let mut imm_used = *imm;

            if handle_multiline_immediate(&mut imm_used, label.clone(), pointer, instr_list, &Instruction::Addi(reg.to_owned(), Reg::G0, *imm)) {
                debug!("Expanded '{macro_in}' at {} into '{}'", *pointer - 1, instr_list.last().unwrap());
                return
            }

            match label {
                Some(labl) => instr_list.insert(*pointer,
                                Operation::LablInstr(labl, Instruction::Lui(reg.to_owned(), imm_used))),
                None => instr_list.insert(*pointer, Instruction::Lui(reg.to_owned(), imm_used).into()),
            }
            *pointer += 1;
            instr_list.insert(*pointer,
            Instruction::Addi(reg.to_owned(), reg.to_owned(), imm_used).into());

            debug!("Expanded '{macro_in}' at {} into '[{}; {}]'", *pointer - 1, instr_list[*pointer-1], instr_list[*pointer]);

            *pointer += 1;
            *accumulator += 1;
        },
        MacroInstr::La(reg, targ_labl) => {
            instr_list.remove(*pointer);
            match label {
                Some(labl) => instr_list.insert(*pointer,
                                Operation::LablMacro(labl, MacroInstr::Auipc(reg.to_owned(), targ_labl.clone(), Part::Upper))),
                None => instr_list.insert(*pointer, MacroInstr::Auipc(reg.to_owned(), targ_labl.clone(), Part::Upper).into())
            }
            *pointer += 1;
            instr_list.insert(*pointer,
            MacroInstr::Addi(reg.to_owned(), reg.to_owned(), targ_labl.clone(), Part::Lower).into());

            debug!("Expanded '{macro_in}' at {} into '[{}; {}]'", *pointer - 1, instr_list[*pointer-1], instr_list[*pointer]);

            *pointer += 1;
            *accumulator += 1;
        },
        MacroInstr::Call(targ_labl) => {
            instr_list.remove(*pointer);
            match label {
                Some(labl) => instr_list.insert(*pointer,
                                Operation::LablMacro(labl, MacroInstr::Auipc(Reg::G1, targ_labl.clone(), Part::Upper))),
                None => instr_list.insert(*pointer, MacroInstr::Auipc(Reg::G1, targ_labl.clone(), Part::Upper).into())
            }
            *pointer += 1;
            instr_list.insert(*pointer,
            MacroInstr::Jalr(Reg::G1, Reg::G1, targ_labl.clone(), Part::Lower).into());

            debug!("Expanded '{macro_in}' at {} into '[{}; {}]'", *pointer - 1, instr_list[*pointer-1], instr_list[*pointer]);

            *pointer += 1;
            *accumulator += 1;
        },
        MacroInstr::Tail(targ_labl) => {
            instr_list.remove(*pointer);
            match label {
                Some(labl) => instr_list.insert(*pointer,
                                Operation::LablMacro(labl, MacroInstr::Auipc(Reg::G6, targ_labl.clone(), Part::Upper))),
                None => instr_list.insert(*pointer, MacroInstr::Auipc(Reg::G6, targ_labl.clone(), Part::Upper).into())
            }
            *pointer += 1;
            instr_list.insert(*pointer,
            MacroInstr::Jalr(Reg::G0, Reg::G6, targ_labl.clone(), Part::Lower).into());

            debug!("Expanded '{macro_in}' at {} into '[{}; {}]'", *pointer - 1, instr_list[*pointer-1], instr_list[*pointer]);

            *pointer += 1;
            *accumulator += 1;
        },
        MacroInstr::Push(regs) => {
            let (mut right_list, mut mid_list) = split_list(instr_list, pointer);

            match label {
                Some(labl) => mid_list.push(Operation::LablInstr(labl, Instruction::Addi(Reg::G2, Reg::G2, -(regs.len() as i32 * 4)))),
                None => mid_list.push(Instruction::Addi(Reg::G2, Reg::G2, -(regs.len() as i32 * 4)).into())
            }

            let mut acc: i32 = regs.len() as i32 * 4 - 4;

            for reg in regs {
                mid_list.push(Instruction::Sw(reg.to_owned(), Reg::G2, acc).into());
                acc -= 4;
            }

            if log_enabled!(log::Level::Debug) {
                debug_operation_vec(macro_in, pointer, &mid_list);
            }

            *accumulator += (mid_list.len() - 1) as i128;
            *pointer += mid_list.len();
            instr_list.append(&mut mid_list);
            instr_list.append(&mut right_list);
        },
        MacroInstr::Pop(regs) => {
            let (mut right_list, mut mid_list) = split_list(instr_list, pointer);

            let mut acc: i32 = 0;

            match label {
                Some(labl) => mid_list.push(Operation::LablInstr(labl, Instruction::Lw(regs[0].to_owned(), Reg::G2, acc))),
                None => mid_list.push(Instruction::Lw(regs[0].to_owned(), Reg::G2, acc).into())
            }

            for reg in regs {
                if acc == 0 {
                    acc += 4;
                    continue;
                }
                mid_list.push(Instruction::Lw(reg.to_owned(), Reg::G2, acc).into());
                acc += 4;
            }

            mid_list.push(Instruction::Addi(Reg::G2, Reg::G2, regs.len() as i32 * 4).into());

            if log_enabled!(log::Level::Debug) {
                debug_operation_vec(macro_in, pointer, &mid_list);
            }

            *accumulator += (mid_list.len() - 1) as i128;
            *pointer += mid_list.len();
            instr_list.append(&mut mid_list);
            instr_list.append(&mut right_list);
        },
        MacroInstr::RepInstr(num, instrni) => {
            let (mut right_list, mut mid_list) = split_list(instr_list, pointer);

            if mid_list.capacity() < *num as usize {
                mid_list.reserve((*num as usize) - mid_list.capacity());
            }

            match label {
                Some(labl) => mid_list.push(Operation::LablInstr(labl, instrni.clone())),
                None => mid_list.push(instrni.clone().into())
            }

            for _ in 0..(*num - 1) {
                mid_list.push(instrni.clone().into());
            }

            if log_enabled!(log::Level::Debug) {
                debug_operation_vec(macro_in, pointer, &mid_list);
            }

            *accumulator += (*num - 1) as i128;
            *pointer += *num as usize;
            instr_list.append(&mut mid_list);
            instr_list.append(&mut right_list);
        },
        MacroInstr::RepMacro(num, macroni) => {
            let (mut right_list, mut mid_list) = split_list(instr_list, pointer);

            let saved_pointer = *pointer;

            translate_macros(macroni, instr_list, accumulator, pointer, None);

            let instr_num = *pointer - saved_pointer;
            let space_needed = instr_num * (*num as usize);

            if mid_list.capacity() < space_needed {
                mid_list.reserve(space_needed - mid_list.capacity());
            }

            for _ in 0..(*num - 1) {
                for x in 0..instr_num {
                    mid_list.push(instr_list[saved_pointer + x].clone());
                }
            }

            if let Some(labl) = label {
                if let Operation::Instr(instrinin) = &instr_list[saved_pointer] {
                    instr_list[saved_pointer] = Operation::LablInstr(labl, instrinin.clone());
                }
            }

            if log_enabled!(log::Level::Debug) {
                debug_operation_vec(macro_in, pointer, &mid_list);
            }

            *accumulator += (space_needed - 1) as i128;
            *pointer += space_needed;
            instr_list.append(&mut mid_list);
            instr_list.append(&mut right_list);
        },
        _ => *pointer += 1,
    }
}

fn expand_instrs(symbol_map: &mut LabelRecog, instr_list: &mut Vec<Operation>) {
    let mut accumulator: i128 = 0;
    let mut pointer = 0;

    loop {
        let operation = instr_list.get(pointer).cloned();
        match operation {
            Some(opera) => {
                match opera {
                    Operation::Instr(_) => pointer += 1,
                    Operation::Macro(macro_in) => translate_macros(&macro_in, instr_list, &mut accumulator, &mut pointer, None),
                    Operation::LablMacro(labl, macro_in) => {
                        if let Some(label) = symbol_map.get_label(&labl) {
                            label.add_def(accumulator);
                        };
                        translate_macros(&macro_in, instr_list, &mut accumulator, &mut pointer, Some(labl));
                    },
                    Operation::LablInstr(labl, _) |
                    Operation::Labl(labl) => {
                        if let Some(label) = symbol_map.get_label(&labl) {
                            label.add_def(accumulator);
                        }

                        pointer += 1;
                    },
                    Operation::Namespace(_) => unreachable!(),
                }
            },
            None => break,
        };
    }
}

#[derive(Debug, PartialEq)]
struct NoData {}
#[derive(Debug, PartialEq)]
struct LabelDef {
    label: smartstring::alias::String
}
#[derive(Debug, PartialEq)]
struct OperationData {
    op: Operation
}
#[derive(Debug, PartialEq)]
struct LabelOperationData {
    label: smartstring::alias::String, 
    op: Operation
}

trait LineHandle {
    fn as_any(&self) -> &dyn Any;
    fn handle(&mut self,
        instr_counter: &mut usize,
        instr_list: &mut Vec<Operation>,
        subroutines: &mut Option<&mut Subroutines>,
        symbol_map: &mut LabelRecog,
        abs_to_label_queue: &mut BTreeMap<usize, Vec<usize>>
    ) -> Result<(), ParserError>;
}

impl From<(Option<&str>, Option<Operation>)> for Box<dyn LineHandle> {
    fn from(value: (Option<&str>, Option<Operation>)) -> Self {
        match value {
            (Some(labl), Some(op)) => Box::from(LabelOperationData { label: labl.into(), op }),
            (None, Some(op)) => Box::from(OperationData { op }),
            (Some(labl), None) => Box::from(LabelDef { label: labl.into() }),
            (None, None) => Box::from(NoData {}),
        }
    }
}

impl LineHandle for OperationData {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn handle(&mut self,
        instr_counter: &mut usize,
        instr_list: &mut Vec<Operation>,
        subroutines: &mut Option<&mut Subroutines>,
        symbol_map: &mut LabelRecog,
        abs_to_label_queue: &mut BTreeMap<usize, Vec<usize>>
    ) -> Result<(), ParserError> {
        let instr = &mut self.op;

        debug!("({instr_counter}) - Parsed instruction '{instr}'");
        match instr {
            Operation::Macro(macro_in) => handle_label_refs(macro_in, subroutines, symbol_map),
            Operation::Instr(instr_in) => {
                match instr_in {
                    Instruction::Beq(reg1, reg2, imm) => {
                        if let Some(jump_label) = handle_abs_addr_label_conv(*instr_counter, abs_to_label_queue,
                                instr_list, symbol_map, imm) {
                            *instr = Operation::Macro(MacroInstr::Beq(*reg1, *reg2, jump_label));
                        }
                    },
                    Instruction::Bne(reg1, reg2, imm) => {
                        if let Some(jump_label) = handle_abs_addr_label_conv(*instr_counter, abs_to_label_queue,
                                instr_list, symbol_map, imm) {
                            *instr = Operation::Macro(MacroInstr::Bne(*reg1, *reg2, jump_label));
                        }
                    },
                    Instruction::Blt(reg1, reg2, imm) => {
                        if let Some(jump_label) = handle_abs_addr_label_conv(*instr_counter, abs_to_label_queue,
                                instr_list, symbol_map, imm) {
                            *instr = Operation::Macro(MacroInstr::Blt(*reg1, *reg2, jump_label));
                        }
                    },
                    Instruction::Bltu(reg1, reg2, imm) => {
                        if let Some(jump_label) = handle_abs_addr_label_conv(*instr_counter, abs_to_label_queue,
                                instr_list, symbol_map, imm) {
                            *instr = Operation::Macro(MacroInstr::Bltu(*reg1, *reg2, jump_label));
                        }
                    },
                    Instruction::Bge(reg1, reg2, imm) => {
                        if let Some(jump_label) = handle_abs_addr_label_conv(*instr_counter, abs_to_label_queue,
                                instr_list, symbol_map, imm) {
                            *instr = Operation::Macro(MacroInstr::Bge(*reg1, *reg2, jump_label));
                        }
                    },
                    Instruction::Bgeu(reg1, reg2, imm) => {
                        if let Some(jump_label) = handle_abs_addr_label_conv(*instr_counter, abs_to_label_queue,
                                instr_list, symbol_map, imm) {
                            *instr = Operation::Macro(MacroInstr::Bgeu(*reg1, *reg2, jump_label));
                        }
                    },
                    Instruction::Jal(reg, imm) => {
                        if let Some(jump_label) = handle_abs_addr_label_conv(*instr_counter, abs_to_label_queue,
                                instr_list, symbol_map, imm) {
                            *instr = Operation::Macro(MacroInstr::Jal(*reg, jump_label));
                        }
                    },
                    Instruction::Jalr(reg1, reg2, imm) => {
                        if let Some(jump_label) = handle_abs_addr_label_conv(*instr_counter, abs_to_label_queue,
                                instr_list, symbol_map, imm) {
                            *instr = Operation::Macro(MacroInstr::Jalr(*reg1, *reg2, jump_label, Part::None));
                        }
                    },
                    _ => (),
                };
            }
            _ => (),
        }

        if let Some(list) = abs_to_label_queue.remove(&(*instr_counter + 1)) {
            let jump_label = smartstring::alias::String::from("__") + instr_counter.to_string().as_str();
            symbol_map.crt_def_ref(&jump_label, false, LabelType::Address, *instr_counter as i128);
            handle_instr_substitution(instr_list, &list, &jump_label);
            match &instr {
                Operation::Instr(instr_in) => *instr = Operation::LablInstr(jump_label, instr_in.to_owned()),
                Operation::Macro(macro_in) => *instr = Operation::LablMacro(jump_label, macro_in.to_owned()),
                _ => unreachable!()
            }
        };

        *instr_counter += 1;
        instr_list.push(instr.to_owned());
        Ok(())
    }
}

impl LineHandle for LabelDef {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn handle(&mut self,
        instr_counter: &mut usize,
        instr_list: &mut Vec<Operation>,
        _subroutines: &mut Option<&mut Subroutines>,
        symbol_map: &mut LabelRecog,
        abs_to_label_queue: &mut BTreeMap<usize, Vec<usize>>
    ) -> Result<(), ParserError> {
        let label = &self.label;

        debug!("({instr_counter}) - Parsed label '{label}'");
        handle_label_defs(label, symbol_map, LabelType::Address, *instr_counter)?;
        if let Some(list) = abs_to_label_queue.remove(&(*instr_counter + 1)) {
            symbol_map.set_refd_label(label);
            handle_instr_substitution(instr_list, &list, label)
        };

        instr_list.push(Operation::Labl(label.clone()));
        Ok(())
    }
}

impl LineHandle for LabelOperationData {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn handle(&mut self, 
        instr_counter: &mut usize,
        instr_list: &mut Vec<Operation>,
        subroutines: &mut Option<&mut Subroutines>,
        symbol_map: &mut LabelRecog,
        abs_to_label_queue: &mut BTreeMap<usize, Vec<usize>>
    ) -> Result<(), ParserError> {
        let label = &self.label;
        let instr = &mut self.op;
        
        debug!("({instr_counter}) - Parsed label '{label}' and instruction '{instr}'");
        handle_label_defs(label, symbol_map, LabelType::Address, *instr_counter)?;

        match instr {
            Operation::Macro(macro_in) => {
                handle_label_refs(macro_in, subroutines, symbol_map);
                *instr = Operation::LablMacro(label.clone(), macro_in.to_owned());
            },
            Operation::Instr(instr_in) => {
                match instr_in {
                    Instruction::Beq(reg1, reg2, imm) => {
                        if let Some(jump_label) = handle_abs_addr_label_conv(*instr_counter, abs_to_label_queue, 
                            instr_list, symbol_map, imm) {
                            *instr = Operation::LablMacro(label.clone(),
                            MacroInstr::Beq(*reg1, *reg2, jump_label));
                        }
                    },
                    Instruction::Bne(reg1, reg2, imm) => {
                        if let Some(jump_label) = handle_abs_addr_label_conv(*instr_counter, 
                            abs_to_label_queue, instr_list, symbol_map, imm) {
                            *instr = Operation::LablMacro(label.clone(),
                            MacroInstr::Bne(*reg1, *reg2, jump_label));
                        }
                    },
                    Instruction::Blt(reg1, reg2, imm) => {
                        if let Some(jump_label) = handle_abs_addr_label_conv(*instr_counter, abs_to_label_queue, 
                            instr_list, symbol_map, imm) {
                            *instr = Operation::LablMacro(label.clone(),
                            MacroInstr::Blt(*reg1, *reg2, jump_label));
                        }
                    },
                    Instruction::Bltu(reg1, reg2, imm) => {
                        if let Some(jump_label) = handle_abs_addr_label_conv(*instr_counter, abs_to_label_queue, 
                            instr_list, symbol_map, imm) {
                            *instr = Operation::LablMacro(label.clone(),
                            MacroInstr::Bltu(*reg1, *reg2, jump_label));
                        }
                    },
                    Instruction::Bge(reg1, reg2, imm) => {
                        if let Some(jump_label) = handle_abs_addr_label_conv(*instr_counter, abs_to_label_queue, 
                            instr_list, symbol_map, imm) {
                            *instr = Operation::LablMacro(label.clone(),
                            MacroInstr::Bge(*reg1, *reg2, jump_label));
                        }
                    },
                    Instruction::Bgeu(reg1, reg2, imm) => {
                        if let Some(jump_label) = handle_abs_addr_label_conv(*instr_counter, abs_to_label_queue, 
                            instr_list, symbol_map, imm) {
                            *instr = Operation::LablMacro(label.clone(),
                            MacroInstr::Bgeu(*reg1, *reg2, jump_label));
                        }
                    },
                    Instruction::Jal(reg, imm) => {
                        if let Some(jump_label) = handle_abs_addr_label_conv(*instr_counter, abs_to_label_queue, 
                            instr_list, symbol_map, imm) {
                            *instr = Operation::LablMacro(label.clone(),
                            MacroInstr::Jal(*reg, jump_label));
                        }
                    },
                    Instruction::Jalr(reg1, reg2, imm) => {
                        if let Some(jump_label) = handle_abs_addr_label_conv(*instr_counter, abs_to_label_queue, 
                            instr_list, symbol_map, imm) {
                            *instr = Operation::LablMacro(label.clone(),
                            MacroInstr::Jalr(*reg1, *reg2, jump_label, Part::None));
                        }
                    },
                    _ => *instr = Operation::LablInstr(label.clone(), instr_in.to_owned()),
                };
            }
            _ => (),
        }

        if let Some(list) = abs_to_label_queue.remove(&(*instr_counter + 1)) {
            symbol_map.set_refd_label(&(label.clone()));
            handle_instr_substitution(instr_list, &list, label);
        };

        *instr_counter += 1;
        instr_list.push(instr.to_owned());
        Ok(())
    }
}

impl LineHandle for NoData {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn handle(&mut self,
        instr_counter: &mut usize,
        _instr_list: &mut Vec<Operation>,
        _subroutines: &mut Option<&mut Subroutines>,
        _symbol_map: &mut LabelRecog,
        _abs_to_label_queue: &mut BTreeMap<usize, Vec<usize>>
    ) -> Result<(), ParserError> {
        debug!("({instr_counter}) - Parsed nothing!");
        Ok(())
    }
}

pub fn parse<'a>(input: &'a str, subroutines: &mut Option<&mut Subroutines>, symbol_map: &mut LabelRecog, sp_init: bool) -> IResult<&'a str, Vec<Operation>> {
    let mut instr_list: Vec<Operation> = vec![];

    // Key = line forward; value = current line
    let mut abs_to_label_queue: BTreeMap<usize, Vec<usize>> = BTreeMap::new();

    let mut rest = input;
    let mut instr_counter: usize = 0;

    if sp_init {
        instr_list.push(Instruction::Lui(Reg::G2, 4096).into());
        instr_counter += 1;
    }

    let privileged = subroutines.is_none();

    loop {
        let (rest_line, mut parsed) = match privileged {
            true => parse_line_priv(rest)?,
            false => parse_line(rest)?,
        };
        rest = rest_line;

        if let Err(e) = parsed.handle(&mut instr_counter, &mut instr_list, subroutines, symbol_map, &mut abs_to_label_queue) {
            error!("{e}");
            return context(e.get_nom_err_text(), fail)(rest)
        }

        if rest.trim().is_empty() {
            break;
        }
    }

    if !abs_to_label_queue.is_empty() {
        let jump_label = match &instr_list[instr_counter - 1] {
            Operation::Labl(labl) => {
                symbol_map.set_refd_label(labl);
                labl.clone()
            },
            _ => {
                let res = smartstring::alias::String::from("__") + instr_counter.to_string().as_str();
                symbol_map.crt_def_ref(&res, false, LabelType::Address, instr_counter as i128);
                res
            }
        };
        for (_, elem) in abs_to_label_queue.iter() {
            handle_instr_substitution(&mut instr_list, elem, &jump_label);
        }
        instr_list.push(Operation::Labl(jump_label));
    }

    expand_instrs(symbol_map, &mut instr_list);

    Ok(("", instr_list))
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! assert_equ_line {
        ($pars_line:literal, $rest:literal, $struct:ident { $( $field:ident: $val:expr ),*}) => {
            let (rest, parsed) = parse_line($pars_line)?;
            assert_eq!(rest, $rest);
            assert_eq!(parsed.as_any().downcast_ref::<$struct>().unwrap(), &$struct { $( $field: $val ),* });
        };
    }

    #[test]
    fn test_parse_line() -> Result<(), Box<dyn std::error::Error>> {
        assert_equ_line!("label: add x1, x5, x6", "",
                         LabelOperationData { label: "label".into(), op: Instruction::Add(Reg::G1, Reg::G5, Reg::G6).into() });
        assert_equ_line!("\ntest:\n\nsub x6, x5, x11", "",
                         LabelOperationData { label: "test".into(), op: Instruction::Sub(Reg::G6, Reg::G5, Reg::G11).into() });
        assert_equ_line!("\n\n\nreturn:\n", "\n", LabelDef { label: "return".into() });
        assert_equ_line!("mv x15, x12\naddi x12, x10, 0x05", "\naddi x12, x10, 0x05",
                         OperationData { op: Instruction::Addi(Reg::G15, Reg::G12, 0).into() });
        assert_equ_line!("label:\ndiv x14, x13, x10", "",
                         LabelOperationData { label: "label".into(), op: Instruction::Div(Reg::G14, Reg::G13, Reg::G10).into() });
        Ok(())
    }

    macro_rules! assert_equ_line_priv {
        ($pars_line:literal, $rest:literal, $struct:ident { $( $field:ident: $val:expr ),*}) => {
            let (rest, parsed) = parse_line_priv($pars_line)?;
            assert_eq!(rest, $rest);
            assert_eq!(parsed.as_any().downcast_ref::<$struct>().unwrap(), &$struct { $( $field: $val ),* });
        };
    }

    #[test]
    fn test_parse_line_privileged() -> Result<(), Box<dyn std::error::Error>> {
        assert_equ_line_priv!("_label: add x1, x5, x6", "",
                         LabelOperationData { label: "_label".into(), op: Instruction::Add(Reg::G1, Reg::G5, Reg::G6).into() });
        assert_equ_line_priv!("\n_test:\n\nsub x6, x5, x11", "",
                         LabelOperationData { label: "_test".into(), op: Instruction::Sub(Reg::G6, Reg::G5, Reg::G11).into() });
        assert_equ_line_priv!("\n\n\n_return:\n", "\n", LabelDef { label: "_return".into() });
        assert_equ_line_priv!("mv x15, x12\naddi x12, x10, 0x05", "\naddi x12, x10, 0x05",
                         OperationData { op: Instruction::Addi(Reg::G15, Reg::G12, 0).into() });
        assert_equ_line_priv!("_label:\ndiv x14, x13, x10", "",
                         LabelOperationData { label: "_label".into(), op: Instruction::Div(Reg::G14, Reg::G13, Reg::G10).into() });
        Ok(())
    }

    #[test]
    fn test_parse() {
        let source_code = r#"START:
    li x4, 16
    mv x3, x4
MUL: beq x3, x4, END
    mul x6, x4, x3
    lui x4, 0x16
    j MUL
END:
"#;

        let mut symbol_map = LabelRecog::new();

        let mut subroutines = Subroutines::new();

        let mut symbols = LabelRecog::new();
        let mut label = LabelElem::new();
        label.set_name("START".into());
        label.set_type(LabelType::Address);
        label.set_scope(true);
        let _ = symbols.insert_label(label);

        label = LabelElem::new_refd("MUL".into());
        label.set_type(LabelType::Address);
        label.set_scope(true);
        label.set_def(2);
        let _ = symbols.insert_label(label);

        label = LabelElem::new_refd("END".into());
        label.set_type(LabelType::Address);
        label.set_scope(true);
        label.set_def(6);
        //label.set_def(9);
        let _ = symbols.insert_label(label);

        let correct_vec: Vec<Operation> = vec![
                                                //Operation::LablInstr(Cow::from("START"), Instruction::Lui(Reg::G4, 16)),
                                                //Operation::from(Instruction::Addi(Reg::G4, Reg::G4, 16)),
                                                Operation::LablInstr("START".into(), Instruction::Addi(Reg::G4, Reg::G0, 16)),
                                                Operation::from(Instruction::Addi(Reg::G3, Reg::G4, 0)),
                                                Operation::LablMacro("MUL".into(), MacroInstr::Beq(Reg::G3, Reg::G4, "END".into())),
                                                Operation::from(Instruction::Mul(Reg::G6, Reg::G4, Reg::G3)),
                                                Operation::from(Instruction::Lui(Reg::G4, 90112)),
                                                Operation::from(MacroInstr::Jal(Reg::G0, "MUL".into())),
                                                Operation::Labl("END".into())
                                                ];

        assert_eq!(parse(source_code, &mut Some(&mut subroutines), &mut symbol_map, false),
                   Ok(("", correct_vec)));
        assert_eq!(symbol_map, symbols);
        assert!(subroutines.get_code().is_empty())
        // TODO: Probably more test cases!
    }

    #[test]
    fn test_parse_abs_addresses_smallex() {
        let source_code = 
r#" li  x4, 16
    mv  x3, x4
    beq x3, x4, 16
    mul x6, x4, x3
    beq x3, x4, 16
    lui x4, 0x16
    jal x0, -12
"#;
        let mut subroutines = Subroutines::new();

        let mut symbol_map = LabelRecog::new();
        let mut symbols = LabelRecog::new();

        /*let mut label = LabelElem::new_refd("_MUL".to_string());
        label.set_scope(true);
        let _ = symbols.insert_label(label);*/

        let mut label = LabelElem::new_refd("__3".into());
        label.set_type(LabelType::Address);
        label.set_scope(false);
        label.set_def(3);
        let _ = symbols.insert_label(label);

        label = LabelElem::new_refd("__6".into());
        label.set_type(LabelType::Address);
        label.set_scope(false);
        label.set_def(6);
        //label.set_def(9);
        let _ = symbols.insert_label(label);

        label = LabelElem::new_refd("__7".into());
        label.set_type(LabelType::Address);
        label.set_scope(false);
        label.set_def(7);
        //label.set_def(10);
        let _ = symbols.insert_label(label);
        
        let correct_vec: Vec<Operation> = vec![
                                                //Operation::Instr(Instruction::Lui(Reg::G4, 16)),
                                                //Operation::from(Instruction::Addi(Reg::G4, Reg::G4, 16)),
                                                Instruction::Addi(Reg::G4, Reg::G0, 16).into(),
                                                Operation::from(Instruction::Addi(Reg::G3, Reg::G4, 0)),
                                                Operation::Macro(MacroInstr::Beq(Reg::G3, Reg::G4, "__6".into())),
                                                Operation::LablInstr("__3".into(), Instruction::Mul(Reg::G6, Reg::G4, Reg::G3)),
                                                Operation::Macro(MacroInstr::Beq(Reg::G3, Reg::G4, "__7".into())),
                                                Operation::from(Instruction::Lui(Reg::G4, 90112)),
                                                Operation::LablMacro("__6".into(), MacroInstr::Jal(Reg::G0, "__3".into())),
                                                Operation::Labl("__7".into())
                                                ];

        assert_eq!(parse(source_code, &mut Some(&mut subroutines), &mut symbol_map, false),
                   Ok(("", correct_vec)));
        assert_eq!(symbol_map, symbols);
        assert_eq!(subroutines.get_code().is_empty(), true);
    }

    #[test]
    fn test_parse_abs_addresses_biggerex() {
        let source_code = 
r#"
    addi a7, zero, 1
    mv a2, a0
    mv a3, a1
    mv a0, zero
    mv a1, zero
    bne a2, a3, 16
    slli a3, a3, 1
    sub a2, a2, a3
    add a0, a0, a7
    blt a2, a3, 40
    slli a3, a3, 1
    slli a7, a7, 1
    blt a3, a2, -8
    srli a3, a3, 1
    srli a7, a7, 1
    sub a2, a2, a3
    add a0, a0, a7
    bne a2, zero, -32
    beq zero, zero, 44
    srli a3, a3, 1
    srli a7, a7, 1
    blt a2, a3, -8
    slli a3, a3, 1
    slli a7, a7, 1
    beq a2, a3, 8
    srli a3, a3, 1
TEST: srli a7, a7, 1
    sub a2, a2, a3
    add a0, a0, a7
    bne a2, zero, -80
    ret
"#;
        let mut subroutines = Subroutines::new();

        let mut symbol_map = LabelRecog::new();
        let mut symbols = LabelRecog::new();

        let mut label = LabelElem::new_refd("__9".into());
        label.set_type(LabelType::Address);
        label.set_scope(false);
        label.set_def(9);
        let _ = symbols.insert_label(label);

        label = LabelElem::new_refd("__10".into());
        label.set_type(LabelType::Address);
        label.set_scope(false);
        label.set_def(10);
        let _ = symbols.insert_label(label);

        label = LabelElem::new_refd("__19".into());
        label.set_type(LabelType::Address);
        label.set_scope(false);
        label.set_def(19);
        let _ = symbols.insert_label(label);


        label = LabelElem::new_refd("TEST".into());
        label.set_type(LabelType::Address);
        label.set_scope(true);
        label.set_def(26);
        let _ = symbols.insert_label(label);

        label = LabelElem::new_refd("__29".into());
        label.set_type(LabelType::Address);
        label.set_scope(false);
        label.set_def(29);
        let _ = symbols.insert_label(label);

        let mut correct_vec: Vec<Operation> = vec![];
        correct_vec.push(Operation::Instr(Instruction::Addi(Reg::G17, Reg::G0, 1)));
        correct_vec.push(Operation::Instr(Instruction::Addi(Reg::G12, Reg::G10, 0)));
        correct_vec.push(Operation::Instr(Instruction::Addi(Reg::G13, Reg::G11, 0)));
        correct_vec.push(Operation::Instr(Instruction::Addi(Reg::G10, Reg::G0, 0)));
        correct_vec.push(Operation::Instr(Instruction::Addi(Reg::G11, Reg::G0, 0)));

        correct_vec.push(Operation::Macro(MacroInstr::Bne(Reg::G12, Reg::G13, "__9".into())));
        correct_vec.push(Operation::Instr(Instruction::Slli(Reg::G13, Reg::G13, 1)));
        correct_vec.push(Operation::Instr(Instruction::Sub(Reg::G12, Reg::G12, Reg::G13)));
        correct_vec.push(Operation::Instr(Instruction::Add(Reg::G10, Reg::G10, Reg::G17)));
        correct_vec.push(Operation::LablMacro("__9".into(), MacroInstr::Blt(Reg::G12, Reg::G13, "__19".into())));

        correct_vec.push(Operation::LablInstr("__10".into(), Instruction::Slli(Reg::G13, Reg::G13, 1)));
        correct_vec.push(Operation::Instr(Instruction::Slli(Reg::G17, Reg::G17, 1)));
        correct_vec.push(Operation::Macro(MacroInstr::Blt(Reg::G13, Reg::G12, "__10".into())));
        correct_vec.push(Operation::Instr(Instruction::Srli(Reg::G13, Reg::G13, 1)));
        correct_vec.push(Operation::Instr(Instruction::Srli(Reg::G17, Reg::G17, 1)));
        correct_vec.push(Operation::Instr(Instruction::Sub(Reg::G12, Reg::G12, Reg::G13)));
        correct_vec.push(Operation::Instr(Instruction::Add(Reg::G10, Reg::G10, Reg::G17)));
        correct_vec.push(Operation::Macro(MacroInstr::Bne(Reg::G12, Reg::G0, "__9".into())));
        correct_vec.push(Operation::Macro(MacroInstr::Beq(Reg::G0, Reg::G0, "__29".into())));

        correct_vec.push(Operation::LablInstr("__19".into(), Instruction::Srli(Reg::G13, Reg::G13, 1)));
        correct_vec.push(Operation::Instr(Instruction::Srli(Reg::G17, Reg::G17, 1)));
        correct_vec.push(Operation::Macro(MacroInstr::Blt(Reg::G12, Reg::G13, "__19".into())));
        correct_vec.push(Operation::Instr(Instruction::Slli(Reg::G13, Reg::G13, 1)));
        correct_vec.push(Operation::Instr(Instruction::Slli(Reg::G17, Reg::G17, 1)));
        correct_vec.push(Operation::Macro(MacroInstr::Beq(Reg::G12, Reg::G13, "TEST".into())));
        correct_vec.push(Operation::Instr(Instruction::Srli(Reg::G13, Reg::G13, 1)));
        correct_vec.push(Operation::LablInstr("TEST".into(), Instruction::Srli(Reg::G17, Reg::G17, 1)));
        correct_vec.push(Operation::Instr(Instruction::Sub(Reg::G12, Reg::G12, Reg::G13)));
        correct_vec.push(Operation::Instr(Instruction::Add(Reg::G10, Reg::G10, Reg::G17)));
        correct_vec.push(Operation::LablMacro("__29".into(), MacroInstr::Bne(Reg::G12, Reg::G0, "__9".into())));
        correct_vec.push(Operation::Instr(Instruction::Jalr(Reg::G0, Reg::G1, 0)));

        assert_eq!(parse(source_code, &mut Some(&mut subroutines), &mut symbol_map, false),
                   Ok(("", correct_vec)));
        assert_eq!(symbol_map, symbols);
        assert!(subroutines.get_code().is_empty());
    }

    fn reset_acc_pointer(accumulator: &mut i128, pointer: &mut usize) {
        *accumulator = 0;
        *pointer = 2;
    }

    #[test]
    fn test_translate_macros() {
        let sample_list: Vec<Operation> = Vec::from([
            Operation::Instr(Instruction::Addi(Reg::G17, Reg::G0, 1)),
            Operation::Instr(Instruction::Addi(Reg::G12, Reg::G10, 0)),
            Operation::Instr(Instruction::Addi(Reg::G13, Reg::G11, 0)),
            Operation::Instr(Instruction::Addi(Reg::G10, Reg::G0, 0)),
            Operation::Instr(Instruction::Addi(Reg::G11, Reg::G0, 0))
        ]);

        let mut accumulator: i128 = 0;
        let mut pointer: usize = 2;

        {
            let mut test_list = sample_list.clone();
            test_list.insert(2, 
                Operation::Macro(MacroInstr::Srr(Reg::G7, Reg::G20, 2)));
            let label: smartstring::alias::String = "TEST3".into();
            
            translate_macros(&MacroInstr::Srr(Reg::G7, Reg::G20, 2), 
            &mut test_list, 
            &mut accumulator, 
            &mut pointer, 
            Some(label.clone()));

            assert_eq!(accumulator, 3);
            assert_eq!(pointer, 6);

            let cor_vec: Vec<Operation> = Vec::from([
                Operation::Instr(Instruction::Addi(Reg::G17, Reg::G0, 1)),
                Operation::Instr(Instruction::Addi(Reg::G12, Reg::G10, 0)),
                Operation::LablInstr(label, Instruction::Addi(Reg::G10, Reg::G20, 0)),
                Operation::Instr(Instruction::Addi(Reg::G11, Reg::G0, 2)),
                Operation::Macro(MacroInstr::Jal(Reg::G1, "_SRR".into())),
                Operation::Instr(Instruction::Addi(Reg::G7, Reg::G10, 0)),
                Operation::Instr(Instruction::Addi(Reg::G13, Reg::G11, 0)),
                Operation::Instr(Instruction::Addi(Reg::G10, Reg::G0, 0)),
                Operation::Instr(Instruction::Addi(Reg::G11, Reg::G0, 0))
            ]);

            assert_eq!(test_list, cor_vec);
        }

        reset_acc_pointer(&mut accumulator, &mut pointer);

        {
            let mut test_list = sample_list.clone();
            test_list.insert(2, 
                Operation::Macro(MacroInstr::Slr(Reg::G7, Reg::G20, 2)));
            let label: smartstring::alias::String = "TEST66".into();
            
            translate_macros(&MacroInstr::Slr(Reg::G7, Reg::G20, 2), 
            &mut test_list, 
            &mut accumulator, 
            &mut pointer, 
            Some(label.clone()));

            assert_eq!(accumulator, 3);
            assert_eq!(pointer, 6);

            let cor_vec: Vec<Operation> = Vec::from([
                Operation::Instr(Instruction::Addi(Reg::G17, Reg::G0, 1)),
                Operation::Instr(Instruction::Addi(Reg::G12, Reg::G10, 0)),
                Operation::LablInstr(label, Instruction::Addi(Reg::G10, Reg::G20, 0)),
                Operation::Instr(Instruction::Addi(Reg::G11, Reg::G0, 2)),
                Operation::Macro(MacroInstr::Jal(Reg::G1, "_SLR".into())),
                Operation::Instr(Instruction::Addi(Reg::G7, Reg::G10, 0)),
                Operation::Instr(Instruction::Addi(Reg::G13, Reg::G11, 0)),
                Operation::Instr(Instruction::Addi(Reg::G10, Reg::G0, 0)),
                Operation::Instr(Instruction::Addi(Reg::G11, Reg::G0, 0))
            ]);

            assert_eq!(test_list, cor_vec);
        }

        reset_acc_pointer(&mut accumulator, &mut pointer);

        {
            let mut test_list = sample_list.clone();
            let target_label = "0800 444 555 666 - JETZT ANRUFEN UND [INSERT PRODUCT] ZUM PREIS VON EINEM BEKOMMEN! LIMITIERTES ANGEBOT";
            test_list.insert(2, 
                Operation::Macro(MacroInstr::La(Reg::G7, 
                    target_label.into())));
            let label: smartstring::alias::String = "TEST80".into();
            
            translate_macros(&MacroInstr::La(Reg::G7, 
                target_label.into()),
            &mut test_list, 
            &mut accumulator, 
            &mut pointer, 
            Some(label.clone()));

            assert_eq!(accumulator, 1);
            assert_eq!(pointer, 4);

            let cor_vec: Vec<Operation> = Vec::from([
                Operation::Instr(Instruction::Addi(Reg::G17, Reg::G0, 1)),
                Operation::Instr(Instruction::Addi(Reg::G12, Reg::G10, 0)),
                Operation::LablMacro(label, MacroInstr::Auipc(Reg::G7, target_label.into(), Part::Upper)),
                Operation::Macro(MacroInstr::Addi(Reg::G7, Reg::G7, target_label.into(), Part::Lower)),
                Operation::Instr(Instruction::Addi(Reg::G13, Reg::G11, 0)),
                Operation::Instr(Instruction::Addi(Reg::G10, Reg::G0, 0)),
                Operation::Instr(Instruction::Addi(Reg::G11, Reg::G0, 0))
            ]);

            assert_eq!(test_list, cor_vec);
        }

        reset_acc_pointer(&mut accumulator, &mut pointer);

        {
            let mut test_list = sample_list.clone();
            let target_label = "0800 444 555 666";
            test_list.insert(2, 
                Operation::Macro(MacroInstr::Call(target_label.into())));
            let label: smartstring::alias::String = "TEST80".into();
            
            translate_macros(&MacroInstr::Call(target_label.into()),
            &mut test_list, 
            &mut accumulator, 
            &mut pointer, 
            Some(label.clone()));

            assert_eq!(accumulator, 1);
            assert_eq!(pointer, 4);

            let cor_vec: Vec<Operation> = Vec::from([
                Operation::Instr(Instruction::Addi(Reg::G17, Reg::G0, 1)),
                Operation::Instr(Instruction::Addi(Reg::G12, Reg::G10, 0)),
                Operation::LablMacro(label, MacroInstr::Auipc(Reg::G1, target_label.into(), Part::Upper)),
                Operation::Macro(MacroInstr::Jalr(Reg::G1, Reg::G1, target_label.into(), Part::Lower)),
                Operation::Instr(Instruction::Addi(Reg::G13, Reg::G11, 0)),
                Operation::Instr(Instruction::Addi(Reg::G10, Reg::G0, 0)),
                Operation::Instr(Instruction::Addi(Reg::G11, Reg::G0, 0))
            ]);

            assert_eq!(test_list, cor_vec);
        }

        reset_acc_pointer(&mut accumulator, &mut pointer);

        {
            let mut test_list = sample_list.clone();
            let target_label = "0800 444 555 666";
            test_list.insert(2, 
                Operation::Macro(MacroInstr::Tail(target_label.into())));
            let label: smartstring::alias::String = "TEST80".into();
            
            translate_macros(&MacroInstr::Tail(target_label.into()),
            &mut test_list, 
            &mut accumulator, 
            &mut pointer, 
            Some(label.clone()));

            assert_eq!(accumulator, 1);
            assert_eq!(pointer, 4);

            let cor_vec: Vec<Operation> = Vec::from([
                Operation::Instr(Instruction::Addi(Reg::G17, Reg::G0, 1)),
                Operation::Instr(Instruction::Addi(Reg::G12, Reg::G10, 0)),
                Operation::LablMacro(label, MacroInstr::Auipc(Reg::G6, target_label.into(), Part::Upper)),
                Operation::Macro(MacroInstr::Jalr(Reg::G0, Reg::G6, target_label.into(), Part::Lower)),
                Operation::Instr(Instruction::Addi(Reg::G13, Reg::G11, 0)),
                Operation::Instr(Instruction::Addi(Reg::G10, Reg::G0, 0)),
                Operation::Instr(Instruction::Addi(Reg::G11, Reg::G0, 0))
            ]);

            assert_eq!(test_list, cor_vec);
        }

        reset_acc_pointer(&mut accumulator, &mut pointer);

        {
            let mut test_list = sample_list.clone();
            test_list.insert(2, 
                Operation::Macro(MacroInstr::Push(Vec::from([Reg::G15, Reg::G16, Reg::G17]))));
            let label: smartstring::alias::String = "TEST80".into();
            
            translate_macros(&MacroInstr::Push(Vec::from([Reg::G15, Reg::G16, Reg::G17])), 
            &mut test_list, 
            &mut accumulator, 
            &mut pointer, 
            Some(label.clone()));

            assert_eq!(accumulator, 3);
            assert_eq!(pointer, 6);

            let cor_vec: Vec<Operation> = Vec::from([
                Operation::Instr(Instruction::Addi(Reg::G17, Reg::G0, 1)),
                Operation::Instr(Instruction::Addi(Reg::G12, Reg::G10, 0)),
                Operation::LablInstr(label, Instruction::Addi(Reg::G2, Reg::G2, -12)),
                Operation::Instr(Instruction::Sw(Reg::G15, Reg::G2, 8)),
                Operation::Instr(Instruction::Sw(Reg::G16, Reg::G2, 4)),
                Operation::Instr(Instruction::Sw(Reg::G17, Reg::G2, 0)),
                Operation::Instr(Instruction::Addi(Reg::G13, Reg::G11, 0)),
                Operation::Instr(Instruction::Addi(Reg::G10, Reg::G0, 0)),
                Operation::Instr(Instruction::Addi(Reg::G11, Reg::G0, 0))
            ]);

            assert_eq!(test_list, cor_vec);
        }

        reset_acc_pointer(&mut accumulator, &mut pointer);

        {
            let mut test_list = sample_list.clone();
            test_list.insert(2, 
                Operation::Macro(MacroInstr::Push(Vec::from([Reg::G15, Reg::G16, Reg::G17]))));
            let label: smartstring::alias::String = "TEST80".into();
            
            translate_macros(&MacroInstr::Push(Vec::from([Reg::G15, Reg::G16, Reg::G17])), 
            &mut test_list, 
            &mut accumulator, 
            &mut pointer, 
            Some(label.clone()));

            assert_eq!(accumulator, 3);
            assert_eq!(pointer, 6);

            let cor_vec: Vec<Operation> = Vec::from([
                Operation::Instr(Instruction::Addi(Reg::G17, Reg::G0, 1)),
                Operation::Instr(Instruction::Addi(Reg::G12, Reg::G10, 0)),
                Operation::LablInstr(label, Instruction::Addi(Reg::G2, Reg::G2, -12)),
                Operation::Instr(Instruction::Sw(Reg::G15, Reg::G2, 8)),
                Operation::Instr(Instruction::Sw(Reg::G16, Reg::G2, 4)),
                Operation::Instr(Instruction::Sw(Reg::G17, Reg::G2, 0)),
                Operation::Instr(Instruction::Addi(Reg::G13, Reg::G11, 0)),
                Operation::Instr(Instruction::Addi(Reg::G10, Reg::G0, 0)),
                Operation::Instr(Instruction::Addi(Reg::G11, Reg::G0, 0))
            ]);

            assert_eq!(test_list, cor_vec);
        }

        reset_acc_pointer(&mut accumulator, &mut pointer);

        {
            let mut test_list = sample_list.clone();
            test_list.insert(2, 
                Operation::Macro(MacroInstr::Pop(Vec::from([Reg::G15, Reg::G16, Reg::G17]))));
            let label: smartstring::alias::String = "TEST80".into();
            
            translate_macros(&MacroInstr::Pop(Vec::from([Reg::G15, Reg::G16, Reg::G17])), 
            &mut test_list, 
            &mut accumulator, 
            &mut pointer, 
            Some(label.clone()));

            assert_eq!(accumulator, 3);
            assert_eq!(pointer, 6);

            let cor_vec: Vec<Operation> = Vec::from([
                Operation::Instr(Instruction::Addi(Reg::G17, Reg::G0, 1)),
                Operation::Instr(Instruction::Addi(Reg::G12, Reg::G10, 0)),
                Operation::LablInstr(label, Instruction::Lw(Reg::G15, Reg::G2, 0)),
                Operation::Instr(Instruction::Lw(Reg::G16, Reg::G2, 4)),
                Operation::Instr(Instruction::Lw(Reg::G17, Reg::G2, 8)),
                Operation::Instr(Instruction::Addi(Reg::G2, Reg::G2, 12)),
                Operation::Instr(Instruction::Addi(Reg::G13, Reg::G11, 0)),
                Operation::Instr(Instruction::Addi(Reg::G10, Reg::G0, 0)),
                Operation::Instr(Instruction::Addi(Reg::G11, Reg::G0, 0))
            ]);

            assert_eq!(test_list, cor_vec);
        }
    }

    #[test]
    fn test_parse_comment() {
        let source_code = r#"
    ; stop this hellO
    ; MULTIPLE COMMENTS
    ; YES, MULTIPLE COMMENTS
    ; CAUSE I LIKE IT
    ; COMMENT, LIKE AND SUBSCRIBE
    ; OR THIS CODE WILL HAUNT YOU IN YOUR DREAMS
START:                  ; TEST
                        ; HANS?
                        ; lit testing omg
    li x4, 16
    mv x3, x4
MUL: beq x3, x4, END    ; mul dead
    mul x6, x4, x3
    lui x4, 0x16
    j MUL               ; YOU BETTER JUMP BACK
END:                    ; TEST
; TEST2
; better worth it
"#;

        let mut subroutines = Subroutines::new();

        let mut symbol_map = LabelRecog::new();
        let mut symbols = LabelRecog::new();
        let mut label = LabelElem::new();
        label.set_name("START".into());
        label.set_type(LabelType::Address);
        label.set_scope(true);
        label.set_def(0);
        let _ = symbols.insert_label(label);

        label = LabelElem::new_refd("MUL".into());
        label.set_type(LabelType::Address);
        label.set_scope(true);
        label.set_def(2);
        let _ = symbols.insert_label(label);

        label = LabelElem::new_refd("END".into());
        label.set_type(LabelType::Address);
        label.set_scope(true);
        label.set_def(6);
        //label.set_def(9);
        let _ = symbols.insert_label(label);

        let correct_vec: Vec<Operation> = vec![
                                                Operation::LablInstr("START".into(), Instruction::Addi(Reg::G4, Reg::G0, 16)),
                                                Operation::from(Instruction::Addi(Reg::G3, Reg::G4, 0)),
                                                Operation::LablMacro("MUL".into(), MacroInstr::Beq(Reg::G3, Reg::G4, "END".into())),
                                                Operation::from(Instruction::Mul(Reg::G6, Reg::G4, Reg::G3)),
                                                Operation::from(Instruction::Lui(Reg::G4, 90112)),
                                                Operation::from(MacroInstr::Jal(Reg::G0, "MUL".into())),
                                                Operation::Labl("END".into())
                                                ];

        assert_eq!(parse(source_code, &mut Some(&mut subroutines), &mut symbol_map, false),
                   Ok(("", correct_vec)));
        assert_eq!(symbol_map, symbols);
        assert!(subroutines.get_code().is_empty())
    }

    #[test]
    fn test_parse_repeat_instr() {
        let source_code = r#"
    ; testing repeat
TESTING: rep 1000, nop
"#;

        let mut subroutines = Subroutines::new();

        let mut symbol_map = LabelRecog::new();
        let mut symbols = LabelRecog::new();
        let mut label = LabelElem::new();
        label.set_name("TESTING".into());
        label.set_type(LabelType::Address);
        label.set_scope(true);
        label.set_def(0);
        let _ = symbols.insert_label(label);

        let mut correct_vec: Vec<Operation> = vec![];

        correct_vec.push(Operation::LablInstr("TESTING".into(), Instruction::Addi(Reg::G0, Reg::G0, 0)));

        for _ in 0..999 {
            correct_vec.push(Operation::Instr(Instruction::Addi(Reg::G0, Reg::G0, 0)));
        }

        assert_eq!(parse(source_code, &mut Some(&mut subroutines), &mut symbol_map, false),
                   Ok(("", correct_vec)));
        assert_eq!(symbol_map, symbols);
        assert!(subroutines.get_code().is_empty());
    }

    #[test]
    fn test_parse_repeat_macro() {
        let mut subroutines = Subroutines::new();

        let mut symbol_map = LabelRecog::new();
        let mut symbols = LabelRecog::new();
        let mut label = LabelElem::new();
        label.set_name("TESTING".into());
        label.set_type(LabelType::Address);
        label.set_scope(true);
        label.set_def(0);
        let _ = symbols.insert_label(label);

        let source_code_two = r#"
    ; testing repeat with macros
TESTING: rep 50, pop x15
"#;

        let mut sec_correct_vec: Vec<Operation> = vec![];

        sec_correct_vec.push(Operation::LablInstr("TESTING".into(), Instruction::Lw(Reg::G15, Reg::G2, 0)));
        sec_correct_vec.push(Operation::Instr(Instruction::Addi(Reg::G2, Reg::G2, 4)));

        for _ in 0..49 {
            sec_correct_vec.push(Operation::Instr(Instruction::Lw(Reg::G15, Reg::G2, 0)));
            sec_correct_vec.push(Operation::Instr(Instruction::Addi(Reg::G2, Reg::G2, 4)));
        }

        assert_eq!(parse(source_code_two, &mut Some(&mut subroutines), &mut symbol_map, false),
                   Ok(("", sec_correct_vec)));
        assert_eq!(symbol_map, symbols);
        assert!(subroutines.get_code().is_empty());
    }
}
