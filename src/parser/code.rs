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
    },
    sequence::{
        pair,
        separated_pair
    }
};
use std::cmp::Ordering;
use std::collections::BTreeMap;
use std::borrow::Cow;
use log::debug;

use crate::parser::{
    handle_label_defs, instructions::parse_instruction, literals::{
        parse_label_definition,
        parse_label_definition_priv
    }, Subroutines
};
use crate::common::*;

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

impl <'a> From<Instruction> for Operation<'a> {
    fn from(item: Instruction) -> Self {
        Operation::Instr(item)
    }
}

impl <'a> From<MacroInstr> for Operation<'a> {
    fn from(item: MacroInstr) -> Self {
        Operation::Macro(item)
    }
}

#[allow(clippy::type_complexity)]
fn parse_line(input: &str) -> IResult<&str, (Option<&str>, Option<Operation>)> {
    let (rest, early) = parse_multiline_comments(input)?;
    if early {
        return Ok((rest, (None, None)))
    }
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
    ))(rest)
}

#[allow(clippy::type_complexity)]
fn parse_line_priv(input: &str) -> IResult<&str, (Option<&str>, Option<Operation>)> {
    let (rest, early) = parse_multiline_comments(input)?;
    if early {
        return Ok((rest, (None, None)))
    }
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
    ))(rest)
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

        MacroInstr::Lui(_, labl) |
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
        
        MacroInstr::CallLabl(labl) |
        MacroInstr::TailLabl(labl) |
        MacroInstr::LaLabl(_, labl) => {
            symbol_map.crt_or_ref_label(labl);
        },
        MacroInstr::Srr(_, _, _) => {
            if let Some(subs) = subroutines {
                subs.srr_defined();
            };
            static LABEL: &str = "_SRR";
            symbol_map.crt_or_ref_label(&LABEL.to_string());
        },
        MacroInstr::Slr(_, _, _) => {
            if let Some(subs) = subroutines {
                subs.slr_defined();
            };
            static LABEL: &str = "_SLR";
            symbol_map.crt_or_ref_label(&LABEL.to_string());
        },

        _ => (),
    }
}

fn handle_abs_addr_label_conv<'b>(
    instr_counter: usize,
    abs_to_label_queue: &mut BTreeMap<usize, Vec<usize>>, 
    instr_list: &'b mut [Operation],
    symbol_map: &mut LabelRecog,
    imm: &Imm
) -> Option<Cow<'b, str>> {
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
            let jump_label: Cow<'_, str>;
            match &instr_list[jump_line] {
                Operation::Instr(instr) => {
                    jump_label = Cow::from("__".to_string() + &jump_line.to_string());
                    symbol_map.crt_def_ref(&jump_label.to_string(), false, jump_line as i128);
                    instr_list[jump_line] = Operation::LablInstr(jump_label.clone(), instr.to_owned());
                },
                Operation::Macro(macro_in) => {
                    jump_label = Cow::from("__".to_string() + &jump_line.to_string());
                    symbol_map.crt_def_ref(&jump_label.to_string(), false, jump_line as i128);
                    instr_list[jump_line] = Operation::LablMacro(jump_label.clone(), macro_in.to_owned());
                },
                Operation::LablInstr(labl, _) |
                Operation::LablMacro(labl, _) |
                Operation::Labl(labl) => {
                    jump_label = labl.clone();
                    symbol_map.set_refd_label(&labl.to_string());
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
                    Instruction::Beq(reg1, reg2, _) => instr_list[*origin] = Operation::Macro(MacroInstr::Beq(reg1.to_owned(), reg2.to_owned(), jump_label.to_string())),
                    Instruction::Bne(reg1, reg2, _) => instr_list[*origin] = Operation::Macro(MacroInstr::Bne(reg1.to_owned(), reg2.to_owned(), jump_label.to_string())),
                    Instruction::Blt(reg1, reg2, _) => instr_list[*origin] = Operation::Macro(MacroInstr::Blt(reg1.to_owned(), reg2.to_owned(), jump_label.to_string())),
                    Instruction::Bltu(reg1, reg2, _) => instr_list[*origin] = Operation::Macro(MacroInstr::Bltu(reg1.to_owned(), reg2.to_owned(), jump_label.to_string())),
                    Instruction::Bge(reg1, reg2, _) => instr_list[*origin] = Operation::Macro(MacroInstr::Bge(reg1.to_owned(), reg2.to_owned(), jump_label.to_string())),
                    Instruction::Bgeu(reg1, reg2, _) => instr_list[*origin] = Operation::Macro(MacroInstr::Bgeu(reg1.to_owned(), reg2.to_owned(), jump_label.to_string())),
                    Instruction::Jal(reg, _) => instr_list[*origin] = Operation::Macro(MacroInstr::Jal(reg.to_owned(), jump_label.to_string())),
                    Instruction::Jalr(reg1, reg2, _) => instr_list[*origin] = Operation::Macro(MacroInstr::Jalr(reg1.to_owned(), reg2.to_owned(), jump_label.to_string(), Part::None)),
                    op => {
                        println!("Matched instr: {:?}", op);
                        unreachable!()
                    },
                }
            },
            Operation::LablInstr(labl, instr) => {
                match instr {
                    Instruction::Beq(reg1, reg2, _) => instr_list[*origin] = Operation::LablMacro(labl.clone(), MacroInstr::Beq(reg1.to_owned(), reg2.to_owned(), jump_label.to_string())),
                    Instruction::Bne(reg1, reg2, _) => instr_list[*origin] = Operation::LablMacro(labl.clone(), MacroInstr::Bne(reg1.to_owned(), reg2.to_owned(), jump_label.to_string())),
                    Instruction::Blt(reg1, reg2, _) => instr_list[*origin] = Operation::LablMacro(labl.clone(), MacroInstr::Blt(reg1.to_owned(), reg2.to_owned(), jump_label.to_string())),
                    Instruction::Bltu(reg1, reg2, _) => instr_list[*origin] = Operation::LablMacro(labl.clone(), MacroInstr::Bltu(reg1.to_owned(), reg2.to_owned(), jump_label.to_string())),
                    Instruction::Bge(reg1, reg2, _) => instr_list[*origin] = Operation::LablMacro(labl.clone(), MacroInstr::Bge(reg1.to_owned(), reg2.to_owned(), jump_label.to_string())),
                    Instruction::Bgeu(reg1, reg2, _) => instr_list[*origin] = Operation::LablMacro(labl.clone(), MacroInstr::Bgeu(reg1.to_owned(), reg2.to_owned(), jump_label.to_string())),
                    Instruction::Jal(reg, _) => instr_list[*origin] = Operation::LablMacro(labl.clone(), MacroInstr::Jal(reg.to_owned(), jump_label.to_string())),
                    Instruction::Jalr(reg1, reg2, _) => instr_list[*origin] = Operation::LablMacro(labl.clone(), MacroInstr::Jalr(reg1.to_owned(), reg2.to_owned(), jump_label.to_string(), Part::None)),
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

fn split_list<'a>(instr_list: &mut Vec<Operation<'a>>, pointer: &usize) -> (Vec<Operation<'a>>, Vec<Operation<'a>>) {
    let mut right_list = instr_list.split_off(*pointer);
    if !right_list.is_empty() {
        right_list.remove(0);
    }
    (right_list, vec![])
}

fn incorporate_changes<'a>(
    instr_list: &mut Vec<Operation<'a>>, 
    mid_list: &mut Vec<Operation<'a>>, 
    right_list: &mut Vec<Operation<'a>>,
    accumulator: &mut i128,
    pointer: &mut usize,
    label: Option<Cow<'a, str>>
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

fn handle_multiline_immediate<'a>(
    imm: &mut i32, 
    label: Option<Cow<'a, str>>, 
    pointer: &mut usize, 
    instr_list: &mut Vec<Operation<'a>>,
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
        } else {
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

fn translate_macros<'a>(
    macro_in: &MacroInstr,
    instr_list: &mut Vec<Operation<'a>>,
    accumulator: &mut i128,
    pointer: &mut usize,
    label: Option<Cow<'a, str>>
) {
    match &macro_in {
        MacroInstr::Srr(reg1, reg2, imm) => {
            let (mut right_list, mut mid_list) = split_list(instr_list, pointer);

            if *reg2 != Reg::G10 {
                mid_list.push(Instruction::Addi(Reg::G10, reg2.to_owned(), 0).into());
            }
            mid_list.push(Instruction::Addi(Reg::G11, Reg::G0, *imm).into());
            mid_list.push(MacroInstr::Jal(Reg::G1, "_SRR".to_string()).into());
            if *reg1 != Reg::G10 {
                mid_list.push(Instruction::Addi(reg1.to_owned(), Reg::G10, 0).into());
            }

            debug!("Expanded '{:?}' at {} into '{:?}'", macro_in, *pointer, mid_list);

            incorporate_changes(instr_list, &mut mid_list, &mut right_list, accumulator, pointer, label);
        },
        MacroInstr::Slr(reg1, reg2, imm) => {
            let (mut right_list, mut mid_list) = split_list(instr_list, pointer);

            if *reg2 != Reg::G10 {
                mid_list.push(Instruction::Addi(Reg::G10, reg2.to_owned(), 0).into());
            }
            mid_list.push(Instruction::Addi(Reg::G11, Reg::G0, *imm).into());
            mid_list.push(MacroInstr::Jal(Reg::G1, "_SLR".to_string()).into());
            if *reg1 != Reg::G10 {
                mid_list.push(Instruction::Addi(reg1.to_owned(), Reg::G10, 0).into());
            }

            debug!("Expanded '{:?}' at {} into '{:?}'", macro_in, *pointer, mid_list);

            incorporate_changes(instr_list, &mut mid_list, &mut right_list, accumulator, pointer, label);
        },
        MacroInstr::Li(reg, imm) => {
            instr_list.remove(*pointer);
            let mut imm_used = *imm;

            if handle_multiline_immediate(&mut imm_used, label.clone(), pointer, instr_list, &Instruction::Addi(reg.to_owned(), Reg::G0, *imm)) {
                debug!("Expanded '{:?}' at {} into '{:?}'", macro_in, *pointer - 1, instr_list.last());
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

            debug!("Expanded '{:?}' at {} into '[{:?}, {:?}]'", macro_in, *pointer - 1, instr_list[*pointer-1], instr_list[*pointer]);

            *pointer += 1;
            *accumulator += 1;
        },
        MacroInstr::LaImm(reg, imm) => {
            instr_list.remove(*pointer);
            let mut imm_used = *imm;

            if handle_multiline_immediate(&mut imm_used, label.clone(), pointer, instr_list, &Instruction::Addi(reg.to_owned(), Reg::G0, *imm)) {
                debug!("Expanded '{:?}' at {} into '{:?}'", macro_in, *pointer - 1, instr_list.last());
                return
            }

            match label {
                Some(labl) => instr_list.insert(*pointer,
                                Operation::LablInstr(labl, Instruction::Auipc(reg.to_owned(), imm_used))),
                None => instr_list.insert(*pointer, Instruction::Auipc(reg.to_owned(), imm_used).into()),
            }
            *pointer += 1;
            instr_list.insert(*pointer,
            Instruction::Addi(reg.to_owned(), reg.to_owned(), imm_used).into());

            debug!("Expanded '{:?}' at {} into '[{:?}, {:?}]'", macro_in, *pointer - 1, instr_list[*pointer-1], instr_list[*pointer]);

            *pointer += 1;
            *accumulator += 1;
        },
        MacroInstr::LaLabl(reg, targ_labl) => {
            instr_list.remove(*pointer);
            match label {
                Some(labl) => instr_list.insert(*pointer,
                                Operation::LablMacro(labl, MacroInstr::Auipc(reg.to_owned(), targ_labl.to_string(), Part::Upper))),
                None => instr_list.insert(*pointer, MacroInstr::Auipc(reg.to_owned(), targ_labl.to_string(), Part::Upper).into())
            }
            *pointer += 1;
            instr_list.insert(*pointer,
            MacroInstr::Addi(reg.to_owned(), reg.to_owned(), targ_labl.to_string(), Part::Lower).into());

            debug!("Expanded '{:?}' at {} into '[{:?}, {:?}]'", macro_in, *pointer - 1, instr_list[*pointer-1], instr_list[*pointer]);

            *pointer += 1;
            *accumulator += 1;
        },
        MacroInstr::CallImm(imm) => {
            instr_list.remove(*pointer);
            let mut imm_used = *imm;

            if handle_multiline_immediate(&mut imm_used, label.clone(), pointer, instr_list, &Instruction::Jalr(Reg::G1, Reg::G0, *imm)) {
                debug!("Expanded '{:?}' at {} into '{:?}'", macro_in, *pointer - 1, instr_list.last());
                return
            }

            match label {
                Some(labl) => instr_list.insert(*pointer,
                                Operation::LablInstr(labl, Instruction::Auipc(Reg::G1, imm_used))),
                None => instr_list.insert(*pointer, Operation::Instr(Instruction::Auipc(Reg::G1, imm_used)))
            }
            *pointer += 1;
            instr_list.insert(*pointer,
            Instruction::Jalr(Reg::G1, Reg::G1, imm_used).into());

            debug!("Expanded '{:?}' at {} into '[{:?}, {:?}]'", macro_in, *pointer - 1, instr_list[*pointer-1], instr_list[*pointer]);

            *pointer += 1;
            *accumulator += 1;
        },
        MacroInstr::TailImm(imm) => {
            instr_list.remove(*pointer);
            let mut imm_used = *imm;

            if handle_multiline_immediate(&mut imm_used, label.clone(), pointer, instr_list, &Instruction::Jalr(Reg::G0, Reg::G0, *imm)) {
                debug!("Expanded '{:?}' at {} into '{:?}'", macro_in, *pointer - 1, instr_list.last());
                return
            }

            match label {
                Some(labl) => instr_list.insert(*pointer,
                                Operation::LablInstr(labl, Instruction::Auipc(Reg::G6, imm_used))),
                None => instr_list.insert(*pointer, Instruction::Auipc(Reg::G6, imm_used).into())
            }
            *pointer += 1;
            instr_list.insert(*pointer,
            Instruction::Jalr(Reg::G0, Reg::G6, imm_used).into());

            debug!("Expanded '{:?}' at {} into '[{:?}, {:?}]'", macro_in, *pointer - 1, instr_list[*pointer-1], instr_list[*pointer]);

            *pointer += 1;
            *accumulator += 1;
        },
        MacroInstr::CallLabl(targ_labl) => {
            instr_list.remove(*pointer);
            match label {
                Some(labl) => instr_list.insert(*pointer,
                                Operation::LablMacro(labl, MacroInstr::Auipc(Reg::G1, targ_labl.to_string(), Part::Upper))),
                None => instr_list.insert(*pointer, MacroInstr::Auipc(Reg::G1, targ_labl.to_string(), Part::Upper).into())
            }
            *pointer += 1;
            instr_list.insert(*pointer,
            MacroInstr::Jalr(Reg::G1, Reg::G1, targ_labl.to_string(), Part::Lower).into());

            debug!("Expanded '{:?}' at {} into '[{:?}, {:?}]'", macro_in, *pointer - 1, instr_list[*pointer-1], instr_list[*pointer]);

            *pointer += 1;
            *accumulator += 1;
        },
        MacroInstr::TailLabl(targ_labl) => {
            instr_list.remove(*pointer);
            match label {
                Some(labl) => instr_list.insert(*pointer,
                                Operation::LablMacro(labl, MacroInstr::Auipc(Reg::G6, targ_labl.to_string(), Part::Upper))),
                None => instr_list.insert(*pointer, MacroInstr::Auipc(Reg::G6, targ_labl.to_string(), Part::Upper).into())
            }
            *pointer += 1;
            instr_list.insert(*pointer,
            MacroInstr::Jalr(Reg::G0, Reg::G6, targ_labl.to_string(), Part::Lower).into());

            debug!("Expanded '{:?}' at {} into '[{:?}, {:?}]'", macro_in, *pointer - 1, instr_list[*pointer-1], instr_list[*pointer]);

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

            debug!("Expanded '{:?}' at {} into '{:?}'", macro_in, *pointer, mid_list);

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

            debug!("Expanded '{:?}' at {} into '{:?}'", macro_in, *pointer, mid_list);

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

            debug!("Expanded '{:?}' at {} into '{:?}'", macro_in, *pointer, mid_list);

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

            debug!("Expanded '{:?}' at {} into '{:?}'", macro_in, *pointer, mid_list);

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
                        if let Some(label) = symbol_map.get_label(&labl.to_string()) {
                            label.add_def(accumulator);
                        };
                        translate_macros(&macro_in, instr_list, &mut accumulator, &mut pointer, Some(labl));
                    },
                    Operation::LablInstr(labl, _) |
                    Operation::Labl(labl) => {
                        if let Some(label) = symbol_map.get_label(&labl.to_string()) {
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

// let mut symbol_map = LabelRecog::new();
pub fn parse<'a>(input: &'a str, subroutines: &mut Option<&mut Subroutines>, symbol_map: &mut LabelRecog) -> IResult<&'a str, Vec<Operation<'a>>> {
    let mut instr_list: Vec<Operation> = vec![];

    // Key = line forward; value = current line
    let mut abs_to_label_queue: BTreeMap<usize, Vec<usize>> = BTreeMap::new();

    let mut rest = input;
    let mut instr_counter: usize = 0;

    let privileged = subroutines.is_none();

    loop {
        let res = match privileged {
            true => parse_line_priv(rest),
            false => parse_line(rest),
        };

        let mut parsed = match res {
            Ok(line) => {
                rest = line.0;
                line.1
            },
            Err(e) => return Err(e),
        };

        match &mut parsed {
            (Some(label), Some(instr)) => {
                debug!("({instr_counter}) - Parsed label '{label}' and instruction '{:?}'", instr);
                handle_label_defs(label, symbol_map, instr_counter);

                match instr {
                    Operation::Macro(macro_in) => {
                        handle_label_refs(macro_in, subroutines, symbol_map);
                        *instr = Operation::LablMacro(std::borrow::Cow::Borrowed(label), macro_in.to_owned());
                    },
                    Operation::Instr(instr_in) => {
                        match instr_in {
                            Instruction::Beq(reg1, reg2, imm) => {
                                if let Some(jump_label) = handle_abs_addr_label_conv(instr_counter, &mut abs_to_label_queue, 
                                    &mut instr_list, symbol_map, imm) {
                                    *instr = Operation::LablMacro(std::borrow::Cow::Borrowed(label),
                                    MacroInstr::Beq(reg1.clone(), reg2.clone(), jump_label.to_string()));
                                }
                            },
                            Instruction::Bne(reg1, reg2, imm) => {
                                if let Some(jump_label) = handle_abs_addr_label_conv(instr_counter, 
                                    &mut abs_to_label_queue, &mut instr_list, symbol_map, imm) {
                                    *instr = Operation::LablMacro(std::borrow::Cow::Borrowed(label),
                                    MacroInstr::Bne(reg1.clone(), reg2.clone(), jump_label.to_string()));
                                }
                            },
                            Instruction::Blt(reg1, reg2, imm) => {
                                if let Some(jump_label) = handle_abs_addr_label_conv(instr_counter, &mut abs_to_label_queue, 
                                    &mut instr_list, symbol_map, imm) {
                                    *instr = Operation::LablMacro(std::borrow::Cow::Borrowed(label),
                                    MacroInstr::Blt(reg1.clone(), reg2.clone(), jump_label.to_string()));
                                }
                            },
                            Instruction::Bltu(reg1, reg2, imm) => {
                                if let Some(jump_label) = handle_abs_addr_label_conv(instr_counter, &mut abs_to_label_queue, 
                                    &mut instr_list, symbol_map, imm) {
                                    *instr = Operation::LablMacro(std::borrow::Cow::Borrowed(label),
                                    MacroInstr::Bltu(reg1.clone(), reg2.clone(), jump_label.to_string()));
                                }
                            },
                            Instruction::Bge(reg1, reg2, imm) => {
                                if let Some(jump_label) = handle_abs_addr_label_conv(instr_counter, &mut abs_to_label_queue, 
                                    &mut instr_list, symbol_map, imm) {
                                    *instr = Operation::LablMacro(std::borrow::Cow::Borrowed(label),
                                    MacroInstr::Bge(reg1.clone(), reg2.clone(), jump_label.to_string()));
                                }
                            },
                            Instruction::Bgeu(reg1, reg2, imm) => {
                                if let Some(jump_label) = handle_abs_addr_label_conv(instr_counter, &mut abs_to_label_queue, 
                                    &mut instr_list, symbol_map, imm) {
                                    *instr = Operation::LablMacro(std::borrow::Cow::Borrowed(label),
                                    MacroInstr::Bgeu(reg1.clone(), reg2.clone(), jump_label.to_string()));
                                }
                            },
                            Instruction::Jal(reg, imm) => {
                                if let Some(jump_label) = handle_abs_addr_label_conv(instr_counter, &mut abs_to_label_queue, 
                                    &mut instr_list, symbol_map, imm) {
                                    *instr = Operation::LablMacro(std::borrow::Cow::Borrowed(label),
                                    MacroInstr::Jal(reg.clone(), jump_label.to_string()));
                                }
                            },
                            Instruction::Jalr(reg1, reg2, imm) => {
                                if let Some(jump_label) = handle_abs_addr_label_conv(instr_counter, &mut abs_to_label_queue, 
                                    &mut instr_list, symbol_map, imm) {
                                    *instr = Operation::LablMacro(std::borrow::Cow::Borrowed(label), 
                                    MacroInstr::Jalr(reg1.clone(), reg2.clone(), jump_label.to_string(), Part::None));
                                }
                            },
                            _ => *instr = Operation::LablInstr(std::borrow::Cow::Borrowed(label), instr_in.to_owned()),
                        };
                    }
                    _ => (),
                }

                if let Some(list) = abs_to_label_queue.remove(&(instr_counter + 1)) {
                    symbol_map.set_refd_label(&label.to_string());
                    handle_instr_substitution(&mut instr_list, &list, label);
                };

                instr_counter += 1;
                instr_list.push(instr.to_owned());
            },
            (None, Some(instr)) => {
                debug!("({instr_counter}) - Parsed instruction '{:?}'", instr);
                match instr {
                    Operation::Macro(macro_in) => handle_label_refs(macro_in, subroutines, symbol_map),
                    Operation::Instr(instr_in) => {
                        match instr_in {
                            Instruction::Beq(reg1, reg2, imm) => {
                                if let Some(jump_label) = handle_abs_addr_label_conv(instr_counter, &mut abs_to_label_queue,
                                     &mut instr_list, symbol_map, imm) {
                                    *instr = Operation::Macro(MacroInstr::Beq(reg1.clone(), reg2.clone(), jump_label.to_string()));
                                }
                            },
                            Instruction::Bne(reg1, reg2, imm) => {
                                if let Some(jump_label) = handle_abs_addr_label_conv(instr_counter, &mut abs_to_label_queue, 
                                    &mut instr_list, symbol_map, imm) {
                                    *instr = Operation::Macro(MacroInstr::Bne(reg1.clone(), reg2.clone(), jump_label.to_string()));
                                }
                            },
                            Instruction::Blt(reg1, reg2, imm) => {
                                if let Some(jump_label) = handle_abs_addr_label_conv(instr_counter, &mut abs_to_label_queue, 
                                    &mut instr_list, symbol_map, imm) {
                                    *instr = Operation::Macro(MacroInstr::Blt(reg1.clone(), reg2.clone(), jump_label.to_string()));
                                }
                            },
                            Instruction::Bltu(reg1, reg2, imm) => {
                                if let Some(jump_label) = handle_abs_addr_label_conv(instr_counter, &mut abs_to_label_queue, 
                                    &mut instr_list, symbol_map, imm) {
                                    *instr = Operation::Macro(MacroInstr::Bltu(reg1.clone(), reg2.clone(), jump_label.to_string()));
                                }
                            },
                            Instruction::Bge(reg1, reg2, imm) => {
                                if let Some(jump_label) = handle_abs_addr_label_conv(instr_counter, &mut abs_to_label_queue, 
                                    &mut instr_list, symbol_map, imm) {
                                    *instr = Operation::Macro(MacroInstr::Bge(reg1.clone(), reg2.clone(), jump_label.to_string()));
                                }
                            },
                            Instruction::Bgeu(reg1, reg2, imm) => {
                                if let Some(jump_label) = handle_abs_addr_label_conv(instr_counter, &mut abs_to_label_queue, 
                                    &mut instr_list, symbol_map, imm) {
                                    *instr = Operation::Macro(MacroInstr::Bgeu(reg1.clone(), reg2.clone(), jump_label.to_string()));
                                }
                            },
                            Instruction::Jal(reg, imm) => {
                                if let Some(jump_label) = handle_abs_addr_label_conv(instr_counter, &mut abs_to_label_queue, 
                                    &mut instr_list, symbol_map, imm) {
                                    *instr = Operation::Macro(MacroInstr::Jal(reg.clone(), jump_label.to_string())); 
                                }
                            },
                            Instruction::Jalr(reg1, reg2, imm) => {
                                if let Some(jump_label) = handle_abs_addr_label_conv(instr_counter, &mut abs_to_label_queue, 
                                    &mut instr_list, symbol_map, imm) {
                                    *instr = Operation::Macro(MacroInstr::Jalr(reg1.clone(), reg2.clone(), jump_label.to_string(), Part::None));   
                                }
                            },
                            _ => (),
                        };
                    }
                    _ => (),
                }
                
                if let Some(list) = abs_to_label_queue.remove(&(instr_counter + 1)) {
                    let jump_label = Cow::from("__".to_string() + &instr_counter.to_string());
                    symbol_map.crt_def_ref(&jump_label.to_string(), false, instr_counter as i128);
                    handle_instr_substitution(&mut instr_list, &list, &jump_label);
                    match &instr {
                        Operation::Instr(instr_in) => *instr = Operation::LablInstr(jump_label, instr_in.to_owned()),
                        Operation::Macro(macro_in) => *instr = Operation::LablMacro(jump_label, macro_in.to_owned()),
                        _ => unreachable!()
                    }
                };

                instr_counter += 1;
                instr_list.push(instr.to_owned());
            },
            (Some(label), None) => {
                debug!("({instr_counter}) - Parsed label '{label}'");
                handle_label_defs(label, symbol_map, instr_counter);
                if let Some(list) = abs_to_label_queue.remove(&(instr_counter + 1)) {
                    symbol_map.set_refd_label(&label.to_string());
                    handle_instr_substitution(&mut instr_list, &list, label)
                };
                instr_counter += 1;
                instr_list.push(Operation::Labl(std::borrow::Cow::Borrowed(label)));
            },
            (None, None) => debug!("{instr_counter} - Parsed nothing!"),
        }

        if rest.trim().is_empty() {
            break;
        }
    }

    if !abs_to_label_queue.is_empty() {
        let jump_label = match &instr_list[instr_counter - 1] {
            Operation::Labl(labl) => {
                symbol_map.set_refd_label(&labl.to_string());
                labl.clone()
            },
            _ => {
                let res = Cow::from("__".to_string() + &instr_counter.to_string());
                symbol_map.crt_def_ref(&res.to_string(), false, instr_counter as i128);
                res
            }
        };
        for (_, elem) in abs_to_label_queue.iter() {
            handle_instr_substitution(&mut instr_list, elem, &jump_label);
        }
        instr_list.push(Operation::Labl(jump_label));
    }

    expand_instrs(symbol_map, &mut instr_list);

    debug!("Finished parser step");

    Ok(("", instr_list))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_line() {
        assert_eq!(parse_line("label: add x1, x5, x6"),
                   Ok(("", (Some("label"), Some(Instruction::Add(Reg::G1, Reg::G5, Reg::G6).into())))));
        assert_eq!(parse_line("\ntest:\n\nsub x6, x5, x11"),
                   Ok(("", (Some("test"), Some(Instruction::Sub(Reg::G6, Reg::G5, Reg::G11).into())))));
        assert_eq!(parse_line("\n\n\nreturn:\n"),
                   Ok(("\n", (Some("return"), None))));
        assert_eq!(parse_line("mv x15, x12\naddi x12, x10, 0x05"),
                   Ok(("\naddi x12, x10, 0x05", (None, Some(Instruction::Addi(Reg::G15, Reg::G12, 0).into())))));
        assert_eq!(parse_line("label:\ndiv x14, x13, x10"),
                   Ok(("", (Some("label"), Some(Instruction::Div(Reg::G14, Reg::G13, Reg::G10).into())))));
    }

    #[test]
    fn test_parse_line_privileged() {
        assert_eq!(parse_line_priv("_label: add x1, x5, x6"),
                    Ok(("", (Some("_label"), Some(Instruction::Add(Reg::G1, Reg::G5, Reg::G6).into())))));
        assert_eq!(parse_line_priv("\n_test:\n\nsub x6, x5, x11"),
                    Ok(("", (Some("_test"), Some(Instruction::Sub(Reg::G6, Reg::G5, Reg::G11).into())))));
        assert_eq!(parse_line_priv("\n\n\n_return:\n"),
                    Ok(("\n", (Some("_return"), None))));
        assert_eq!(parse_line_priv("mv x15, x12\naddi x12, x10, 0x05"),
                    Ok(("\naddi x12, x10, 0x05", (None, Some(Instruction::Addi(Reg::G15, Reg::G12, 0).into())))));
        assert_eq!(parse_line_priv("_label:\ndiv x14, x13, x10"),
                    Ok(("", (Some("_label"), Some(Instruction::Div(Reg::G14, Reg::G13, Reg::G10).into())))));
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
        label.set_name("START".to_string());
        label.set_scope(true);
        label.set_def(0);
        let _ = symbols.insert_label(label);

        label = LabelElem::new_refd("MUL".to_string());
        label.set_scope(true);
        label.set_def(2);
        let _ = symbols.insert_label(label);

        label = LabelElem::new_refd("END".to_string());
        label.set_scope(true);
        label.set_def(6);
        //label.set_def(9);
        let _ = symbols.insert_label(label);

        let correct_vec: Vec<Operation> = vec![
                                                //Operation::LablInstr(Cow::from("START"), Instruction::Lui(Reg::G4, 16)),
                                                //Operation::from(Instruction::Addi(Reg::G4, Reg::G4, 16)),
                                                Operation::LablInstr(Cow::from("START"), Instruction::Addi(Reg::G4, Reg::G0, 16)),
                                                Operation::from(Instruction::Addi(Reg::G3, Reg::G4, 0)),
                                                Operation::LablMacro(Cow::from("MUL"), MacroInstr::Beq(Reg::G3, Reg::G4, "END".to_string())),
                                                Operation::from(Instruction::Mul(Reg::G6, Reg::G4, Reg::G3)),
                                                Operation::from(Instruction::Lui(Reg::G4, 0x16)),
                                                Operation::from(MacroInstr::Jal(Reg::G0, "MUL".to_string())),
                                                Operation::Labl(Cow::from("END"))
                                                ];

        assert_eq!(parse(source_code, &mut Some(&mut subroutines), &mut symbol_map),
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
    j   -12
"#;
        let mut subroutines = Subroutines::new();

        let mut symbol_map = LabelRecog::new();
        let mut symbols = LabelRecog::new();

        /*let mut label = LabelElem::new_refd("_MUL".to_string());
        label.set_scope(true);
        let _ = symbols.insert_label(label);*/

        let mut label = LabelElem::new_refd("__3".to_string());
        label.set_scope(false);
        label.set_def(3);
        let _ = symbols.insert_label(label);

        label = LabelElem::new_refd("__6".to_string());
        label.set_scope(false);
        label.set_def(6);
        //label.set_def(9);
        let _ = symbols.insert_label(label);

        label = LabelElem::new_refd("__7".to_string());
        label.set_scope(false);
        label.set_def(7);
        //label.set_def(10);
        let _ = symbols.insert_label(label);
        
        let correct_vec: Vec<Operation> = vec![
                                                //Operation::Instr(Instruction::Lui(Reg::G4, 16)),
                                                //Operation::from(Instruction::Addi(Reg::G4, Reg::G4, 16)),
                                                Instruction::Addi(Reg::G4, Reg::G0, 16).into(),
                                                Operation::from(Instruction::Addi(Reg::G3, Reg::G4, 0)),
                                                Operation::Macro(MacroInstr::Beq(Reg::G3, Reg::G4, "__6".to_string())),
                                                Operation::LablInstr(Cow::from("__3"), Instruction::Mul(Reg::G6, Reg::G4, Reg::G3)),
                                                Operation::Macro(MacroInstr::Beq(Reg::G3, Reg::G4, "__7".to_string())),
                                                Operation::from(Instruction::Lui(Reg::G4, 0x16)),
                                                Operation::LablMacro(Cow::from("__6"), MacroInstr::Jal(Reg::G0, "__3".to_string())),
                                                Operation::Labl(Cow::from("__7"))
                                                ];

        assert_eq!(parse(source_code, &mut Some(&mut subroutines), &mut symbol_map),
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

        let mut label = LabelElem::new_refd("__9".to_string());
        label.set_scope(false);
        label.set_def(9);
        let _ = symbols.insert_label(label);

        label = LabelElem::new_refd("__10".to_string());
        label.set_scope(false);
        label.set_def(10);
        let _ = symbols.insert_label(label);

        label = LabelElem::new_refd("__19".to_string());
        label.set_scope(false);
        label.set_def(19);
        let _ = symbols.insert_label(label);


        label = LabelElem::new_refd("TEST".to_string());
        label.set_scope(true);
        label.set_def(26);
        let _ = symbols.insert_label(label);

        label = LabelElem::new_refd("__29".to_string());
        label.set_scope(false);
        label.set_def(29);
        let _ = symbols.insert_label(label);

        let mut correct_vec: Vec<Operation> = vec![];
        correct_vec.push(Operation::Instr(Instruction::Addi(Reg::G17, Reg::G0, 1)));
        correct_vec.push(Operation::Instr(Instruction::Addi(Reg::G12, Reg::G10, 0)));
        correct_vec.push(Operation::Instr(Instruction::Addi(Reg::G13, Reg::G11, 0)));
        correct_vec.push(Operation::Instr(Instruction::Addi(Reg::G10, Reg::G0, 0)));
        correct_vec.push(Operation::Instr(Instruction::Addi(Reg::G11, Reg::G0, 0)));

        correct_vec.push(Operation::Macro(MacroInstr::Bne(Reg::G12, Reg::G13, "__9".to_string())));
        correct_vec.push(Operation::Instr(Instruction::Slli(Reg::G13, Reg::G13, 1)));
        correct_vec.push(Operation::Instr(Instruction::Sub(Reg::G12, Reg::G12, Reg::G13)));
        correct_vec.push(Operation::Instr(Instruction::Add(Reg::G10, Reg::G10, Reg::G17)));
        correct_vec.push(Operation::LablMacro(Cow::from("__9"), MacroInstr::Blt(Reg::G12, Reg::G13, "__19".to_string())));

        correct_vec.push(Operation::LablInstr(Cow::from("__10"), Instruction::Slli(Reg::G13, Reg::G13, 1)));
        correct_vec.push(Operation::Instr(Instruction::Slli(Reg::G17, Reg::G17, 1)));
        correct_vec.push(Operation::Macro(MacroInstr::Blt(Reg::G13, Reg::G12, "__10".to_string())));
        correct_vec.push(Operation::Instr(Instruction::Srli(Reg::G13, Reg::G13, 1)));
        correct_vec.push(Operation::Instr(Instruction::Srli(Reg::G17, Reg::G17, 1)));
        correct_vec.push(Operation::Instr(Instruction::Sub(Reg::G12, Reg::G12, Reg::G13)));
        correct_vec.push(Operation::Instr(Instruction::Add(Reg::G10, Reg::G10, Reg::G17)));
        correct_vec.push(Operation::Macro(MacroInstr::Bne(Reg::G12, Reg::G0, "__9".to_string())));
        correct_vec.push(Operation::Macro(MacroInstr::Beq(Reg::G0, Reg::G0, "__29".to_string())));

        correct_vec.push(Operation::LablInstr(Cow::from("__19"), Instruction::Srli(Reg::G13, Reg::G13, 1)));
        correct_vec.push(Operation::Instr(Instruction::Srli(Reg::G17, Reg::G17, 1)));
        correct_vec.push(Operation::Macro(MacroInstr::Blt(Reg::G12, Reg::G13, "__19".to_string())));
        correct_vec.push(Operation::Instr(Instruction::Slli(Reg::G13, Reg::G13, 1)));
        correct_vec.push(Operation::Instr(Instruction::Slli(Reg::G17, Reg::G17, 1)));
        correct_vec.push(Operation::Macro(MacroInstr::Beq(Reg::G12, Reg::G13, "TEST".to_string())));
        correct_vec.push(Operation::Instr(Instruction::Srli(Reg::G13, Reg::G13, 1)));
        correct_vec.push(Operation::LablInstr(Cow::from("TEST"), Instruction::Srli(Reg::G17, Reg::G17, 1)));
        correct_vec.push(Operation::Instr(Instruction::Sub(Reg::G12, Reg::G12, Reg::G13)));
        correct_vec.push(Operation::Instr(Instruction::Add(Reg::G10, Reg::G10, Reg::G17)));
        correct_vec.push(Operation::LablMacro(Cow::from("__29"), MacroInstr::Bne(Reg::G12, Reg::G0, "__9".to_string())));
        correct_vec.push(Operation::Instr(Instruction::Jalr(Reg::G0, Reg::G1, 0)));

        assert_eq!(parse(source_code, &mut Some(&mut subroutines), &mut symbol_map),
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
            let label: Cow<'_, str> = Cow::from("TEST3");
            
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
                Operation::Macro(MacroInstr::Jal(Reg::G1, "_SRR".to_string())),
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
            let label: Cow<'_, str> = Cow::from("TEST66");
            
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
                Operation::Macro(MacroInstr::Jal(Reg::G1, "_SLR".to_string())),
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
                Operation::Macro(MacroInstr::LaLabl(Reg::G7, 
                    target_label.to_string())));
            let label: Cow<'_, str> = Cow::from("TEST80");
            
            translate_macros(&MacroInstr::LaLabl(Reg::G7, 
                target_label.to_string()), 
            &mut test_list, 
            &mut accumulator, 
            &mut pointer, 
            Some(label.clone()));

            assert_eq!(accumulator, 1);
            assert_eq!(pointer, 4);

            let cor_vec: Vec<Operation> = Vec::from([
                Operation::Instr(Instruction::Addi(Reg::G17, Reg::G0, 1)),
                Operation::Instr(Instruction::Addi(Reg::G12, Reg::G10, 0)),
                Operation::LablMacro(label, MacroInstr::Auipc(Reg::G7, target_label.to_string(), Part::Upper)),
                Operation::Macro(MacroInstr::Addi(Reg::G7, Reg::G7, target_label.to_string(), Part::Lower)),
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
                Operation::Macro(MacroInstr::CallImm(50)));
            let label: Cow<'_, str> = Cow::from("TEST80");
            
            translate_macros(&MacroInstr::CallImm(50), 
            &mut test_list, 
            &mut accumulator, 
            &mut pointer, 
            Some(label.clone()));

            assert_eq!(accumulator, 0);
            assert_eq!(pointer, 3);

            let cor_vec: Vec<Operation> = Vec::from([
                Operation::Instr(Instruction::Addi(Reg::G17, Reg::G0, 1)),
                Operation::Instr(Instruction::Addi(Reg::G12, Reg::G10, 0)),
                //Operation::LablInstr(label, Instruction::Auipc(Reg::G1, 0)),
                //Operation::Instr(Instruction::Jalr(Reg::G1, Reg::G1, 50)),
                Operation::LablInstr(label, Instruction::Jalr(Reg::G1, Reg::G0, 50)),
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
                Operation::Macro(MacroInstr::TailImm(50)));
            let label: Cow<'_, str> = Cow::from("TEST80");
            
            translate_macros(&MacroInstr::TailImm(50), 
            &mut test_list, 
            &mut accumulator, 
            &mut pointer, 
            Some(label.clone()));

            assert_eq!(accumulator, 0);
            assert_eq!(pointer, 3);

            let cor_vec: Vec<Operation> = Vec::from([
                Operation::Instr(Instruction::Addi(Reg::G17, Reg::G0, 1)),
                Operation::Instr(Instruction::Addi(Reg::G12, Reg::G10, 0)),
                //Operation::LablInstr(label, Instruction::Auipc(Reg::G6, 0)),
                //Operation::Instr(Instruction::Jalr(Reg::G0, Reg::G6, 50)),
                Operation::LablInstr(label, Instruction::Jalr(Reg::G0, Reg::G0, 50)),
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
                Operation::Macro(MacroInstr::CallLabl(target_label.to_string())));
            let label: Cow<'_, str> = Cow::from("TEST80");
            
            translate_macros(&MacroInstr::CallLabl(target_label.to_string()), 
            &mut test_list, 
            &mut accumulator, 
            &mut pointer, 
            Some(label.clone()));

            assert_eq!(accumulator, 1);
            assert_eq!(pointer, 4);

            let cor_vec: Vec<Operation> = Vec::from([
                Operation::Instr(Instruction::Addi(Reg::G17, Reg::G0, 1)),
                Operation::Instr(Instruction::Addi(Reg::G12, Reg::G10, 0)),
                Operation::LablMacro(label, MacroInstr::Auipc(Reg::G1, target_label.to_string(), Part::Upper)),
                Operation::Macro(MacroInstr::Jalr(Reg::G1, Reg::G1, target_label.to_string(), Part::Lower)),
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
                Operation::Macro(MacroInstr::TailLabl(target_label.to_string())));
            let label: Cow<'_, str> = Cow::from("TEST80");
            
            translate_macros(&MacroInstr::TailLabl(target_label.to_string()), 
            &mut test_list, 
            &mut accumulator, 
            &mut pointer, 
            Some(label.clone()));

            assert_eq!(accumulator, 1);
            assert_eq!(pointer, 4);

            let cor_vec: Vec<Operation> = Vec::from([
                Operation::Instr(Instruction::Addi(Reg::G17, Reg::G0, 1)),
                Operation::Instr(Instruction::Addi(Reg::G12, Reg::G10, 0)),
                Operation::LablMacro(label, MacroInstr::Auipc(Reg::G6, target_label.to_string(), Part::Upper)),
                Operation::Macro(MacroInstr::Jalr(Reg::G0, Reg::G6, target_label.to_string(), Part::Lower)),
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
            let label: Cow<'_, str> = Cow::from("TEST80");
            
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
            let label: Cow<'_, str> = Cow::from("TEST80");
            
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
            let label: Cow<'_, str> = Cow::from("TEST80");
            
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
        label.set_name("START".to_string());
        label.set_scope(true);
        label.set_def(0);
        let _ = symbols.insert_label(label);

        label = LabelElem::new_refd("MUL".to_string());
        label.set_scope(true);
        label.set_def(2);
        let _ = symbols.insert_label(label);

        label = LabelElem::new_refd("END".to_string());
        label.set_scope(true);
        label.set_def(6);
        //label.set_def(9);
        let _ = symbols.insert_label(label);

        let correct_vec: Vec<Operation> = vec![
                                                Operation::LablInstr(Cow::from("START"), Instruction::Addi(Reg::G4, Reg::G0, 16)),
                                                Operation::from(Instruction::Addi(Reg::G3, Reg::G4, 0)),
                                                Operation::LablMacro(Cow::from("MUL"), MacroInstr::Beq(Reg::G3, Reg::G4, "END".to_string())),
                                                Operation::from(Instruction::Mul(Reg::G6, Reg::G4, Reg::G3)),
                                                Operation::from(Instruction::Lui(Reg::G4, 0x16)),
                                                Operation::from(MacroInstr::Jal(Reg::G0, "MUL".to_string())),
                                                Operation::Labl(Cow::from("END"))
                                                ];

        assert_eq!(parse(source_code, &mut Some(&mut subroutines), &mut symbol_map),
                   Ok(("", correct_vec)));
        assert_eq!(symbol_map, symbols);
        assert!(subroutines.get_code().is_empty())
    }

    #[test]
    fn test_parse_repeat() {
        let source_code = r#"
    ; testing repeat
TESTING: rep 1000, nop
"#;

        let mut subroutines = Subroutines::new();

        let mut symbol_map = LabelRecog::new();
        let mut symbols = LabelRecog::new();
        let mut label = LabelElem::new();
        label.set_name("TESTING".to_string());
        label.set_scope(true);
        label.set_def(0);
        let _ = symbols.insert_label(label);

        let mut correct_vec: Vec<Operation> = vec![];

        correct_vec.push(Operation::LablInstr(Cow::from("TESTING"), Instruction::Addi(Reg::G0, Reg::G0, 0)));

        for _ in 0..999 {
            correct_vec.push(Operation::Instr(Instruction::Addi(Reg::G0, Reg::G0, 0)));
        }

        assert_eq!(parse(source_code, &mut Some(&mut subroutines), &mut symbol_map),
                   Ok(("", correct_vec)));
        assert_eq!(symbol_map, symbols);
        assert!(subroutines.get_code().is_empty());

        let source_code_two = r#"
    ; testing repeat with macros
TESTING: rep 50, pop x15
"#;

        let mut sec_correct_vec: Vec<Operation> = vec![];

        sec_correct_vec.push(Operation::LablInstr(Cow::from("TESTING"), Instruction::Lw(Reg::G15, Reg::G2, 0)));
        sec_correct_vec.push(Operation::Instr(Instruction::Addi(Reg::G2, Reg::G2, 4)));

        for _ in 0..49 {
            sec_correct_vec.push(Operation::Instr(Instruction::Lw(Reg::G15, Reg::G2, 0)));
            sec_correct_vec.push(Operation::Instr(Instruction::Addi(Reg::G2, Reg::G2, 4)));
        }

        assert_eq!(parse(source_code_two, &mut Some(&mut subroutines), &mut symbol_map),
                   Ok(("", sec_correct_vec)));
        assert_eq!(symbol_map, symbols);
        assert!(subroutines.get_code().is_empty());
    }
}