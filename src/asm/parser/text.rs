/*
 * Copyright (c) 2023 Steven Becker
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

mod op_exp;

use winnow::{
    ascii::{space0, space1, till_line_ending}, combinator::{
        alt, delimited, empty, eof, fail, not, opt, preceded, separated_pair, terminated
    }, error::{StrContext, StrContextValue}, token::none_of, PResult, Parser
};
use std::{any::Any, cmp::Ordering};
use std::collections::BTreeMap;
use log::{debug, error};

use crate::asm::parser::{
    handle_label_defs, instructions::parse_instruction, literals::{
        parse_label_definition,
        parse_label_definition_priv
    }, Subroutines
};
use crate::common::*;

use self::{errors::ParserError, op_exp::expand_instrs};

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

fn real_parse_line(input: &mut &str) -> PResult<Box<dyn LineHandle>> {
    alt((
        separated_pair(
            parse_label_definition.map(Some),
            space1,
            parse_instruction.map(Some).context(StrContext::Label("instruction or macro"))
        ),
        (
            parse_label_definition.map(Some), 
            empty.value(None)
        ),
        (
            empty.value(None),
            parse_instruction.map(Some).context(StrContext::Label("instruction or macro"))
        ),
        (
            empty.value(None),
            not(none_of(('\n', ';', '\r'))).value(None).context(StrContext::Label("operation"))
        )
    ))
    .output_into()
    .parse_next(input)
}

fn parse_line(input: &mut &str) -> PResult<Box<dyn LineHandle>> {
    terminated(
        delimited(space0, real_parse_line, space0), 
        alt((
            (';', till_line_ending).void(),
            not(none_of(('\n', ';', '\r'))).void(),
        )).context(StrContext::Label("comment")).context(StrContext::Expected(StrContextValue::CharLiteral(';')))
    ).parse_next(input)
}

fn real_parse_line_priv(input: &mut &str) -> PResult<Box<dyn LineHandle>> {
    alt((
        separated_pair(
            parse_label_definition_priv.map(Some),
            space1,
            parse_instruction.map(Some).context(StrContext::Label("instruction or macro"))
        ),
        (
            parse_label_definition_priv.map(Some), 
            empty.value(None)
        ),
        (
            empty.value(None),
            parse_instruction.map(Some).context(StrContext::Label("instruction or macro"))
        ),
        (
            empty.value(None),
            not(none_of(('\n', ';', '\r'))).value(None).context(StrContext::Label("operation"))
        )
    ))
    .output_into()
    .parse_next(input)
}

fn parse_line_priv(input: &mut &str) -> PResult<Box<dyn LineHandle>> {
    terminated(
        delimited(space0, real_parse_line_priv, space0), 
        alt((
            (';', till_line_ending).void(),
            not(none_of(('\n', ';', '\r'))).void(),
        )).context(StrContext::Label("comment")).context(StrContext::Expected(StrContextValue::CharLiteral(';')))
    ).parse_next(input)
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
        MacroInstr::Auipc(_, labl) |

        MacroInstr::LbLabl(_, _, labl, _) |
        MacroInstr::LhLabl(_, _, labl, _) |
        MacroInstr::LwLabl(_, _, labl, _) |

        MacroInstr::LbuLabl(_, _, labl) |
        MacroInstr::LhuLabl(_, _, labl) |

        MacroInstr::SbLabl(_, _, labl) |
        MacroInstr::ShLabl(_, _, labl) |
        MacroInstr::SwLabl(_, _, labl) | 
        
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
                        unreachable!("Matched instr: {:?}", op)
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
                        unreachable!("Matched labl: {}, matched instr: {:?}", labl, op)
                    },
                }
            },
            op => {
                unreachable!("Matched operation: {:?}", op)
            },
        }
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

        if let Some(Operation::Labl(labl)) = instr_list.last() {
            let label = labl.clone();
            let length = instr_list.len() - 1;
            match &instr {
                Operation::Instr(instr_in) => instr_list[length] = Operation::LablInstr(labl.clone(), instr_in.to_owned()),
                Operation::Macro(macro_in) => instr_list[length] = Operation::LablMacro(labl.clone(), macro_in.to_owned()),
                op => unreachable!("{op}")
            }
            if let Some(list) = abs_to_label_queue.remove(&(*instr_counter + 1)) {
                handle_instr_substitution(instr_list, &list, &label);
            }
        } else {
            if let Some(list) = abs_to_label_queue.remove(&(*instr_counter + 1)) {
                let jump_label = smartstring::alias::String::from("__") + instr_counter.to_string().as_str();
                symbol_map.crt_def_ref(&jump_label, false, LabelType::Address, *instr_counter as i128);
                handle_instr_substitution(instr_list, &list, &jump_label);
                match &instr {
                    Operation::Instr(instr_in) => *instr = Operation::LablInstr(jump_label, instr_in.to_owned()),
                    Operation::Macro(macro_in) => *instr = Operation::LablMacro(jump_label, macro_in.to_owned()),
                    _ => unreachable!()
                }
            }
            instr_list.push(instr.to_owned());
        }

        *instr_counter += 1;
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

pub fn parse(input: &mut &str, subroutines: &mut Option<&mut Subroutines>, symbol_map: &mut LabelRecog) -> PResult<Vec<Operation>> {
    let mut instr_list: Vec<Operation> = vec![];

    // Key = line forward; value = current line
    let mut abs_to_label_queue: BTreeMap<usize, Vec<usize>> = BTreeMap::new();
    let mut instr_counter: usize = 0;

    let privileged = subroutines.is_none();

    loop {
        let mut parsed = preceded(opt('\n'), match privileged {
            true =>  parse_line_priv,
            false => parse_line,
        }).parse_next(input)?;

        if let Err(e) = parsed.handle(&mut instr_counter, &mut instr_list, subroutines, symbol_map, &mut abs_to_label_queue) {
            error!("{e}");
            return fail.context(StrContext::Label(e.get_nom_err_text())).parse_next(input)
        }

        if opt(eof).parse_next(input)?.is_some() {
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

    Ok(instr_list)
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! assert_equ_line {
        ($pars_line:literal, $rest:literal, $struct:ident { $( $field:ident: $val:expr ),*}) => {
            let parsed = parse_line(&mut $pars_line).unwrap();
            //assert_eq!(&mut &$pars_line, &mut &$rest);
            assert_eq!(parsed.as_any().downcast_ref::<$struct>().unwrap(), &$struct { $( $field: $val ),* });
        };
    }

    #[test]
    fn test_parse_line_o() -> Result<(), Box<dyn std::error::Error>> {
        assert_equ_line!("label: add x1, x5, x6", "",
                         LabelOperationData { label: "label".into(), op: Instruction::Add(Reg::G1, Reg::G5, Reg::G6).into() });
        assert_equ_line!("\ntest:\n\nsub x6, x5, x11", "",
                         NoData { });
        assert_equ_line!("test:\n\nsub x6, x5, x11", "",
                         LabelDef { label: "test".into() });
        assert_equ_line!("\n\n\nreturn:\n", "\n", NoData { });
        assert_equ_line!("test: sub x6, x5, x11", "",
                        LabelOperationData { label: "test".into(), op: Instruction::Sub(Reg::G6, Reg::G5, Reg::G11).into() });
        assert_equ_line!("mv x15, x12\naddi x12, x10, 0x05", "\naddi x12, x10, 0x05",
                         OperationData { op: Instruction::Addi(Reg::G15, Reg::G12, 0).into() });
        assert_equ_line!("label:        div x14, x13, x10", "",
                         LabelOperationData { label: "label".into(), op: Instruction::Div(Reg::G14, Reg::G13, Reg::G10).into() });
        Ok(())
    }

    macro_rules! assert_equ_line_priv {
        ($pars_line:literal, $rest:literal, $struct:ident { $( $field:ident: $val:expr ),*}) => {
            let parsed = parse_line_priv(&mut $pars_line).unwrap();
            //assert_eq!(&mut &$pars_line, &mut &$rest);
            assert_eq!(parsed.as_any().downcast_ref::<$struct>().unwrap(), &$struct { $( $field: $val ),* });
        };
    }

    #[test]
    fn test_parse_line_privileged() -> Result<(), Box<dyn std::error::Error>> {
        assert_equ_line_priv!("_label: add x1, x5, x6", "",
                         LabelOperationData { label: "_label".into(), op: Instruction::Add(Reg::G1, Reg::G5, Reg::G6).into() });
        assert_equ_line_priv!("\n_test:\n\nsub x6, x5, x11", "",
                         NoData { });
        assert_equ_line_priv!("_test:\n\nsub x6, x5, x11", "",
                         LabelDef { label: "_test".into() });
        assert_equ_line_priv!("\n\n\n_return:\n", "\n", NoData { });
        assert_equ_line_priv!("_test: sub x6, x5, x11", "",
                        LabelOperationData { label: "_test".into(), op: Instruction::Sub(Reg::G6, Reg::G5, Reg::G11).into() });
        assert_equ_line_priv!("mv x15, x12\naddi x12, x10, 0x05", "\naddi x12, x10, 0x05",
                         OperationData { op: Instruction::Addi(Reg::G15, Reg::G12, 0).into() });
        assert_equ_line_priv!("_label:        div x14, x13, x10", "",
                         LabelOperationData { label: "_label".into(), op: Instruction::Div(Reg::G14, Reg::G13, Reg::G10).into() });
        Ok(())
    }

    #[test]
    fn test_parse_o() {
        let mut source_code = r#"START:
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

        assert_eq!(parse(&mut source_code, &mut Some(&mut subroutines), &mut symbol_map),
                   Ok(correct_vec));
        assert_eq!(symbol_map, symbols);
        assert!(subroutines.get_code().is_empty())
        // TODO: Probably more test cases!
    }

    #[test]
    fn test_parse_abs_addresses_smallex() {
        let mut source_code = 
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

        assert_eq!(parse(&mut source_code, &mut Some(&mut subroutines), &mut symbol_map),
                   Ok(correct_vec));
        assert_eq!(symbol_map, symbols);
        assert_eq!(subroutines.get_code().is_empty(), true);
    }

    #[test]
    fn test_parse_abs_addresses_biggerex() {
        let mut source_code = 
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

        assert_eq!(parse(&mut source_code, &mut Some(&mut subroutines), &mut symbol_map),
                   Ok(correct_vec));
        assert_eq!(symbol_map, symbols);
        assert!(subroutines.get_code().is_empty());
    }

    #[test]
    fn test_parse_comment() {
        let mut source_code = r#"
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

        assert_eq!(parse(&mut source_code, &mut Some(&mut subroutines), &mut symbol_map),
                   Ok(correct_vec));
        assert_eq!(symbol_map, symbols);
        assert!(subroutines.get_code().is_empty())
    }

    #[test]
    fn test_parse_repeat_instr() {
        let mut source_code = r#"
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

        assert_eq!(parse(&mut source_code, &mut Some(&mut subroutines), &mut symbol_map),
                   Ok(correct_vec));
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

        let mut source_code_two = r#"
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

        assert_eq!(parse(&mut source_code_two, &mut Some(&mut subroutines), &mut symbol_map),
                   Ok(sec_correct_vec));
        assert_eq!(symbol_map, symbols);
        assert!(subroutines.get_code().is_empty());
    }
}
