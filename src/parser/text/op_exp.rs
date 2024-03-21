/*
 * Copyright (c) 2023 Steven Becker
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

use log::{debug, log_enabled};

use crate::common::Part;

use super::{Instruction, LabelRecog, MacroInstr, Operation, Reg};

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

fn split_list(instr_list: &mut Vec<Operation>, pointer: &usize) -> (Vec<Operation>, Vec<Operation>) {
    let mut right_list = instr_list.split_off(*pointer);
    if !right_list.is_empty() {
        right_list.remove(0);
    }
    (right_list, vec![])
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
                                Operation::LablMacro(labl, MacroInstr::Auipc(reg.to_owned(), targ_labl.clone()))),
                None => instr_list.insert(*pointer, MacroInstr::Auipc(reg.to_owned(), targ_labl.clone()).into())
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
                                Operation::LablMacro(labl, MacroInstr::Auipc(Reg::G1, targ_labl.clone()))),
                None => instr_list.insert(*pointer, MacroInstr::Auipc(Reg::G1, targ_labl.clone()).into())
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
                                Operation::LablMacro(labl, MacroInstr::Auipc(Reg::G6, targ_labl.clone()))),
                None => instr_list.insert(*pointer, MacroInstr::Auipc(Reg::G6, targ_labl.clone()).into())
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
        MacroInstr::SbLabl(_, reg, targ_labl) => {
            match label {
                Some(labl) => instr_list.insert(*pointer - 1,
                                Operation::LablMacro(labl, MacroInstr::Auipc(reg.to_owned(), targ_labl.clone()))),
                None => instr_list.insert(*pointer - 1, MacroInstr::Auipc(reg.to_owned(), targ_labl.clone()).into())
            }
            *pointer += 1;

            debug!("Expanded '{macro_in}' at {} into '[{}; {}]'", *pointer - 1, instr_list[*pointer-1], instr_list[*pointer]);

            *accumulator += 1;
        },
        MacroInstr::ShLabl(_, reg, targ_labl) => {
            match label {
                Some(labl) => instr_list.insert(*pointer - 1,
                                Operation::LablMacro(labl, MacroInstr::Auipc(reg.to_owned(), targ_labl.clone()))),
                None => instr_list.insert(*pointer - 1, MacroInstr::Auipc(reg.to_owned(), targ_labl.clone()).into())
            }
            *pointer += 1;

            debug!("Expanded '{macro_in}' at {} into '[{}; {}]'", *pointer - 1, instr_list[*pointer-1], instr_list[*pointer]);

            *accumulator += 1;
        },
        MacroInstr::SwLabl(_, reg, targ_labl) => {
            match label {
                Some(labl) => instr_list.insert(*pointer - 1,
                                Operation::LablMacro(labl, MacroInstr::Auipc(reg.to_owned(), targ_labl.clone()))),
                None => instr_list.insert(*pointer - 1, MacroInstr::Auipc(reg.to_owned(), targ_labl.clone()).into())
            }
            *pointer += 1;

            debug!("Expanded '{macro_in}' at {} into '[{}; {}]'", *pointer - 1, instr_list[*pointer-1], instr_list[*pointer]);

            *accumulator += 1;
        },
        MacroInstr::SbImm(reg1, reg2, imm) => {
            instr_list.remove(*pointer);
            let mut imm_used = *imm;

            handle_multiline_immediate(&mut imm_used, label.clone(), pointer, instr_list, &Instruction::Sb(reg1.to_owned(), reg2.to_owned(), *imm));

            match label {
                Some(labl) => instr_list.insert(*pointer,
                                Operation::LablInstr(labl, Instruction::Auipc(reg2.to_owned(), imm_used))),
                None => instr_list.insert(*pointer, Instruction::Auipc(reg2.to_owned(), imm_used).into()),
            }
            *pointer += 1;
            instr_list.insert(*pointer,
            Instruction::Sb(reg1.to_owned(), reg2.to_owned(), imm_used).into());

            debug!("Expanded '{macro_in}' at {} into '[{}; {}]'", *pointer - 1, instr_list[*pointer-1], instr_list[*pointer]);

            *pointer += 1;
            *accumulator += 1;
        },
        MacroInstr::ShImm(reg1, reg2, imm) => {
            instr_list.remove(*pointer);
            let mut imm_used = *imm;

            handle_multiline_immediate(&mut imm_used, label.clone(), pointer, instr_list, &Instruction::Sh(reg1.to_owned(), reg2.to_owned(), *imm));

            match label {
                Some(labl) => instr_list.insert(*pointer,
                                Operation::LablInstr(labl, Instruction::Auipc(reg2.to_owned(), imm_used))),
                None => instr_list.insert(*pointer, Instruction::Auipc(reg2.to_owned(), imm_used).into()),
            }
            *pointer += 1;
            instr_list.insert(*pointer,
            Instruction::Sh(reg1.to_owned(), reg2.to_owned(), imm_used).into());

            debug!("Expanded '{macro_in}' at {} into '[{}; {}]'", *pointer - 1, instr_list[*pointer-1], instr_list[*pointer]);

            *pointer += 1;
            *accumulator += 1;
        },
        MacroInstr::SwImm(reg1, reg2, imm) => {
            instr_list.remove(*pointer);
            let mut imm_used = *imm;

            handle_multiline_immediate(&mut imm_used, label.clone(), pointer, instr_list, &Instruction::Sw(reg1.to_owned(), reg2.to_owned(), *imm));

            match label {
                Some(labl) => instr_list.insert(*pointer,
                                Operation::LablInstr(labl, Instruction::Auipc(reg2.to_owned(), imm_used))),
                None => instr_list.insert(*pointer, Instruction::Auipc(reg2.to_owned(), imm_used).into()),
            }
            *pointer += 1;
            instr_list.insert(*pointer,
            Instruction::Sw(reg1.to_owned(), reg2.to_owned(), imm_used).into());

            debug!("Expanded '{macro_in}' at {} into '[{}; {}]'", *pointer - 1, instr_list[*pointer-1], instr_list[*pointer]);

            *pointer += 1;
            *accumulator += 1;
        },
        MacroInstr::LbLabl(reg1, reg2, targ_labl, part) => {
            instr_list.remove(*pointer);

            if let Part::Lower = part {
                match label {
                    Some(labl) => instr_list.insert(*pointer,
                                    Operation::LablMacro(labl, MacroInstr::LbLabl(*reg1, *reg2, targ_labl.clone(), Part::None))),
                    None => instr_list.insert(*pointer, MacroInstr::LbLabl(*reg1, *reg2, targ_labl.clone(), Part::None).into()),
                }
                return;
            }

            match label {
                Some(labl) => instr_list.insert(*pointer,
                                Operation::LablMacro(labl, MacroInstr::Auipc(*reg2, targ_labl.clone()))),
                None => instr_list.insert(*pointer, MacroInstr::Auipc(*reg2, targ_labl.clone()).into()),
            }
            *pointer += 1;
            instr_list.insert(*pointer,
            MacroInstr::LbLabl(reg1.to_owned(), reg2.to_owned(), targ_labl.clone(), Part::Lower).into());

            debug!("Expanded '{macro_in}' at {} into '[{}; {}]'", *pointer - 1, instr_list[*pointer-1], instr_list[*pointer]);

            *pointer += 1;
            *accumulator += 1;
        },
        MacroInstr::LhLabl(reg1, reg2, targ_labl, part) => {
            instr_list.remove(*pointer);

            if let Part::Lower = part {
                match label {
                    Some(labl) => instr_list.insert(*pointer,
                                    Operation::LablMacro(labl, MacroInstr::LhLabl(*reg1, *reg2, targ_labl.clone(), Part::None))),
                    None => instr_list.insert(*pointer, MacroInstr::LhLabl(*reg1, *reg2, targ_labl.clone(), Part::None).into()),
                }
                return;
            }

            match label {
                Some(labl) => instr_list.insert(*pointer,
                                Operation::LablMacro(labl, MacroInstr::Auipc(*reg2, targ_labl.clone()))),
                None => instr_list.insert(*pointer, MacroInstr::Auipc(*reg2, targ_labl.clone()).into()),
            }
            *pointer += 1;
            instr_list.insert(*pointer,
            MacroInstr::LhLabl(reg1.to_owned(), reg2.to_owned(), targ_labl.clone(), Part::Lower).into());

            debug!("Expanded '{macro_in}' at {} into '[{}; {}]'", *pointer - 1, instr_list[*pointer-1], instr_list[*pointer]);

            *pointer += 1;
            *accumulator += 1;
        },
        MacroInstr::LwLabl(reg1, reg2, targ_labl, part) => {
            instr_list.remove(*pointer);

            if let Part::Lower = part {
                match label {
                    Some(labl) => instr_list.insert(*pointer,
                                    Operation::LablMacro(labl, MacroInstr::LwLabl(*reg1, *reg2, targ_labl.clone(), Part::None))),
                    None => instr_list.insert(*pointer, MacroInstr::LwLabl(*reg1, *reg2, targ_labl.clone(), Part::None).into()),
                }
                return;
            }

            match label {
                Some(labl) => instr_list.insert(*pointer,
                                Operation::LablMacro(labl, MacroInstr::Auipc(*reg2, targ_labl.clone()))),
                None => instr_list.insert(*pointer, MacroInstr::Auipc(*reg2, targ_labl.clone()).into()),
            }
            *pointer += 1;
            instr_list.insert(*pointer,
            MacroInstr::LwLabl(reg1.to_owned(), reg2.to_owned(), targ_labl.clone(), Part::Lower).into());

            debug!("Expanded '{macro_in}' at {} into '[{}; {}]'", *pointer - 1, instr_list[*pointer-1], instr_list[*pointer]);

            *pointer += 1;
            *accumulator += 1;
        },
        MacroInstr::LbuLabl(reg1, reg2, targ_labl) => {
            instr_list.remove(*pointer);

            match label {
                Some(labl) => instr_list.insert(*pointer,
                                Operation::LablMacro(labl, MacroInstr::Auipc(*reg2, targ_labl.clone()))),
                None => instr_list.insert(*pointer, MacroInstr::Auipc(*reg2, targ_labl.clone()).into()),
            }
            *pointer += 1;
            instr_list.insert(*pointer,
            MacroInstr::LbuLabl(reg1.to_owned(), reg2.to_owned(), targ_labl.clone()).into());

            debug!("Expanded '{macro_in}' at {} into '[{}; {}]'", *pointer - 1, instr_list[*pointer-1], instr_list[*pointer]);

            *pointer += 1;
            *accumulator += 1;
        },
        MacroInstr::LhuLabl(reg1, reg2, targ_labl) => {
            instr_list.remove(*pointer);

            match label {
                Some(labl) => instr_list.insert(*pointer,
                                Operation::LablMacro(labl, MacroInstr::Auipc(*reg2, targ_labl.clone()))),
                None => instr_list.insert(*pointer, MacroInstr::Auipc(*reg2, targ_labl.clone()).into()),
            }
            *pointer += 1;
            instr_list.insert(*pointer,
            MacroInstr::LhuLabl(reg1.to_owned(), reg2.to_owned(), targ_labl.clone()).into());

            debug!("Expanded '{macro_in}' at {} into '[{}; {}]'", *pointer - 1, instr_list[*pointer-1], instr_list[*pointer]);

            *pointer += 1;
            *accumulator += 1;
        },
        MacroInstr::LbImm(reg1, reg2, imm) => {
            instr_list.remove(*pointer);
            let mut imm_used = *imm;

            handle_multiline_immediate(&mut imm_used, label.clone(), pointer, instr_list, &Instruction::Lb(reg1.to_owned(), reg2.to_owned(), *imm));

            match label {
                Some(labl) => instr_list.insert(*pointer,
                                Operation::LablInstr(labl, Instruction::Auipc(reg2.to_owned(), imm_used))),
                None => instr_list.insert(*pointer, Instruction::Auipc(reg2.to_owned(), imm_used).into()),
            }
            *pointer += 1;
            instr_list.insert(*pointer,
            Instruction::Lb(reg1.to_owned(), reg2.to_owned(), imm_used).into());

            debug!("Expanded '{macro_in}' at {} into '[{}; {}]'", *pointer - 1, instr_list[*pointer-1], instr_list[*pointer]);

            *pointer += 1;
            *accumulator += 1;
        },
        MacroInstr::LhImm(reg1, reg2, imm) => {
            instr_list.remove(*pointer);
            let mut imm_used = *imm;

            handle_multiline_immediate(&mut imm_used, label.clone(), pointer, instr_list, &Instruction::Lh(reg1.to_owned(), reg2.to_owned(), *imm));

            match label {
                Some(labl) => instr_list.insert(*pointer,
                                Operation::LablInstr(labl, Instruction::Auipc(reg2.to_owned(), imm_used))),
                None => instr_list.insert(*pointer, Instruction::Auipc(reg2.to_owned(), imm_used).into()),
            }
            *pointer += 1;
            instr_list.insert(*pointer,
            Instruction::Lh(reg1.to_owned(), reg2.to_owned(), imm_used).into());

            debug!("Expanded '{macro_in}' at {} into '[{}; {}]'", *pointer - 1, instr_list[*pointer-1], instr_list[*pointer]);

            *pointer += 1;
            *accumulator += 1;
        },
        MacroInstr::LwImm(reg1, reg2, imm) => {
            instr_list.remove(*pointer);
            let mut imm_used = *imm;

            handle_multiline_immediate(&mut imm_used, label.clone(), pointer, instr_list, &Instruction::Lw(reg1.to_owned(), reg2.to_owned(), *imm));

            match label {
                Some(labl) => instr_list.insert(*pointer,
                                Operation::LablInstr(labl, Instruction::Auipc(reg2.to_owned(), imm_used))),
                None => instr_list.insert(*pointer, Instruction::Auipc(reg2.to_owned(), imm_used).into()),
            }
            *pointer += 1;
            instr_list.insert(*pointer,
            Instruction::Lw(reg1.to_owned(), reg2.to_owned(), imm_used).into());

            debug!("Expanded '{macro_in}' at {} into '[{}; {}]'", *pointer - 1, instr_list[*pointer-1], instr_list[*pointer]);

            *pointer += 1;
            *accumulator += 1;
        },
        MacroInstr::LbuImm(reg1, reg2, imm) => {
            instr_list.remove(*pointer);
            let mut imm_used = *imm;

            handle_multiline_immediate(&mut imm_used, label.clone(), pointer, instr_list, &Instruction::Lbu(reg1.to_owned(), reg2.to_owned(), *imm));

            match label {
                Some(labl) => instr_list.insert(*pointer,
                                Operation::LablInstr(labl, Instruction::Auipc(reg2.to_owned(), imm_used))),
                None => instr_list.insert(*pointer, Instruction::Auipc(reg2.to_owned(), imm_used).into()),
            }
            *pointer += 1;
            instr_list.insert(*pointer,
            Instruction::Lbu(reg1.to_owned(), reg2.to_owned(), imm_used).into());

            debug!("Expanded '{macro_in}' at {} into '[{}; {}]'", *pointer - 1, instr_list[*pointer-1], instr_list[*pointer]);

            *pointer += 1;
            *accumulator += 1;
        },
        MacroInstr::LhuImm(reg1, reg2, imm) => {
            instr_list.remove(*pointer);
            let mut imm_used = *imm;

            handle_multiline_immediate(&mut imm_used, label.clone(), pointer, instr_list, &Instruction::Lhu(reg1.to_owned(), reg2.to_owned(), *imm));

            match label {
                Some(labl) => instr_list.insert(*pointer,
                                Operation::LablInstr(labl, Instruction::Auipc(reg2.to_owned(), imm_used))),
                None => instr_list.insert(*pointer, Instruction::Auipc(reg2.to_owned(), imm_used).into()),
            }
            *pointer += 1;
            instr_list.insert(*pointer,
            Instruction::Lhu(reg1.to_owned(), reg2.to_owned(), imm_used).into());

            debug!("Expanded '{macro_in}' at {} into '[{}; {}]'", *pointer - 1, instr_list[*pointer-1], instr_list[*pointer]);

            *pointer += 1;
            *accumulator += 1;
        },
        _ => *pointer += 1,
    }
}

pub fn expand_instrs(symbol_map: &mut LabelRecog, instr_list: &mut Vec<Operation>) {
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

#[cfg(test)]
mod tests {
    use super::*;

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
                Operation::LablMacro(label, MacroInstr::Auipc(Reg::G7, target_label.into())),
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
                Operation::LablMacro(label, MacroInstr::Auipc(Reg::G1, target_label.into())),
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
                Operation::LablMacro(label, MacroInstr::Auipc(Reg::G6, target_label.into())),
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
}