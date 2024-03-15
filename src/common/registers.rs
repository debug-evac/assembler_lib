/*
 * Copyright (c) 2023 Steven Becker
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

//
// This module defines the register data structure.

use std::fmt::Display;

#[repr(u8)]
#[derive(Debug, Copy, Clone, PartialEq)]
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
