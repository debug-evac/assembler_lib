/*
 * Copyright (c) 2023 Steven Becker
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

//
// This module defines the data structure for directives.

use std::fmt::Display;

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