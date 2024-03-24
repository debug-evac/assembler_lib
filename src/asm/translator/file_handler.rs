/*
 * Copyright (c) 2023 Steven Becker
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

use std::{fs::File, path::{Path, PathBuf}};

use crate::common::{errors::TranslatorError, ByteData, DWordData, HalfData, Instruction, MemData, TranslatableCode, WordData};

use super::{set_data_path, Translate};

pub trait Translatable: std::fmt::Display {
    fn translate(&self) -> u32;
 }
 
 impl Translatable for Instruction {
    fn translate(&self) -> u32 {
       self.translate_instruction()
    }
 }
 
 impl Translatable for u32 {
    fn translate(&self) -> u32 {
       *self
    }
 }

pub(super) fn translate_data_to_word_vec(memdata: &[MemData]) -> Vec<u32> {
    let mut translatable_data: Vec<u8> = vec![];

    for data in memdata.iter() {
        match data {
            MemData::Bytes(data_vec, _) => {
                for byte in data_vec {
                    if let ByteData::Byte(num) = byte {
                        let bytes = num.to_le_bytes();
                        translatable_data.push(bytes[0]);
                    }
                }
            }
            MemData::Halfs(data_vec) => {
                for half in data_vec {
                    if let HalfData::Half(num) = half {
                        let bytes = num.to_le_bytes();
                        translatable_data.push(bytes[0]);
                        translatable_data.push(bytes[1]);
                    }
                }
            }
            MemData::Words(data_vec) => {
                for word in data_vec {
                    if let WordData::Word(num) = word {
                        let bytes = num.to_le_bytes();
                        for byte in bytes.iter().take(4) {
                            translatable_data.push(*byte);
                        }
                    }
                }
            }
            MemData::DWords(data_vec) => {
                for dword in data_vec {
                    if let DWordData::DWord(num) = dword {
                        let bytes = num.to_le_bytes();
                        for byte in bytes.iter().take(8) {
                            translatable_data.push(*byte);
                        }
                    }
                }
            }
            MemData::Namespace(_) => (),
        }
    }

    let mut word_vec: Vec<u32> = vec![];
    let mut iter = translatable_data.chunks_exact(4);

    for byte in iter.by_ref() {
        word_vec.push(
            ((byte[3] as u32) << 24)
                + ((byte[2] as u32) << 16)
                + ((byte[1] as u32) << 8)
                + (byte[0] as u32),
        );
    }

    let remainder = iter.remainder();

    match remainder.len() {
        0 => (),
        1 => word_vec.push((remainder[0] as u32) << 24),
        2 => word_vec.push(((remainder[1] as u32) << 24) + ((remainder[0] as u32) << 16)),
        3 => word_vec.push(
            ((remainder[2] as u32) << 24)
                + ((remainder[1] as u32) << 16)
                + ((remainder[0] as u32) << 8),
        ),
        _ => unreachable!(),
    }

    word_vec
}

#[derive(Clone, Copy, Debug, Default)]
pub enum WordWidth {
    EightBit,
    #[default]
    ThirtyTwoBit,
}

impl std::fmt::Display for WordWidth {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!(
            "{}",
            if let WordWidth::ThirtyTwoBit = self {
                32
            } else {
                8
            }
        ))
    }
}

pub trait Format {
    fn write<W: std::io::Write, T: Translatable>(
        &self,
        writer: &mut W,
        writable: &[T],
    ) -> Result<(), TranslatorError>;
}

pub struct RawFormat;
impl Format for RawFormat {
    fn write<W: std::io::Write, T: Translatable>(
        &self,
        writer: &mut W,
        writable: &[T],
    ) -> Result<(), TranslatorError> {
        let mut byte_instrs: Vec<u8> = vec![];

        for instr in writable.iter() {
            byte_instrs.extend(instr.translate().to_le_bytes());
        }
        writer.write_all(&byte_instrs)?;
        Ok(())
    }
}

pub struct MifFormat {
    comment: bool,
    mem_len: u16,
    word_len: WordWidth,
}
impl Format for MifFormat {
    fn write<W: std::io::Write, T: Translatable>(
        &self,
        writer: &mut W,
        writable: &[T],
    ) -> Result<(), TranslatorError> {
        let depth = self.mem_len;
        let width = self.word_len;

        let (sep, added) = if let WordWidth::ThirtyTwoBit = width {
            if (self.mem_len as usize) < writable.len() {
                return Err(TranslatorError::DepthNotFit(
                    self.mem_len as usize,
                    writable.len(),
                ));
            }
            ("", 1)
        } else {
            let addr_count = (depth as usize) / 4;
            if addr_count < writable.len() {
                return Err(TranslatorError::DepthNotFit(addr_count, writable.len()));
            }
            (" ", 4)
        };

        writeln!(writer, "DEPTH = {depth};\nWIDTH = {width};\nADDRESS_RADIX = DEC;\nDATA_RADIX = BIN;\nCONTENT\nBEGIN")?;

        let mut counter: usize = 0;
        if self.comment {
            for elem in writable.iter() {
                let mach_instr = elem.translate().to_le_bytes();
                writeln!(
                    writer,
                    "{counter}\t: {:08b}{sep}{:08b}{sep}{:08b}{sep}{:08b};\t\t-- {elem}",
                    mach_instr[0], mach_instr[1], mach_instr[2], mach_instr[3]
                )?;
                counter += added;
            }
        } else {
            for elem in writable.iter() {
                let mach_instr = elem.translate().to_le_bytes();
                writeln!(
                    writer,
                    "{counter}\t: {:08b}{sep}{:08b}{sep}{:08b}{sep}{:08b};",
                    mach_instr[0], mach_instr[1], mach_instr[2], mach_instr[3]
                )?;
                counter += added;
            }
        }

        writeln!(writer, "END;")?;

        Ok(())
    }
}

impl MifFormat {
    pub fn set_comment(mut self, comment: bool) -> Self {
        self.comment = comment;
        self
    }

    pub fn set_mem_len(mut self, mem_len: u16) -> Self {
        self.mem_len = mem_len;
        self
    }

    pub fn set_word_len(mut self, word_len: WordWidth) -> Self {
        self.word_len = word_len;
        self
    }

    pub fn set_args(&mut self, mem_len: u16, word_len: WordWidth, comment: bool) {
        self.mem_len = mem_len;
        self.word_len = word_len;
        self.comment = comment;
    }
}

impl Default for MifFormat {
    fn default() -> Self {
        Self {
            comment: false,
            mem_len: 1024,
            word_len: Default::default(),
        }
    }
}

struct DatFormat;
impl Format for DatFormat {
    fn write<W: std::io::Write, T: Translatable>(
        &self,
        _writer: &mut W,
        _writable: &[T],
    ) -> Result<(), TranslatorError> {
        unimplemented!()
    }
}

pub struct CodeWriter<T: Format> {
    text: Vec<Instruction>,
    data: Vec<u32>,
    format: T,
}

impl<T: Format> CodeWriter<T> {
    #[inline]
    fn write_text_to<W: std::io::Write>(&self, mut writer: W) -> Result<(), TranslatorError> {
        self.format.write(&mut writer, &self.text)
    }

    #[inline]
    fn write_data_to<W: std::io::Write>(&self, mut writer: W) -> Result<(), TranslatorError> {
        self.format.write(&mut writer, &self.data)
    }

    pub fn write_to<W: std::io::Write>(&self, mut writer: W) -> Result<(), TranslatorError> {
        self.write_text_to(&mut writer)?;
        self.write_data_to(&mut writer)
    }

    pub fn write_text_file<P: AsRef<Path>>(&self, text_output: P) -> Result<(), TranslatorError> {
        let mut text_file = File::create(text_output)?;
        self.write_text_to(&mut text_file)
    }

    pub fn write_data_file<P: AsRef<Path>>(&self, data_output: P) -> Result<(), TranslatorError> {
        let mut data_file = File::create(data_output)?;
        self.write_data_to(&mut data_file)
    }

    pub fn write_text_stdout(&self) -> Result<(), TranslatorError> {
        self.format.write(&mut std::io::stdout(), &self.text)?;
        Ok(())
    }

    #[deprecated = "Consider using write_files"]
    pub fn write_file(&self, output: &PathBuf) -> Result<(), TranslatorError> {
        let data_output = set_data_path(output, "mem.mif");
        self.write_files(output, &data_output)
    }

    pub fn write_files<P: AsRef<Path>>(
        &self,
        text_output: P,
        data_output: P,
    ) -> Result<(), TranslatorError> {
        self.write_text_file(text_output)?;

        if !self.data.is_empty() {
            self.write_data_file(data_output)?;
        }

        Ok(())
    }

    pub fn new(format: T, translate_code: TranslatableCode) -> CodeWriter<T> {
        let (text, data) = translate_code.decompose();
        CodeWriter {
            format,
            text,
            data: translate_data_to_word_vec(&data),
        }
    }
}
