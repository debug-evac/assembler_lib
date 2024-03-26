/*
 * Copyright (c) 2023 Steven Becker
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

pub(crate) mod parser;
pub(crate) mod linker;
pub(crate) mod optimizer;
pub mod translator;

use crate::common::errors::LibraryError;

use crate::common::{AssemblyCode, AssemblyCodeRecog, Instruction, LabelRecog, Reg, TranslatableCode};
use indicatif::ProgressBar;

#[cfg(feature = "python_lib")]
use std::path::PathBuf;
#[cfg(feature = "python_lib")]
use pyo3::{exceptions::PyRuntimeError, prelude::*};

use self::parser::Subroutines;

#[derive(Default)]
pub struct ParseLinkBuilder<'a> {
    assembly_code: Vec<String>,
    sp_init: bool,
    no_nop_insert: bool,
    progbar: Option<&'a ProgressBar>,
}

impl <'a> ParseLinkBuilder<'a> {
    pub fn new() -> Self {
        ParseLinkBuilder { assembly_code: vec![], sp_init: false, no_nop_insert: false, progbar: None }
    }

    pub fn progbar(mut self, progbar: &'a ProgressBar) -> Self {
        self.progbar = Some(progbar);
        self
    }

    pub fn add_code(&mut self, code: String) {
        self.assembly_code.push(code)
    }

    pub fn sp_init(mut self, sp_init: bool) -> Self {
        self.sp_init = sp_init;
        self
    }

    pub fn no_nop_insert(mut self, no_nop_insert: bool) -> Self {
        self.no_nop_insert = no_nop_insert;
        self
    }

    #[inline]
    fn parse(&self) -> Result<Vec<AssemblyCodeRecog>, LibraryError> {
        let mut parsed_vector = Vec::with_capacity(self.assembly_code.len());

        if self.sp_init {
            let mut assembly = AssemblyCodeRecog::new(LabelRecog::new());
            let text = assembly.get_text_refmut();
            text.push(Instruction::Lui(Reg::G2, 4096).into());
            parsed_vector.push(assembly);
        }

        for code in self.assembly_code.iter() {
            parsed_vector.push(code.parse::<AssemblyCodeRecog>()?);
        }

        let subroutine = parsed_vector.iter_mut().fold(Subroutines::new(), |mut acc, x| {
            acc.merge(x.get_subroutine_refmut());
            acc
        });

        for code in subroutine.get_code() {
            let mut assembly = AssemblyCodeRecog::new(LabelRecog::new());
            *assembly.get_text_refmut() = parser::text_parse(&mut code.as_str(), &mut None, assembly.get_labels_refmut())?;
            parsed_vector.push(assembly)
        }

        Ok(parsed_vector)
    }

    pub fn parse_link_optimize(self) -> Result<TranslatableCode, LibraryError> {
        if self.assembly_code.is_empty() {
            return Err(LibraryError::NoCode)
        }

        let parsed_vector = self.parse()?;

        if let Some(progbar) = self.progbar {
            progbar.inc(1);
            progbar.set_message("Linking...");
        };

        let unoptimized = linker::link(parsed_vector)?;

        if let Some(progbar) = self.progbar {
            progbar.inc(1);
            progbar.set_message("Optimizing...");
        };

        Ok(optimizer::optimize(unoptimized, self.no_nop_insert)?)
    }
}

#[cfg(feature = "python_lib")]
/// Assembles the `assembly_code` to files of `format`.
///
/// # Arguments
///
/// * `assembly_code` - Strings of assembly code in a List
/// * `sp_init` - Stack pointer initialization as boolean (default: true)
/// * `no_nop_insert` - If nop insertions should be done, as boolean (default: false)
/// * `format` - The output format as String (default: "mif")
/// * `text_outpath` - The path of the output file for the text section as Path (default: "a.mif")
/// * `data_outpath` - The path of the output file for the data section as Path (default: "a.mem.mif")
/// * `comment` - If the output file should be commented, as boolean (default: false)
///               Does not do anything, if format != "mif"
/// * `depth` - Memory depth of mif file as Int (default: 1024)
///             Does not do anything, if format != "mif"
/// * `width` - Memory width of mif file as Int (default: 32)
///             Does not do anything, if format != "mif"
///
/// # Returns
///
/// If there is an error, a RuntimeError is returned. Otherwise nothing is returned.
#[pyfunction]
#[pyo3(signature = (
    assembly_code,
    sp_init=true, 
    no_nop_insert=false, 
    format="mif",
    text_outpath=PathBuf::from("a.mif"),
    data_outpath=PathBuf::from("a.mem.mif"),
    comment=false,
    depth=1024,
    width=32
))]
fn assemble(
    assembly_code: Vec<String>,
    sp_init: bool,
    no_nop_insert: bool,
    format: &str,
    text_outpath: PathBuf,
    data_outpath: PathBuf,
    comment: bool,
    depth: u16,
    width: u8
) -> PyResult<()> {
    use crate::{asm::translator::{CodeWriter, RawFormat, WordWidth}, common::errors::TranslatorError};

    use self::translator::MifFormat;

    let mut builder = ParseLinkBuilder::new()
                                                .sp_init(sp_init)
                                                .no_nop_insert(no_nop_insert);

    for code in assembly_code {
        builder.add_code(code);
    }

    let translate_code = builder.parse_link_optimize()?;

    match format {
        "mif" => {
            let width_num = match width {
                8 => WordWidth::EightBit,
                32 => WordWidth::ThirtyTwoBit,
                num => return Err(TranslatorError::UndefinedWidth(num).into()),
            };
            let mif_format = MifFormat::default().set_comment(comment).set_mem_len(depth).set_word_len(width_num);
            let code_writer = CodeWriter::new(mif_format, translate_code);
            code_writer.write_files(text_outpath, data_outpath)
        },
        "raw" => {
            let code_writer = CodeWriter::new(RawFormat, translate_code);
            code_writer.write_files(text_outpath, data_outpath)
        },
        "dat" => return Err(TranslatorError::UndefinedFormat("dat".to_string()).into()),
        form => return Err(TranslatorError::UndefinedFormat(form.to_string()).into()),
    }?;
    
    Ok(())
}

#[cfg(feature = "python_lib")]
/// Convenience function that reads Assembly Code from files and then assembles them.
/// Uses `assemble` internally. See `assemble` function for details.
///
/// Only difference to `assemble` function is that `assembly_paths` is a List of Paths.
#[pyfunction]
#[pyo3(signature = (
    assembly_paths,
    sp_init=true, 
    no_nop_insert=true, 
    format="mif",
    text_outpath=PathBuf::from("a.mif"),
    data_outpath=PathBuf::from("a.mem.mif"),
    comment=false,
    depth=1024,
    width=32
))]
fn assemble_paths(
    assembly_paths: Vec<PathBuf>,
    sp_init: bool,
    no_nop_insert: bool,
    format: &str,
    text_outpath: PathBuf,
    data_outpath: PathBuf,
    comment: bool,
    depth: u16,
    width: u8
) -> PyResult<()> {
    let mut assembly_code: Vec<String> = Vec::with_capacity(assembly_paths.len());

    for file in assembly_paths {
        match std::fs::read_to_string(file.as_path()) {
            Ok(val) => assembly_code.push(val),
            Err(msg) => return Err(PyRuntimeError::new_err(msg.to_string())),
        };
    }

    assemble(
        assembly_code, 
        sp_init,
        no_nop_insert,
        format,
        text_outpath,
        data_outpath,
        comment,
        depth,
        width,
    )
}

#[cfg(feature = "python_lib")]
#[pymodule]
fn assembler_lib(_py: Python, m: &PyModule) -> PyResult<()> {
    m.add_function(wrap_pyfunction!(assemble, m)?)?;
    m.add_function(wrap_pyfunction!(assemble_paths, m)?)?;
    Ok(())
}
