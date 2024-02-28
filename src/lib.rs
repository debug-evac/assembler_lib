/*
 * Copyright (c) 2023 Steven Becker
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

mod parser;
mod linker;
mod optimizer;
pub mod translator;
pub mod common;

use crate::common::errors::LibraryError;

use common::TranslatableCode;
use indicatif::ProgressBar;

#[cfg(feature = "python_lib")]
use std::path::PathBuf;
#[cfg(feature = "python_lib")]
use pyo3::{exceptions::PyRuntimeError, prelude::*};

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

    pub fn parse_link_optimize(self) -> Result<TranslatableCode, LibraryError> {
        if self.assembly_code.is_empty() {
            return Err(LibraryError::NoCode)
        }

        let mut parsed_vector = Vec::with_capacity(self.assembly_code.len());
        let mut subroutine = parser::Subroutines::new();

        for (counter, code) in self.assembly_code.iter().enumerate() {
            parsed_vector.push(parser::parse(code, &mut Some(&mut subroutine), counter == 0 && self.sp_init)?.1);
        }

        for code in subroutine.get_code() {
            parsed_vector.push(parser::parse(&code, &mut None, false)?.1)
        }

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
/// * `outpath` - The path of the output file as Path (default: "a.mif")
/// * `comment` - If the output file should be commented, as boolean (default: false)
///               Does not do anything, if format != "mif"
/// * `depth` - Memory depth of mif file as Int (default: 1024)
///             Does not do anything, if format != "mif"
/// * `width` - Memory width of mif file as Int (default: 8)
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
    outpath=PathBuf::from("a.mif"),
    comment=false,
    depth=1024,
    width=8
))]
fn assemble(
    assembly_code: Vec<String>,
    sp_init: bool,
    no_nop_insert: bool,
    format: &str,
    outpath: PathBuf,
    comment: bool,
    depth: u16,
    width: u8
) -> PyResult<()> {
    let mut builder = ParseLinkBuilder::new()
                                                .sp_init(sp_init)
                                                .no_nop_insert(no_nop_insert);

    for code in assembly_code {
        builder.add_code(code);
    }

    let translate_code = builder.parse_link_optimize()?;

    translator::translate_and_present(&outpath, translate_code, comment, format, (depth, width))?;
    
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
    outpath=PathBuf::from("a.mif"),
    comment=false,
    depth=1024,
    width=8
))]
fn assemble_paths(
    assembly_paths: Vec<PathBuf>,
    sp_init: bool,
    no_nop_insert: bool,
    format: &str,
    outpath: PathBuf,
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
        outpath,
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