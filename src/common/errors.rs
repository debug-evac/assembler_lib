/*
 * Copyright (c) 2023 Steven Becker
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

//
// This module defines errors that are used across the modules.

#[cfg(feature = "python_lib")]
use pyo3::{PyErr, exceptions::PyRuntimeError};

pub use asm_core_lib::errors::CommonError;
use std::num::TryFromIntError;

use crate::common::LabelElem;
use crate::common::MacroInstr;

pub trait ExitErrorCode {
    fn get_err_code(&self) -> i32 {
        1
    }
}

impl ExitErrorCode for CommonError {
    fn get_err_code(&self) -> i32 {
        65
    }
}

#[derive(Debug)]
pub enum LinkError {
    InsertRecog(CommonError),
    UndefinedGlobal(LabelElem)
}

impl std::fmt::Display for LinkError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LinkError::InsertRecog(source) => write!(f, "Could not insert LabelRecog into Namespace: {source}"),
            LinkError::UndefinedGlobal(labelel) => write!(f, "Global label '{}' is not defined!", labelel.get_name())
        }
    }
}

impl From<CommonError> for LinkError {
    fn from(under: CommonError) -> Self {
        LinkError::InsertRecog(under)
    }
}

impl ExitErrorCode for LinkError {
    fn get_err_code(&self) -> i32 {
        65
    }
}

#[derive(Debug)]
pub enum OptimizerError {
    LabelNonExistent(smartstring::alias::String),
    LabelSubNotRequiredFor(MacroInstr),
    LabelTooFar(TryFromIntError)
}

impl std::fmt::Display for OptimizerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OptimizerError::LabelNonExistent(label) => write!(f, "Label '{}' is not existent!", label),
            OptimizerError::LabelSubNotRequiredFor(macro_in) => write!(f, "Label substitution not required for Macro '{:?}'", macro_in),
            OptimizerError::LabelTooFar(std_err) => write!(f, "{std_err}"),
        }
    }
}

impl From<TryFromIntError> for OptimizerError {
    fn from(value: TryFromIntError) -> Self {
        OptimizerError::LabelTooFar(value)
    }
}

impl ExitErrorCode for OptimizerError {
    fn get_err_code(&self) -> i32 {
        65
    }
}

#[derive(Debug)]
pub enum TranslatorError {
    DepthNotFit(usize, usize),
    UndefinedWidth(u8),
    IOError(std::io::Error),
    UndefinedFormat(String)
}

impl ExitErrorCode for TranslatorError {
    fn get_err_code(&self) -> i32 {
        match self {
            TranslatorError::DepthNotFit(_, _) => 65,
            TranslatorError::UndefinedWidth(_) => 64,
            TranslatorError::IOError(_) => 73, // or 74 or 1
            TranslatorError::UndefinedFormat(_) => 64, 
        }
    }
}

impl From<std::io::Error> for TranslatorError {
    fn from(value: std::io::Error) -> Self {
        TranslatorError::IOError(value)
    }
}

impl std::fmt::Display for TranslatorError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TranslatorError::DepthNotFit(got, req) => write!(f, "Not enough addresses! {got} < {req}"),
            TranslatorError::UndefinedWidth(width) => write!(f, "Width {width} is not defined. Try using 8 or 32!"),
            TranslatorError::IOError(e) => write!(f, "{}", e),
            TranslatorError::UndefinedFormat(format) => write!(f, "Format '{format}' is not defined. Try one of [\"mif\", \"raw\", \"debug\"]."),
        }
    }
}

#[cfg(feature = "python_lib")]
impl std::convert::From<TranslatorError> for PyErr {
    fn from(err: TranslatorError) -> PyErr {
        PyRuntimeError::new_err(err.to_string())
    }
}

#[derive(Debug)]
pub enum LibraryError {
    NoCode,
    ParserError(String),
    LinkerError(LinkError),
    OptimizerError(OptimizerError),
}

impl From<LinkError> for LibraryError {
    fn from(value: LinkError) -> Self {
        LibraryError::LinkerError(value)
    }
}

impl From<OptimizerError> for LibraryError {
    fn from(value: OptimizerError) -> Self {
        LibraryError::OptimizerError(value)
    }
}

impl <I: std::fmt::Debug> From<nom::Err<nom::error::Error<I>>> for LibraryError {
    fn from(value: nom::Err<nom::error::Error<I>>) -> Self {
        LibraryError::ParserError(format!("{value}"))
    }
}

impl std::fmt::Display for LibraryError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LibraryError::NoCode => write!(f, "No code has been specified!"),
            LibraryError::ParserError(nom_err) => write!(f, "{nom_err}"),
            LibraryError::LinkerError(link_err) => write!(f, "{link_err}"),
            LibraryError::OptimizerError(opt_err) => write!(f, "{opt_err}"),
        }
    }
}

impl ExitErrorCode for LibraryError {
    fn get_err_code(&self) -> i32 {
        match self {
            LibraryError::NoCode => 1,
            LibraryError::ParserError(_) => 65,
            LibraryError::LinkerError(link_err) => link_err.get_err_code(),
            LibraryError::OptimizerError(opt_err) => opt_err.get_err_code(),
        }
    }
}

#[cfg(feature = "python_lib")]
impl std::convert::From<LibraryError> for PyErr {
    fn from(err: LibraryError) -> PyErr {
        PyRuntimeError::new_err(err.to_string())
    }
}
pub enum ParserError {
    NoTextSection,
    CommonError(CommonError)
}

impl ParserError {
    pub fn get_nom_err_text(&self) -> &'static str {
        match self {
            ParserError::NoTextSection => "Specified .data section without .text section!",
            ParserError::CommonError(_) => "Label is already defined!",
        }
    }
}

impl From<CommonError> for ParserError {
    fn from(value: CommonError) -> Self {
        ParserError::CommonError(value)
    }
}

impl std::fmt::Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParserError::NoTextSection => write!(f, "Specified .data section without .text section!"),
            ParserError::CommonError(com_err) => write!(f, "{com_err}"),
        }
    }
}