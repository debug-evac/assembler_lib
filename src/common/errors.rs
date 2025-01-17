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
use winnow::error::ContextError;

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
#[non_exhaustive]
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
#[non_exhaustive]
pub enum OptimizerError {
    LabelNonExistent(smartstring::alias::String),
    LabelSubNotRequiredFor(MacroInstr),
    LabelTooFar(TryFromIntError),
    JumpTooFar(MacroInstr, usize, i32, i32) // Macro, line, difference, must be equal or under
}

impl std::fmt::Display for OptimizerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OptimizerError::LabelNonExistent(label) => write!(f, "Label '{}' is not existent!", label),
            OptimizerError::LabelSubNotRequiredFor(macro_in) => write!(f, "Label substitution not required for Macro '{:?}'", macro_in),
            OptimizerError::LabelTooFar(std_err) => write!(f, "{std_err}"),
            OptimizerError::JumpTooFar(macro_in, cur_line, dif, under_equal) => 
            write!(f, "[Operation #{cur_line}] Jump in '{macro_in}' too far away: {dif} > {under_equal}!"),
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
#[non_exhaustive]
pub enum TranslatorError {
    DepthNotFit(usize, usize),
    UndefinedWidth(u8),
    IOError(std::io::Error),
    UndefinedFormat(String)
}

impl std::error::Error for TranslatorError {}

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
#[non_exhaustive]
pub enum LibraryError {
    NoCode,
    #[deprecated(since = "2.1.0", note = "Parsing error is used instead of ParserError")]
    ParserError(String),
    ParsingError {
        file: usize, 
        err: String
    },
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

impl <I: std::fmt::Debug + std::clone::Clone> From<winnow::error::ErrMode<winnow::error::ContextError<I>>> for LibraryError {
    fn from(value: winnow::error::ErrMode<winnow::error::ContextError<I>>) -> Self {
        #[allow(deprecated)]
        LibraryError::ParserError(format!("{value}"))
    }
}

impl From<winnow::error::ParseError<&str, ContextError>> for LibraryError {
    fn from(value: winnow::error::ParseError<&str, ContextError>) -> Self {
        #[allow(deprecated)]
        LibraryError::ParserError(format!("{value}"))
    }
}

impl From<(usize, winnow::error::ParseError<&str, ContextError>)> for LibraryError {
    fn from((file, value): (usize, winnow::error::ParseError<&str, ContextError>)) -> Self {
        LibraryError::ParsingError{file, err: format!("{value}")}
    }
}

impl <I: std::fmt::Debug + std::clone::Clone> From<(usize, winnow::error::ErrMode<winnow::error::ContextError<I>>)> for LibraryError {
    fn from((file, value): (usize, winnow::error::ErrMode<winnow::error::ContextError<I>>)) -> Self {
        #[allow(deprecated)]
        LibraryError::ParsingError{ file, err: format!("{value}")}
    }
}

impl std::fmt::Display for LibraryError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LibraryError::NoCode => write!(f, "No code has been specified!"),
            LibraryError::ParsingError{err: nom_err, ..} => write!(f, "\n{nom_err}"),
            #[allow(deprecated)]
            LibraryError::ParserError(nom_err) => write!(f, "\n{nom_err}"),
            LibraryError::LinkerError(link_err) => write!(f, "{link_err}"),
            LibraryError::OptimizerError(opt_err) => write!(f, "{opt_err}"),
        }
    }
}

impl ExitErrorCode for LibraryError {
    fn get_err_code(&self) -> i32 {
        match self {
            LibraryError::NoCode => 1,
            #[allow(deprecated)]
            LibraryError::ParserError(_) => 65,
            LibraryError::ParsingError{ .. } => 65,
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

#[derive(Debug)]
#[non_exhaustive]
pub enum ParserError {
    NoTextSection,
    CommonError(CommonError),
    LockNotWritable(smartstring::alias::String),
    NumberTooLarge(TryFromIntError),
    NestedRepeat,
}

impl std::error::Error for ParserError {}

impl ParserError {
    pub fn get_nom_err_text(&self) -> &'static str {
        match self {
            ParserError::NoTextSection => "Specified .data section without .text section!",
            ParserError::CommonError(_) => "Label is already defined!",
            ParserError::LockNotWritable(_) => "Could not emit label into symbol map!",
            ParserError::NumberTooLarge(_) => "Number is too large!",
            ParserError::NestedRepeat => "Nested repeats are not allowed!",
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
            ParserError::NoTextSection => write!(f, "{}", self.get_nom_err_text()),
            ParserError::CommonError(com_err) => write!(f, "{com_err}"),
            ParserError::LockNotWritable(labl) => write!(f, "Could not emit label '{labl}' into symbol map!"),
            ParserError::NumberTooLarge(std_err) => write!(f, "{std_err}"),
            ParserError::NestedRepeat => write!(f, "{}", self.get_nom_err_text()),
        }
    }
}

impl From<TryFromIntError> for ParserError {
    fn from(value: TryFromIntError) -> Self {
        ParserError::NumberTooLarge(value)
    }
}

#[derive(Debug)]
#[non_exhaustive]
pub enum CommonError {
    LabelsNameNotEqual(LabelElem, LabelElem),
    MultipleGlobalDefined(LabelElem),
    LabelAlreadyDefined(LabelElem),
    TooManyInstructions(TryFromIntError),
    RegisterUnknownNum(usize),
    RegisterUnknownStr(String),
}

impl std::fmt::Display for CommonError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CommonError::LabelsNameNotEqual(labelel_a, labelel_b) => write!(f, "Cannot compare different label names '{}' and '{}'", labelel_a.get_name(), labelel_b.get_name()),
            CommonError::MultipleGlobalDefined(labelel) => write!(f, "Global label '{}' defined multiple times!", labelel.get_name()),
            CommonError::LabelAlreadyDefined(labelel) => write!(f, "Label '{}' already defined!", labelel.get_name()),
            CommonError::TooManyInstructions(std_err) => write!(f, "{std_err}"),
            CommonError::RegisterUnknownNum(al_reg) => write!(f, "Register x{al_reg} does not exist!"),
            CommonError::RegisterUnknownStr(al_reg) => write!(f, "Register '{al_reg}' does not exist!"),
        }
    }
}

impl std::error::Error for CommonError {}

impl From<TryFromIntError> for CommonError {
    fn from(value: TryFromIntError) -> Self {
        CommonError::TooManyInstructions(value)
    }
}