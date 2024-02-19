/*
 * Copyright (c) 2023 Steven Becker
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

//
// This module defines errors that are used across the modules.

use crate::common::LabelElem;
use crate::common::MacroInstr;

pub trait ExitErrorCode {
    fn get_err_code(&self) -> i32 {
        1
    }
}

#[derive(Debug)]
pub enum CommonError {
    LabelsNameNotEqual(LabelElem, LabelElem),
    MultipleGlobalDefined(LabelElem),
    LabelAlreadyDefined(LabelElem),
}

impl std::fmt::Display for CommonError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CommonError::LabelsNameNotEqual(labelel_a, labelel_b) => write!(f, "Cannot compare different label names '{}' and '{}'", labelel_a.get_name(), labelel_b.get_name()),
            CommonError::MultipleGlobalDefined(labelel) => write!(f, "Global label '{}' defined multiple times!", labelel.get_name()),
            CommonError::LabelAlreadyDefined(labelel) => write!(f, "Label '{}' already defined!", labelel.get_name()),
        }
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
    LabelNonExistent(String),
    LabelSubNotRequiredFor(MacroInstr)
}

impl std::fmt::Display for OptimizerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OptimizerError::LabelNonExistent(label) => write!(f, "Label '{}' is not existent!", label),
            OptimizerError::LabelSubNotRequiredFor(macro_in) => write!(f, "Label substitution not required for Macro '{:?}'", macro_in),
        }
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
}

impl ExitErrorCode for TranslatorError {
    fn get_err_code(&self) -> i32 {
        match self {
            TranslatorError::DepthNotFit(_, _) => 65,
            TranslatorError::UndefinedWidth(_) => 64,
            TranslatorError::IOError(_) => 73, // or 74 or 1
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
        }
    }
}
