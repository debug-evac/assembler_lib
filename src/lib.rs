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
