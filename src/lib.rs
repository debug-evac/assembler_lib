/*
 * Copyright (c) 2023 Steven Becker
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

pub mod parser;
pub mod linker;
pub mod optimizer;
pub mod translator;
pub mod common;

use crate::common::errors::LibraryError;
use crate::common::AssemblyCode;
use crate::linker::Namespaces;

#[derive(Default)]
pub struct ParseLinkBuilder {
    assembly_code: Vec<String>
}

impl ParseLinkBuilder {
    pub fn new() -> Self {
        ParseLinkBuilder { assembly_code: vec![] }
    }

    pub fn add_code(&mut self, code: String) {
        self.assembly_code.push(code)
    }

    pub fn parse_link(self, sp_init: bool) -> Result<AssemblyCode<Namespaces>, LibraryError> {
        // Currently builder cannot be destroyed since result borrows from vec of strings
        if self.assembly_code.is_empty() {
            return Err(LibraryError::NoCode)
        }

        let mut parsed_vector = Vec::with_capacity(self.assembly_code.len());
        let mut subroutine = parser::Subroutines::new();

        {
          for (counter, code) in self.assembly_code.iter().enumerate() {
              parsed_vector.push(parser::parse(code, &mut Some(&mut subroutine), counter == 0 && sp_init)?.1);
          }
        }

        for code in subroutine.get_code() {
            parsed_vector.push(parser::parse(&code, &mut None, false)?.1)
        }

        Ok(linker::link(parsed_vector)?)
    }
}
