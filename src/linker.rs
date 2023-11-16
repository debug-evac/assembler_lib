/*
 * Copyright (c) 2023 Steven Becker
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

//
// Links multiple sources that have been parsed together to one source.
// Will require some rewrites of the parser to retrieve symbol maps and
// symbol res map. Might also require parser to not replace labels with
// absolute addresses.
// Linked files need to be specified and ordered from flags or a
// particular file (probably flags only, but we'll see).
use crate::parser::LabelRecog;

pub struct LinkError {

}
