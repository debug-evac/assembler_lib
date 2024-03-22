/*
 * Copyright (c) 2023 Steven Becker
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

use std::{cell::RefCell, collections::HashMap, sync::OnceLock};

pub(super) struct Symbols;

#[allow(clippy::thread_local_initializer_can_be_made_const)]
impl Symbols {
    //#[thread_local]
    thread_local! {
        static ARRAY: OnceLock<RefCell<HashMap<smartstring::alias::String, i128>>> = OnceLock::new();
    }

    pub(super) fn symbols_write(symbol: smartstring::alias::String, def: i128) {
        Symbols::ARRAY.with(|x| {
            let map = x.get_or_init(|| RefCell::from(HashMap::new()));
            map.borrow_mut().insert(symbol, def);
        })
    }
    
    pub(super) fn symbols_read(symbol: &smartstring::alias::String) -> Option<i128> {
        Symbols::ARRAY.with(|x| {
            let map = x.get_or_init(|| RefCell::from(HashMap::new()));
            map.borrow().get(symbol).copied()
        })
    }
    
    pub(super) fn symbols_clear() {
        Symbols::ARRAY.with(|x| {
            let map = x.get_or_init(|| RefCell::from(HashMap::new()));
            map.borrow_mut().clear();
        })
    }
}