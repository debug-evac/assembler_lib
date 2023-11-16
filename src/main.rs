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
mod translator;

fn main() {
    //let src = "addi $3, $3, 1";
/*
    let source_code = r#"START:
    movu $3, 16
    movl $3, 16
    cmpe $3, $4
    bt END
    movu $4, 16
    movl $4, 16
    jmp START
END:
"#;

    let res = parser::parse("divi $10,$10, 51");
    match res {
        Ok(parsed) => println!("{:?}", parsed.1[0]),
        Err(mes) => println!("{}", mes),
    };

    let res = parser::parse(source_code);
    match res {
        Ok(parsed) => println!("{:?}", parsed.1),
        Err(_) => (),
    }*/
}
