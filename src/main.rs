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

use clap::{Arg, Command};
use clap::value_parser;
use clap::ArgAction;
use std::{fs, path::PathBuf};

use crate::parser::Instruction;

fn main() {
    //let src = "addi $3, $3, 1";
    let matches = Command::new("Test")
    .arg(Arg::new("file")
                .value_parser(value_parser!(PathBuf))
                .action(ArgAction::Set)
                 .short('f')
                 .num_args(1..=10)
                 .required(true)
                 .long("file")
                 .help("A cool file, use \"...\""))
    .get_matches();

    let vals: Vec<&PathBuf> = matches.get_many::<PathBuf>("file")
    .expect("`file`is required")
    .collect();
    
    let parsed_vector: Vec<(LabelRecog, Vec<Operation>)>;

    for file in vals {
        let contents = fs::read_to_string(file.as_path());
        match contents {
            Ok(val) => {
                let res: Result<(&str, Vec<Instruction>), nom::Err<nom::error::Error<&str>>> = parser::parse(&val);
                match res {
                    Ok(parsed) => parsed_vector.push(parsed),
                    Err(mes) => println!("{}", mes),
                }
            },
            Err(mes) => println!("could not find file"),
        };
    }
let myfile: Vec<&PathBuf> = matches.get_many::<PathBuf>("file")
.expect("files are needed!")
.collect();
println!("{:?}", myfile); 

    
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
    }

    

}
