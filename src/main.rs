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
mod common;

use clap::{Arg, Command};
use clap::value_parser;
use clap::ArgAction;
use std::path::Path;
use std::{fs, path::PathBuf};
use std::fs::File;

use crate::common::{LabelRecog, Operation};

// TODO
pub static MUL_SUB: &str = r#"
"#;
pub static DIV_SUB: &str = r#"
"#;

pub static SRR_SUB: &str = r#"
_SRR:
    sub a4, zero, a1
    srl a2, a0, a1
    sll a3, a0, a4
    or a0, a2, a3
    ret
"#;
pub static SLR_SUB: &str = r#"
_SLR:
    sub a4, zero, a1
    sll a2, a0, a1
    srl a3, a0, a4
    or a0, a2, a3
    ret
"#;

fn main() {
    //let src = "addi $3, $3, 1";
    let matches = Command::new("Assembler Input")
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
    
    let mut parsed_vector: Vec<(LabelRecog, Vec<Operation>)> = vec![];

    let mut string_vector: Vec<String> = vec![];

    for file in vals {
        let contents = fs::read_to_string(file.as_path());
        match contents {
            Ok(val) => string_vector.push(val),
            Err(_) => panic!("File error"),
        };
    }

    for val in string_vector.as_slice() {
        let val = parser::parse(&val);
        match val {
            Ok(val) => parsed_vector.push(val.1),
            Err(_) => panic!("Parser error"),
        }
    }

    /*let linked_vector: (LabelRecog, Vec<Operation>);
        let res = linker::link(parsed_vector);
        match res{
            Ok(linked) => linked_vector  = linked,
            Err(mes) => println!("could not link the assembler files"),
        }*/



    let assembler_output = Command::new("Assembler Output")
    .arg(Arg::new("destination")
                .value_parser(value_parser!(PathBuf))
                .action(clap::ArgAction::Append)
                .short('d')
                .num_args(1..=10)
                .required(false)
                .long("destination")
                .help("The destination for the output file"))
    .get_matches();

    let mut filename;
    match assembler_output.get_one::<PathBuf>("destination"){
        None => filename = Path::new("./a.bin"),
        Some(assembler_output) => filename = assembler_output,
    }
    let f = File::create(filename);
    //f.write_all(.as_byteArray());  Translator ausgabe muss eingefügt werden
     
     
     
     
     
     
     
     
     
     
     
         /*
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
 */
     
 
}
 