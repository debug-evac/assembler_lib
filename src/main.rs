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

use clap::{
    Arg, 
    Command, 
    value_parser, 
    ArgAction
};
use parser::Subroutines;
use std::{
    io::Write,
    fs,
    fs::File,
    path::{
        PathBuf,
        Path
    },
};

use crate::common::{LabelRecog, Operation};

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

    let mut subroutines = Subroutines::new();

    for val in string_vector.as_slice() {
        let val = parser::parse(&val, &mut Some(&mut subroutines));
        match val {
            Ok(val) => parsed_vector.push(val.1),
            Err(_) => panic!("Parser error"),
        }
    }

    let sub_code = subroutines.get_code();
    for code in sub_code.as_slice() {
        let val = parser::parse(&code, &mut None);
        match val {
            Ok(res) => parsed_vector.push(res.1),
            Err(_) => (),
        }
    }

    let linked_vector = match linker::link(parsed_vector) {
        Ok(linked) => linked,
        Err(mes) => panic!("[Error] could not link assembly files: {:?}", mes),
    };

    let optimized_code = optimizer::optimize(linked_vector);
    let translated_code = translator::translate(optimized_code);

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

    let filename = match assembler_output.get_one::<PathBuf>("destination"){
        None => Path::new("./a.bin"),
        Some(assembler_output) => assembler_output,
    };

    let mut output_file = match File::create(filename) {
        Ok(file) => file,
        Err(msg) => panic!("[Error] could not create output file: {}", msg),
    };

    output_file.write_all(&translated_code).expect("[Error] could not write to output file!");

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
 