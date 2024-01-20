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
    ArgAction,
    ArgMatches
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

fn cli_interface() -> ArgMatches {
    Command::new("Assembler Input")
    .arg(Arg::new("input assembly files")
                .value_parser(value_parser!(PathBuf))
                .action(ArgAction::Set)
                .short('i')
                .num_args(1..=10)
                .required(true)
                .long("input")
                .help("Input assembly files, use \"<PATH>\""))
    .arg(Arg::new("destination binary file")
                .value_parser(value_parser!(PathBuf))
                .action(ArgAction::Set)
                .short('d')
                .num_args(1)
                .required(false)
                .long("destination")
                .help("The destination for the output file"))
    .get_matches()
}

fn main() {
    let matches = cli_interface();

    let vals: Vec<&PathBuf> = matches.get_many::<PathBuf>("input assembly files")
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
        let val = parser::parse(val, &mut Some(&mut subroutines));
        match val {
            Ok(val) => parsed_vector.push(val.1),
            Err(_) => panic!("Parser error"),
        }
    }

    let sub_code = subroutines.get_code();
    for code in sub_code.as_slice() {
        let val = parser::parse(code, &mut None);
        if let Ok(res) = val {
            parsed_vector.push(res.1)
        }
    }

    let linked_vector = match linker::link(parsed_vector) {
        Ok(linked) => linked,
        Err(mes) => panic!("[Error] could not link assembly files: {:?}", mes),
    };

    let optimized_code = optimizer::optimize(linked_vector);
    let translated_code = translator::translate(optimized_code);

    let outpath = match matches.get_one::<PathBuf>("destination binary file"){
        None => Path::new("./a.bin"),
        Some(assembler_output) => assembler_output,
    };

    let mut output_file = match File::create(outpath) {
        Ok(file) => file,
        Err(msg) => panic!("[Error] could not create output file: {}", msg),
    };

    output_file.write_all(&translated_code).expect("[Error] could not write to output file!");
}
 