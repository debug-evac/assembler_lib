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
    builder::ArgPredicate,
    Arg, 
    Command, 
    value_parser, 
    ArgAction,
    ArgMatches,
    crate_authors, crate_version, crate_description, ValueHint
};
use parser::Subroutines;
use std::{
    io::Write,
    fs,
    fs::File,
    path::PathBuf,
};

use crate::common::{LabelRecog, Operation};

fn cli_interface() -> ArgMatches {
    #[allow(non_upper_case_globals)]
    Command::new("Assembler")
    .author(crate_authors!(", "))
    .version(crate_version!())
    .about(crate_description!())
    .help_template("\
{before-help}{name} - {version}
by {author-with-newline}{about-with-newline}
{usage-heading} {usage}

{all-args}{after-help}

Copyright: MPL-2.0 (https://mozilla.org/MPL/2.0/)
")
    .arg(Arg::new("format")
                .value_hint(ValueHint::Other)
                .value_parser(["mif", "raw"])
                .action(ArgAction::Set)
                .short('f')
                .num_args(1)
                .default_value("mif")
                .required(false)
                .long("format")
                .help("The format in which the output should be written in")
    )
    .arg(Arg::new("input")
                .value_names(["main asm file", "another asm file"])
                .value_hint(ValueHint::FilePath)
                .value_parser(value_parser!(PathBuf))
                .action(ArgAction::Set)
                .short('i')
                .num_args(1..=10)
                .required(true)
                .long("input")
                .help("Input assembly files, use \"<PATH>\"")
    )
    .arg(Arg::new("output")
                .value_name("output bin file")
                .value_hint(ValueHint::FilePath)
                .value_parser(value_parser!(PathBuf))
                .action(ArgAction::Set)
                .short('o')
                .num_args(1)
                .default_value("a.bin")
                .default_value_if("format", ArgPredicate::Equals("raw".into()), Some("a.bin"))
                .default_value_if("format", ArgPredicate::Equals("mif".into()), Some("a.mif"))
                .required(false)
                .long("output")
                .help("The destination for the output file")
    )
    .arg(Arg::new("nop_insert")
                .long("no-nop-insertion")
                .action(ArgAction::SetTrue)
                .required(false)
                .help("Disallow nop insertion. Currently not respected!")
    )
    .get_matches()
}

fn write_to_file(output: &PathBuf, translated_code: &Vec<u8>, format: &str) {
    let mut output_file = match File::create(output) {
        Ok(file) => file,
        Err(msg) => panic!("[Error] could not create output file: {}", msg),
    };

    match format {
        "mif" => {
            let depth = translated_code.len() / 4;
            let width = 32;

            write!(output_file, "DEPTH = {depth};\nWIDTH = {width};\nADDRESS_RADIX = DEC;\nDATA_RADIX = BIN;\nCONTENT\nBEGIN\n").unwrap();

            for (counter, values) in translated_code.chunks_exact(4).enumerate() {
                write!(output_file, "{counter}\t: {:08b}{:08b}{:08b}{:08b};\n", values[0], values[1], values[2], values[3]).unwrap();
            }

            write!(output_file, "END;\n").unwrap();
        },
        "raw" => output_file.write_all(&translated_code).expect("[Error] could not write to output file!"),
        _ => unreachable!(),
    }
}

fn main() {
    let matches = cli_interface();

    let vals: Vec<&PathBuf> = matches.get_many::<PathBuf>("input")
    .expect("At least one assembly input file is required")
    .collect();
    
    let mut parsed_vector: Vec<(LabelRecog, Vec<Operation>)> = vec![];
    let mut string_vector: Vec<String> = vec![];

    for file in vals {
        match fs::read_to_string(file.as_path()) {
            Ok(val) => string_vector.push(val),
            Err(msg) => panic!("[Error] Could not read a file: {}", msg),
        };
    }

    let mut subroutines = Subroutines::new();

    for val in string_vector.as_slice() {
        match parser::parse(val, &mut Some(&mut subroutines)) {
            Ok(val) => parsed_vector.push(val.1),
            Err(msg) => panic!("[Error] Parser error: {}", msg),
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

    let no_nop_insert = matches.get_flag("nop_insert");

    let optimized_code = optimizer::optimize(linked_vector, no_nop_insert);
    let translated_code = translator::translate(optimized_code);

    let outfmt = matches.get_one::<String>("format").unwrap();

    // always returns Some(_)
    let outpath = matches.get_one::<PathBuf>("output").unwrap();

    write_to_file(outpath, &translated_code, &outfmt);
}
 