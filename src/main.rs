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

use crate::common::{LabelRecog, Operation, present_error};

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
    .arg(Arg::new("format-depth")
                .value_name("address count")
                .value_hint(ValueHint::Other)
                .value_parser(value_parser!(u16).range(1..65536))
                .long("depth")
                .action(ArgAction::Set)
                .num_args(1)
                .default_value("1024")
                .help("Depth for MIF format. Does not do anything, if format != mif.")
    )
    .arg(Arg::new("format-width")
                .value_name("word width in bits")
                .value_hint(ValueHint::Other)
                .value_parser(["8", "32"])
                .long("width")
                .action(ArgAction::Set)
                .num_args(1)
                .default_value("32")
                .help("Width for MIF format. Does not do anything, if format != mif.")
    )
    .arg(Arg::new("nop_insert")
                .long("no-nop-insertion")
                .action(ArgAction::SetTrue)
                .required(false)
                .help("Disallow nop insertion")
    )
    .get_matches()
}

fn write_to_file(output: &PathBuf, translated_code: &Vec<u8>, format: &str, size: (u16, u8)) -> Result<(), String> {
    let mut output_file = match File::create(output) {
        Ok(file) => file,
        Err(msg) => return Err(format!("Could not create file!\n{msg}")),
    };

    match format {
        "mif" => {
            let depth = size.0;
            let width = size.1;

            writeln!(output_file, "DEPTH = {depth};\nWIDTH = {width};\nADDRESS_RADIX = DEC;\nDATA_RADIX = BIN;\nCONTENT\nBEGIN").unwrap();

            if width == 32 {
                if (depth as usize) < translated_code.len() {
                    return Err(format!("MIF depth is smaller than the instruction count! {} < {}", depth, translated_code.len()));
                }
                for (counter, values) in translated_code.chunks_exact(4).enumerate() {
                    writeln!(output_file, "{counter}\t: {:08b}{:08b}{:08b}{:08b};", values[0], values[1], values[2], values[3]).unwrap();
                }
            } else if width == 8 {
                if ((depth as usize) / 4) < translated_code.len() {
                    return Err(format!("MIF depth is smaller than the instruction count! {} < {}", (depth / 4), translated_code.len()));
                }
                for (counter, value) in translated_code.iter().enumerate() {
                    writeln!(output_file, "{counter}\t: {:08b};", value).unwrap();
                }
            } else {
                return Err(format!("Expected width of either 8 or 32 bits, got '{width}'!"));
            }
            
            writeln!(output_file, "END;").unwrap();
        },
        "raw" => {
            if let Err(msg) = output_file.write_all(translated_code) {
                return Err(format!("Could not write to output file!\n{msg}"));
            }
        },
        _ => unreachable!(),
    }
    Ok(())
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

    // always returns Some(_)
    let outfmt = matches.get_one::<String>("format").unwrap();
    let outpath = matches.get_one::<PathBuf>("output").unwrap();
    let depth = matches.get_one("format-depth").unwrap();
    let width = str::parse::<u8>(matches.get_one::<String>("format-width").unwrap()).unwrap();

    if let Err(msg) = write_to_file(outpath, &translated_code, outfmt, (*depth, width)) {
        present_error(msg);
    }
}
 