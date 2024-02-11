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
use indicatif_log_bridge::LogWrapper;
use log::{log_enabled, error};
use parser::Subroutines;
use std::{
    fs,
    path::PathBuf,
};
use indicatif::{MultiProgress, ProgressBar, ProgressStyle};
use console::Term;

use crate::common::{LabelRecog, AssemblyCode, errors::ExitErrorCode};

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
                .value_parser(["mif", "raw", "debug"])
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
    .arg(Arg::new("comment_mif")
                .long("comment")
                .short('c')
                .action(ArgAction::SetTrue)
                .required(false)
                .help("Comment mif with used instructions. Does not do anything, if format != mif.")
    )
    .get_matches()
}

fn main() {
    let logger =
        env_logger::Builder::from_env(
            env_logger::Env::default().default_filter_or("info"))
            .format_timestamp(None)
            .build();

    let multi = MultiProgress::new();

    LogWrapper::new(multi.clone(), logger)
        .try_init()
        .unwrap();

    let matches = cli_interface();

    let vals: Vec<&PathBuf> = matches.get_many::<PathBuf>("input")
    .unwrap()
    .collect();

    let mut parsed_vector: Vec<AssemblyCode<LabelRecog>> = vec![];
    let mut string_vector: Vec<String> = vec![];

    let progbar;

    if !log_enabled!(log::Level::Info) {
        progbar = ProgressBar::hidden();
    } else {
        progbar = multi.add(ProgressBar::new(5));
        progbar.set_style(
            ProgressStyle::with_template(
                if Term::stdout().size().1 > 80 {
                    "{prefix:>12.cyan.bold} [{bar:57}] {pos}/{len} {wide_msg}"
                } else {
                    "{prefix:>12.cyan.bold} [{bar:57}] {pos}/{len}"
                },
            )
            .unwrap()
            .progress_chars("=> "),
        );
    }
    progbar.set_prefix("Assembling");
    progbar.set_message("Reading assembly files...");

    for file in vals {
        match fs::read_to_string(file.as_path()) {
            Ok(val) => string_vector.push(val),
            Err(msg) => {
                error!("Could not read '{}': {}", file.as_path().to_string_lossy(), msg);
                std::process::exit(66)
            },
        };
    }

    progbar.inc(1);
    progbar.set_message("Parsing...");

    let mut subroutines = Subroutines::new();

    for val in string_vector.as_slice() {
        match parser::parse(val, &mut Some(&mut subroutines)) {
            Ok(val) => parsed_vector.push(val.1),
            Err(e) => {
                error!("{e}");
                std::process::exit(65)
            },
        }
    }

    let sub_code = subroutines.get_code();
    for code in sub_code.as_slice() {
        let val = parser::parse(code, &mut None);
        if let Ok(res) = val {
            parsed_vector.push(res.1)
        }
    }

    progbar.inc(1);
    progbar.set_message("Linking...");

    let linked_vector = match linker::link(parsed_vector) {
        Ok(linked) => linked,
        Err(e) => {
            error!("{e}");
            std::process::exit(e.get_err_code())
        },
    };

    progbar.inc(1);
    progbar.set_message("Optimizing...");

    let no_nop_insert = matches.get_flag("nop_insert");

    let translatable_code = match optimizer::optimize(linked_vector, no_nop_insert) {
        Ok(instr_list) => instr_list,
        Err(e) => {
            error!("{e}");
            std::process::exit(e.get_err_code())
        }
    };

    progbar.inc(1);
    progbar.set_message("Translating & Writing...");

    // always returns Some(_)
    let outfmt = matches.get_one::<String>("format").unwrap();
    let outpath = matches.get_one::<PathBuf>("output").unwrap();
    let comment = matches.get_flag("comment_mif");
    let depth = matches.get_one("format-depth").unwrap();
    let width = str::parse::<u8>(matches.get_one::<String>("format-width").unwrap()).unwrap();

    if let Err(e) = translator::translate_and_present(outpath, translatable_code, comment, outfmt, (*depth, width)) {
        error!("{e}");
        std::process::exit(e.get_err_code())
    };

    let line = if outfmt != "debug" {
        format!(
            "{:>12} {} ({})",
            console::Style::new().bold().apply_to("Assembled"),
            outpath.as_path().file_name().unwrap().to_str().unwrap(),
            std::fs::canonicalize(outpath.as_path()).unwrap().parent().unwrap().to_str().unwrap()
        )
    } else {
        format!(
            "{:>12} {}",
            console::Style::new().bold().apply_to("Assembled"),
            "Printed to console"
        )
    };

    progbar.println(line);
    progbar.set_prefix("Finished");
    progbar.finish_with_message("Success");
}
 
