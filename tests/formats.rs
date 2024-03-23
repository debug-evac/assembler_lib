/*
 * Copyright (c) 2023 Steven Becker
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

use assembler_lib::asm::{translator::{self, MifFormat, WordWidth}, ParseLinkBuilder};

use predicates::prelude::*;
use assert_fs::prelude::*;

#[test]
fn test_mif_format_32() -> Result<(), Box<dyn std::error::Error>> {
    let temp = assert_fs::TempDir::new()?;

    let output_path = temp.child("a.mif");

    let input = r#"
rep 3, nop
"#;

    let cor_output = temp.child("correct_output.mif");

    let mut cor_instr_vec: Vec<u8> = vec![];

    let instr_vec: Vec<u32> = Vec::from([
        0b00_00000_00000_00000_00100_01001_10111, // Lui - sp init
        0b00_00000_00000_00000_00000_00000_10011, // nop
        0b00_00000_00000_00000_00000_00000_10011, // nop
        0b00_00000_00000_00000_00000_00000_10011  // nop
    ]);

    for instr in instr_vec.iter() {
        cor_instr_vec.extend(instr.to_le_bytes());
    }

    let mut output_str = "DEPTH = 1024;\nWIDTH = 32;\nADDRESS_RADIX = DEC;\nDATA_RADIX = BIN;\nCONTENT\nBEGIN\n".to_string();

    for (counter, values) in cor_instr_vec.chunks_exact(4).enumerate() {
        output_str.push_str(&format!("{counter}\t: {:08b}{:08b}{:08b}{:08b};\n", values[0], values[1], values[2], values[3]));
    }

    output_str.push_str("END;\n");

    cor_output.write_str(&output_str)?;

    let mut parser_builder = ParseLinkBuilder::new()
        .sp_init(true);
    parser_builder.add_code(input.to_string());

    let assembly_code = parser_builder.parse_link_optimize().unwrap();
    let code_writer = translator::CodeWriter::new(MifFormat::default(), assembly_code);

    code_writer.write_text_file(output_path.path())?;

    output_path
        .assert(predicate::path::is_file())
        .assert(predicate::path::eq_file(cor_output.path()));

    temp.close()?;

    Ok(())
}

#[test]
fn test_mif_format_8() -> Result<(), Box<dyn std::error::Error>> {
    let temp = assert_fs::TempDir::new()?;

    let output_path = temp.child("a.mif");

    let input = r#"
rep 3, nop
"#;

    let cor_output = temp.child("correct_output.mif");

    let instr_vec: Vec<u32> = Vec::from([
        0b00_00000_00000_00000_00100_01001_10111, // Lui - sp init
        0b00_00000_00000_00000_00000_00000_10011, // nop
        0b00_00000_00000_00000_00000_00000_10011, // nop
        0b00_00000_00000_00000_00000_00000_10011  // nop
    ]);

    let mut output_str = "DEPTH = 1024;\nWIDTH = 8;\nADDRESS_RADIX = DEC;\nDATA_RADIX = BIN;\nCONTENT\nBEGIN\n".to_string();

    let mut counter: usize = 0;

    for instr in instr_vec.iter() {
        let mach_instr = instr.to_le_bytes();
        output_str.push_str(&format!("{counter}\t: {:08b} {:08b} {:08b} {:08b};\n", mach_instr[0], mach_instr[1], mach_instr[2], mach_instr[3]));
        counter += 4;
    }

    output_str.push_str("END;\n");

    cor_output.write_str(&output_str)?;

    let mut parser_builder = ParseLinkBuilder::new()
        .sp_init(true);
    parser_builder.add_code(input.to_string());

    let assembly_code = parser_builder.parse_link_optimize().unwrap();

    let mif_format = MifFormat::default().set_word_len(WordWidth::EightBit);
    let code_writer = translator::CodeWriter::new(mif_format, assembly_code);

    code_writer.write_text_file(output_path.path())?;

    output_path
        .assert(predicate::path::is_file())
        .assert(predicate::path::eq_file(cor_output.path()));

    temp.close()?;

    Ok(())
}

#[test]
fn test_mif_format_32_cmt() -> Result<(), Box<dyn std::error::Error>> {
    let temp = assert_fs::TempDir::new()?;

    let output_path = temp.child("a.mif");

    let input = r#"
rep 3, nop
"#;

    let cor_output = temp.child("correct_output.mif");

    let mut cor_instr_vec: Vec<u8> = vec![];

    let instr_vec: Vec<u32> = Vec::from([
        0b00_00000_00000_00000_00100_01001_10111, // Lui - sp init
        0b00_00000_00000_00000_00000_00000_10011, // nop
        0b00_00000_00000_00000_00000_00000_10011, // nop
        0b00_00000_00000_00000_00000_00000_10011  // nop
    ]);

    for instr in instr_vec.iter() {
        cor_instr_vec.extend(instr.to_le_bytes());
    }

    let mut output_str = "DEPTH = 1024;\nWIDTH = 32;\nADDRESS_RADIX = DEC;\nDATA_RADIX = BIN;\nCONTENT\nBEGIN\n".to_string();

    for (counter, values) in cor_instr_vec.chunks_exact(4).enumerate() {
        if counter == 0 {
            output_str.push_str(&format!("{counter}\t: {:08b}{:08b}{:08b}{:08b};\t\t-- {}\n", values[0], values[1], values[2], values[3], "lui sp, 1"));
        } else {
            output_str.push_str(&format!("{counter}\t: {:08b}{:08b}{:08b}{:08b};\t\t-- {}\n", values[0], values[1], values[2], values[3], "addi zero, zero, 0"));
        }
    }

    output_str.push_str("END;\n");

    cor_output.write_str(&output_str)?;

    let mut parser_builder = ParseLinkBuilder::new()
        .sp_init(true);
    parser_builder.add_code(input.to_string());

    let assembly_code = parser_builder.parse_link_optimize().unwrap();

    let mif_format = MifFormat::default().set_comment(true);
    let code_writer = translator::CodeWriter::new(mif_format, assembly_code);

    code_writer.write_text_file(output_path.path())?;

    output_path
        .assert(predicate::path::is_file())
        .assert(predicate::path::eq_file(cor_output.path()));

    temp.close()?;

    Ok(())
}

#[test]
fn test_mif_format_8_cmt() -> Result<(), Box<dyn std::error::Error>> {
    let temp = assert_fs::TempDir::new()?;

    let output_path = temp.child("a.mif");

    let input = r#"
rep 3, nop
"#;

    let cor_output = temp.child("correct_output.mif");

    let instr_vec: Vec<u32> = Vec::from([
        0b00_00000_00000_00000_00100_01001_10111, // Lui - sp init
        0b00_00000_00000_00000_00000_00000_10011, // nop
        0b00_00000_00000_00000_00000_00000_10011, // nop
        0b00_00000_00000_00000_00000_00000_10011  // nop
    ]);

    let mut output_str = "DEPTH = 1024;\nWIDTH = 8;\nADDRESS_RADIX = DEC;\nDATA_RADIX = BIN;\nCONTENT\nBEGIN\n".to_string();

    let mut counter: usize = 0;

    for instr in instr_vec.iter() {
        let mach_instr = instr.to_le_bytes();
        if counter == 0 {
            output_str.push_str(&format!("{counter}\t: {:08b} {:08b} {:08b} {:08b};\t\t-- {}\n", mach_instr[0], mach_instr[1], mach_instr[2], mach_instr[3], "lui sp, 1"));
        } else {
            output_str.push_str(&format!("{counter}\t: {:08b} {:08b} {:08b} {:08b};\t\t-- {}\n", mach_instr[0], mach_instr[1], mach_instr[2], mach_instr[3], "addi zero, zero, 0"));
        }
        counter += 4;
    }

    output_str.push_str("END;\n");

    cor_output.write_str(&output_str)?;

    let mut parser_builder = ParseLinkBuilder::new()
        .sp_init(true);
    parser_builder.add_code(input.to_string());

    let assembly_code = parser_builder.parse_link_optimize().unwrap();

    let mif_format = MifFormat::default().set_comment(true).set_word_len(WordWidth::EightBit);
    let code_writer = translator::CodeWriter::new(mif_format, assembly_code);

    code_writer.write_text_file(output_path.path())?;

    output_path
        .assert(predicate::path::is_file())
        .assert(predicate::path::eq_file(cor_output.path()));

    temp.close()?;

    Ok(())
}

#[test]
fn test_debug_format() -> Result<(), Box<dyn std::error::Error>> {
    let temp = assert_fs::TempDir::new()?;

    let no_output_path = temp.child("a.mif");

    let input = r#"
rep 3, nop
"#;

    let instr_vec: Vec<u32> = Vec::from([
        0b00_00000_00000_00000_00100_01001_10111, // Lui - sp init
        0b00_00000_00000_00000_00000_00000_10011, // nop
        0b00_00000_00000_00000_00000_00000_10011, // nop
        0b00_00000_00000_00000_00000_00000_10011  // nop
    ]);

    let mut output_str = "DEPTH = 1024;\nWIDTH = 8;\nADDRESS_RADIX = DEC;\nDATA_RADIX = BIN;\nCONTENT\nBEGIN\n".to_string();

    let mut counter: usize = 0;

    for instr in instr_vec.iter() {
        let mach_instr = instr.to_le_bytes();
        output_str.push_str(&format!("{counter}\t: {:08b} {:08b} {:08b} {:08b};\t\t-- {}\n", mach_instr[0], mach_instr[1], mach_instr[2], mach_instr[3], "Addi(G0, G0, 0)"));
        counter += 4;
    }

    output_str.push_str("END;\n");

    let mut parser_builder = ParseLinkBuilder::new()
        .sp_init(true);
    parser_builder.add_code(input.to_string());

    let assembly_code = parser_builder.parse_link_optimize().unwrap();

    let mif_format = MifFormat::default().set_comment(true).set_word_len(WordWidth::EightBit);
    let code_writer = translator::CodeWriter::new(mif_format, assembly_code);

    code_writer.write_text_stdout()?;

    no_output_path
        .assert(predicate::path::missing());

    temp.close()?;

    Ok(())
}