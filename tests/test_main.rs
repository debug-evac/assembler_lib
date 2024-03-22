/*
 * Copyright (c) 2023 Steven Becker
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

use assembler_lib::{translator, ParseLinkBuilder};

use predicates::prelude::*;
use assert_fs::prelude::*;

#[test]
fn test_translate_test_file() -> Result<(), Box<dyn std::error::Error>> {
    let temp = assert_fs::TempDir::new()?;

    let input = r#"SmallExample: addi sp, zero, 0x300
li x25, 150006
push x25
pop x25
ret
"#;

    let output_path = temp.child("a.bin");

    let cor_output = temp.child("correct_output.bin");

    let mut cor_instr_vec: Vec<u8> = vec![];

    #[cfg(feature = "raw_nop")]
    let instr_vec: Vec<u32> = Vec::from([
        0b00_00000_00000_00000_00100_01001_10111, // Lui - sp init
        0b00_11000_00000_00000_00000_01000_10011,
        0b00_00000_00000_00100_10111_00101_10111, // Li - lui
        0b00_00000_00000_00000_00000_00000_10011, // 3x NOP
        0b00_00000_00000_00000_00000_00000_10011,
        0b00_00000_00000_00000_00000_00000_10011,
        0b10_01111_10110_11001_00011_00100_10011, // Li - addi
        0b11_11111_11100_00010_00000_01000_10011, // Push - addi -4
        0b00_00000_00000_00000_00000_00000_10011, // 3x NOP
        0b00_00000_00000_00000_00000_00000_10011,
        0b00_00000_00000_00000_00000_00000_10011,
        0b00_00000_11001_00010_11000_00001_00011,
        0b00_00000_00000_00010_01011_00100_00011, // Pop - Lw
        0b00_00000_00100_00010_00000_01000_10011, // Pop - addi +4
        0b00_00000_00000_00001_00000_00011_00111
    ]);

    #[cfg(not(feature = "raw_nop"))]
    let instr_vec: Vec<u32> = Vec::from([
        0b00_00000_00000_00000_00100_01001_10111, // Lui - sp init
        0b00_11000_00000_00000_00000_01000_10011,
        0b00_00000_00000_00100_10111_00101_10111, // Li - lui
        0b10_01111_10110_11001_00011_00100_10011, // Li - addi
        0b11_11111_11100_00010_00000_01000_10011, // Push - addi -4
        0b00_00000_11001_00010_11000_00001_00011,
        0b00_00000_00000_00010_01011_00100_00011, // Pop - Lw
        0b00_00000_00100_00010_00000_01000_10011,
        0b00_00000_00000_00001_00000_00011_00111
    ]);

    for instr in instr_vec.iter() {
        cor_instr_vec.extend(instr.to_le_bytes());
    }

    cor_output.write_binary(&cor_instr_vec)?;

    let mut parser_builder = ParseLinkBuilder::new()
        .sp_init(true);
    parser_builder.add_code(input.to_string());

    let assembly_code = parser_builder.parse_link_optimize().unwrap();
    translator::translate_and_present(&output_path.to_path_buf(), assembly_code, false, "raw", (0, 0)).unwrap();

    output_path
        .assert(predicate::path::is_file())
        .assert(predicate::path::eq_file(cor_output.path()));

    temp.close()?;

    Ok(())
}

#[test]
fn test_translate_multiple_files() -> Result<(), Box<dyn std::error::Error>> {
    let temp = assert_fs::TempDir::new()?;
    let inputs = [r#"SmallExample: addi sp, zero, 0x300
li x25, 150006
push x25
call biggun
pop x25
ret
"#, 
r#"biggun: lb x26, 0x500(zero)
ret
"#];

    let output_path = temp.child("a.bin");

    let cor_output = temp.child("correct_output.bin");

    let mut cor_instr_vec: Vec<u8> = vec![];

    #[cfg(feature = "raw_nop")]
    let instr_vec: Vec<u32> = Vec::from([
        0b00_11000_00000_00000_00000_01000_10011,
        0b00_00000_00000_00100_10111_00101_10111, // Li - lui
        0b00_00000_00000_00000_00000_00000_10011, // 3x NOP
        0b00_00000_00000_00000_00000_00000_10011,
        0b00_00000_00000_00000_00000_00000_10011,
        0b10_01111_10110_11001_00011_00100_10011, // Li - addi
        0b11_11111_11100_00010_00000_01000_10011, // Push - addi -4
        0b00_00000_00000_00000_00000_00000_10011, // 3x NOP
        0b00_00000_00000_00000_00000_00000_10011,
        0b00_00000_00000_00000_00000_00000_10011,
        0b00_00000_11001_00010_11000_00001_00011, // Push - sw
        0b00_00000_00000_00000_00000_00100_10111, // Call - auipc - 11
        0b00_00000_00000_00000_00000_00000_10011, // 3x NOP
        0b00_00000_00000_00000_00000_00000_10011,
        0b00_00000_00000_00000_00000_00000_10011,
        0b00_00001_00000_00001_00000_00111_00111, // Call - jalr - 15
        0b00_00000_00000_00010_01011_00100_00011, // Pop - Lw
        0b00_00000_00100_00010_00000_01000_10011, // Pop - addi +4
        0b00_00000_00000_00001_00000_00011_00111, // ret
        0b01_01000_00000_00000_00011_01000_00011, // Lw - 19
        0b00_00000_00000_00001_00000_00011_00111, // ret
    ]);

    #[cfg(not(feature = "raw_nop"))]
    let instr_vec: Vec<u32> = Vec::from([
        0b00_00000_00000_00000_00100_01001_10111, // Lui - sp init
        0b00_11000_00000_00000_00000_01000_10011,
        0b00_00000_00000_00100_10111_00101_10111, // Li - lui
        0b10_01111_10110_11001_00011_00100_10011, // Li - addi
        0b11_11111_11100_00010_00000_01000_10011, // Push - addi -4
        0b00_00000_11001_00010_11000_00001_00011, // Push - sw
        0b00_00000_00000_00000_00000_00100_10111, // Call - auipc - 5
        0b00_00000_10100_00001_00000_00111_00111, // Call - jalr - 6
        0b00_00000_00000_00010_01011_00100_00011, // Pop - Lw
        0b00_00000_00100_00010_00000_01000_10011, // Pop - addi +4
        0b00_00000_00000_00001_00000_00011_00111, // ret
        0b01_01000_00000_00000_00011_01000_00011, // Lw - 10
        0b00_00000_00000_00001_00000_00011_00111, // ret
    ]);

    for instr in instr_vec.iter() {
        cor_instr_vec.extend(instr.to_le_bytes());
    }

    cor_output.write_binary(&cor_instr_vec)?;

    #[cfg(feature = "raw_nop")]
    let mut parser_builder = ParseLinkBuilder::new();
    #[cfg(not(feature = "raw_nop"))]
    let mut parser_builder = ParseLinkBuilder::new()
        .sp_init(true);

    for input in inputs {
        parser_builder.add_code(input.to_string());
    }

    let assembly_code = parser_builder.parse_link_optimize().unwrap();
    translator::translate_and_present(&output_path.to_path_buf(), assembly_code, false, "raw", (0, 0)).unwrap();

    let get_vec = std::fs::read(output_path.path())?;

    for (counter, var) in get_vec.chunks(4).enumerate() {
        let read_val = u32::from_le_bytes(var.try_into().unwrap());
        let cor_val = *instr_vec.get(counter).unwrap();
        if cor_val != read_val {
            println!("Line: {}\nExpected: {:#034b}\nGot: {:#034b}", counter, cor_val, read_val);
        }
    }

    output_path
        .assert(predicate::path::is_file())
        .assert(predicate::path::eq_file(cor_output.path()));

    temp.close()?;

    Ok(())
}

#[test]
fn test_translate_no_nop() -> Result<(), Box<dyn std::error::Error>> {
    let temp = assert_fs::TempDir::new()?;
    let input = r#"SmallExample: addi sp, zero, 0x300
li x25, 150006
push x25
pop x25
ret
"#;

    let output_path = temp.child("a.bin");

    let cor_output = temp.child("correct_output.bin");

    let mut cor_instr_vec: Vec<u8> = vec![];

    let instr_vec: Vec<u32> = Vec::from([
        0b00_00000_00000_00000_00100_01001_10111, // Lui - sp init
        0b00_11000_00000_00000_00000_01000_10011,
        0b00_00000_00000_00100_10111_00101_10111, // Li - lui
        0b10_01111_10110_11001_00011_00100_10011, // Li - addi
        0b11_11111_11100_00010_00000_01000_10011, // Push - addi -4
        0b00_00000_11001_00010_11000_00001_00011,
        0b00_00000_00000_00010_01011_00100_00011, // Pop - Lw
        0b00_00000_00100_00010_00000_01000_10011,
        0b00_00000_00000_00001_00000_00011_00111
    ]);

    for instr in instr_vec.iter() {
        cor_instr_vec.extend(instr.to_le_bytes());
    }

    cor_output.write_binary(&cor_instr_vec)?;

    let mut parser_builder = ParseLinkBuilder::new()
        .sp_init(true)
        .no_nop_insert(true);
    parser_builder.add_code(input.to_string());

    let assembly_code = parser_builder.parse_link_optimize().unwrap();
    translator::translate_and_present(&output_path.to_path_buf(), assembly_code, false, "raw", (0, 0)).unwrap();

    output_path
        .assert(predicate::path::is_file())
        .assert(predicate::path::eq_file(cor_output.path()));

    temp.close()?;

    Ok(())
}

#[test]
fn test_translate_fail_undef_label() -> Result<(), Box<dyn std::error::Error>> {
    let input = r#"call nonExistentLabel
ret
"#;

    let mut parser_builder = ParseLinkBuilder::new();
    parser_builder.add_code(input.to_string());

    if let Ok(_) = parser_builder.parse_link_optimize() {
        panic!("Parse has succeeded although label is undefined!");
    }

    Ok(())
}

#[test]
fn test_translate_fail_unknown_instr() -> Result<(), Box<dyn std::error::Error>> {
    let input = r#"aylmao
ret
"#;

    let mut parser_builder = ParseLinkBuilder::new();
    parser_builder.add_code(input.to_string());

    if let Ok(code) = parser_builder.parse_link_optimize() {
        panic!("Parse has succeeded although instruction is unknown! Got: {:?}", code);
    }

    Ok(())
}

#[test]
fn test_faraway_calls() -> Result<(), Box<dyn std::error::Error>> {
    let temp = assert_fs::TempDir::new()?;

    let output_path = temp.child("a.bin");

    let input = r#"
call farAway
rep 1024, nop
farAway: j farAway
"#;

    let cor_output = temp.child("correct_output.bin");

    let mut cor_instr_vec: Vec<u8> = vec![];

    #[cfg(feature = "raw_nop")]
    let mut instr_vec: Vec<u32> = Vec::from([
        0b00_00000_00000_00000_00100_00100_10111, // auipc call farAway
        0b00_00000_00000_00000_00000_00000_10011,
        0b00_00000_00000_00000_00000_00000_10011,
        0b00_00000_00000_00000_00000_00000_10011,
        0b00_00000_11000_00001_00000_00111_00111, // jalr
    ]);

    #[cfg(not(feature = "raw_nop"))]
    let mut instr_vec: Vec<u32> = Vec::from([
        0b00_00000_00000_00000_00100_01001_10111, // Lui - sp init
        0b00_00000_00000_00000_00100_00100_10111, // auipc call farAway
        0b00_00000_01100_00001_00000_00111_00111, // jalr
    ]);

    for _ in 0..1024 {
        instr_vec.push(0b00_00000_00000_00000_00000_00000_10011); // nop
    }

    instr_vec.push(0b00_00000_00000_00000_00000_00011_01111); // jal

    for instr in instr_vec.iter() {
        cor_instr_vec.extend(instr.to_le_bytes());
    }

    cor_output.write_binary(&cor_instr_vec)?;

    #[cfg(feature = "raw_nop")]
    let mut parser_builder = ParseLinkBuilder::new();
    #[cfg(not(feature = "raw_nop"))]
    let mut parser_builder = ParseLinkBuilder::new()
        .sp_init(true);

    parser_builder.add_code(input.to_string());

    let assembly_code = parser_builder.parse_link_optimize().unwrap();
    translator::translate_and_present(&output_path.to_path_buf(), assembly_code, false, "raw", (0, 0)).unwrap();

    output_path
        .assert(predicate::path::is_file())
        .assert(predicate::path::eq_file(cor_output.path()));

    temp.close()?;

    Ok(())
}

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
    translator::translate_and_present(&output_path.to_path_buf(), assembly_code, false, "mif", (1024, 32)).unwrap();

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
    translator::translate_and_present(&output_path.to_path_buf(), assembly_code, false, "mif", (1024, 8)).unwrap();

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
    translator::translate_and_present(&output_path.to_path_buf(), assembly_code, true, "mif", (1024, 32)).unwrap();

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
    translator::translate_and_present(&output_path.to_path_buf(), assembly_code, true, "mif", (1024, 8)).unwrap();

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
    translator::translate_and_present(&no_output_path.to_path_buf(), assembly_code, true, "debug", (1024, 8)).unwrap();

    no_output_path
        .assert(predicate::path::missing());

    temp.close()?;

    Ok(())
}

#[test]
fn test_data_mif_o() -> Result<(), Box<dyn std::error::Error>> {
    let temp = assert_fs::TempDir::new()?;

    let output_path = temp.child("a.mif");
    let cor_instr_output = temp.child("correct_output.mif");
    let cor_data_output = temp.child("correct_output.mem.mif");

    let input = r#"
.data
    .space  3000            ; very much free space
testing:
    .word   GETIT
    .eqv    INSTANT, 25
.text
SmallExample:
    addi    sp, zero, 0x300
    la      t0, testing     ; load address of testing
    li      t1, INSTANT     ; load 25
GETIT:
    j       GETIT           ; haha get it?
"#;

    let mut data_vec: Vec<u32> = vec![];
    data_vec.push(0);
    data_vec = data_vec.repeat(750);

    let instr_vec: Vec<u32>;

    #[cfg(feature = "raw_nop")] {
        instr_vec = Vec::from([
            0b00_00000_00000_00000_00100_01001_10111, // Lui - sp init
            0b00_11000_00000_00000_00000_01000_10011,
            0b00_00000_00000_00000_00100_10100_10111, // la - auipc
            0b00_00000_00000_00000_00000_00000_10011,
            0b00_00000_00000_00000_00000_00000_10011,
            0b00_00000_00000_00000_00000_00000_10011,
            0b10_11101_00100_00101_00000_10100_10011, // la - addi
            0b00_00000_11001_00000_00000_11000_10011, // lui - addi
            0b00_00000_00000_00000_00000_00011_01111
        ]);
        data_vec.push(32);
    }

    #[cfg(not(feature = "raw_nop"))] {
        instr_vec = Vec::from([
            0b00_00000_00000_00000_00100_01001_10111, // Lui - sp init
            0b00_11000_00000_00000_00000_01000_10011, // addi
            0b00_00000_00000_00000_00100_10100_10111, // la
            0b10_11101_10000_00101_00000_10100_10011, // la
            0b00_00000_11001_00000_00000_11000_10011, // lui - addi
            0b00_00000_00000_00000_00000_00011_01111  // j GETIT
        ]);
        data_vec.push(20);
    }

    let mut output_data_str = "DEPTH = 1024;\nWIDTH = 32;\nADDRESS_RADIX = DEC;\nDATA_RADIX = BIN;\nCONTENT\nBEGIN\n".to_string();
    let mut output_instr_str = "DEPTH = 1024;\nWIDTH = 32;\nADDRESS_RADIX = DEC;\nDATA_RADIX = BIN;\nCONTENT\nBEGIN\n".to_string();

    for (counter, instr) in instr_vec.iter().enumerate() {
        let mach_instr = instr.to_le_bytes();
        output_instr_str.push_str(&format!("{counter}\t: {:08b}{:08b}{:08b}{:08b};\n", mach_instr[0], mach_instr[1], mach_instr[2], mach_instr[3]));
    }

    for (counter, data) in data_vec.iter().enumerate() {
        let mach_data = data.to_le_bytes();
        output_data_str.push_str(&format!("{counter}\t: {:08b}{:08b}{:08b}{:08b};\n", mach_data[0], mach_data[1], mach_data[2], mach_data[3]));
    }

    output_data_str.push_str("END;\n");
    output_instr_str.push_str("END;\n");

    cor_data_output.write_str(&output_data_str)?;
    cor_instr_output.write_str(&output_instr_str)?;

    let mut parser_builder = ParseLinkBuilder::new()
        .sp_init(true);
    parser_builder.add_code(input.to_string());

    let assembly_code = parser_builder.parse_link_optimize().unwrap();
    translator::translate_and_present(&output_path.to_path_buf(), assembly_code, false, "mif", (1024, 32)).unwrap();

    output_path
        .assert(predicate::path::is_file())
        .assert(predicate::path::eq_file(cor_instr_output.path()));

    temp.child("a.mem.mif")
        .assert(predicate::path::is_file())
        .assert(predicate::path::eq_file(cor_data_output.path()));

    temp.close()?;

    Ok(())
}

#[test]
fn test_data_mif8() -> Result<(), Box<dyn std::error::Error>> {
    let temp = assert_fs::TempDir::new()?;

    let output_path = temp.child("a.mif");
    let cor_instr_output = temp.child("correct_output.mif");
    let cor_data_output = temp.child("correct_output.mem.mif");

    let input = r#"
.data
BYTEDATA:
    .byte   1, 2, 3
    .half   1, 2, 3
DWORDDATA:
    .dword  20, 15
.text
SmallExample:
    addi    sp, zero, 0x300
    la      t0, BYTEDATA        ; load address of bytes and halfs
    la      t1, DWORDDATA       ; load dword data address
GETIT:
    j       GETIT               ; haha get it?
"#;

    let mut data_vec: Vec<u32> = vec![];
    data_vec.push(1_u32 + (2_u32 << 8) + (3_u32 << 16));
    data_vec.push((2_u32 << 16) + 1_u32);
    data_vec.push(3_u32);
    data_vec.push(20_u32);
    data_vec.push(0);
    data_vec.push(15_u32);
    data_vec.push(0);

    let instr_vec: Vec<u32>;

    #[cfg(feature = "raw_nop")] {
        instr_vec = Vec::from([
            0b00_11000_00000_00000_00000_01000_10011,
            0b00_00000_00000_00000_00000_10100_10111, // la BYTEDATA (0)
            0b00_00000_00000_00000_00000_00000_10011,
            0b00_00000_00000_00000_00000_00000_10011,
            0b00_00000_00000_00000_00000_00000_10011,
            0b11_11111_10000_00101_00000_10100_10011, // la
            0b00_00000_00000_00000_00000_11000_10111, // la DWORDDATA (12)
            0b00_00000_00000_00000_00000_00000_10011,
            0b00_00000_00000_00000_00000_00000_10011,
            0b00_00000_00000_00000_00000_00000_10011,
            0b11_11111_01000_00110_00000_11000_10011, // la
            0b00_00000_00000_00000_00000_00011_01111
        ]);
    }

    #[cfg(not(feature = "raw_nop"))] {
        instr_vec = Vec::from([
            0b00_11000_00000_00000_00000_01000_10011, // addi
            0b00_00000_00000_00000_00000_10100_10111, // la BYTEDATA (0)
            0b11_11111_11000_00101_00000_10100_10011, // la
            0b00_00000_00000_00000_00000_11000_10111, // la DWORDDATA (12)
            0b11_11111_11100_00110_00000_11000_10011, // la
            0b00_00000_00000_00000_00000_00011_01111  // j GETIT
        ]);
    }

    let mut output_data_str = "DEPTH = 1024;\nWIDTH = 8;\nADDRESS_RADIX = DEC;\nDATA_RADIX = BIN;\nCONTENT\nBEGIN\n".to_string();
    let mut output_instr_str = "DEPTH = 1024;\nWIDTH = 8;\nADDRESS_RADIX = DEC;\nDATA_RADIX = BIN;\nCONTENT\nBEGIN\n".to_string();

    let mut counter: usize = 0;

    for instr in instr_vec.iter() {
        let mach_instr = instr.to_le_bytes();
        output_instr_str.push_str(&format!("{counter}\t: {:08b} {:08b} {:08b} {:08b};\n", mach_instr[0], mach_instr[1], mach_instr[2], mach_instr[3]));
        counter += 4;
    }

    counter = 0;

    for data in data_vec.iter() {
        let mach_data = data.to_le_bytes();
        output_data_str.push_str(&format!("{counter}\t: {:08b} {:08b} {:08b} {:08b};\n", mach_data[0], mach_data[1], mach_data[2], mach_data[3]));
        counter += 4;
    }

    output_data_str.push_str("END;\n");
    output_instr_str.push_str("END;\n");

    cor_data_output.write_str(&output_data_str)?;
    cor_instr_output.write_str(&output_instr_str)?;

    let mut parser_builder = ParseLinkBuilder::new()
        .sp_init(false);
    parser_builder.add_code(input.to_string());

    let assembly_code = parser_builder.parse_link_optimize().unwrap();
    translator::translate_and_present(&output_path.to_path_buf(), assembly_code, false, "mif", (1024, 8)).unwrap();

    output_path
        .assert(predicate::path::is_file())
        .assert(predicate::path::eq_file(cor_instr_output.path()));

    temp.child("a.mem.mif")
        .assert(predicate::path::is_file())
        .assert(predicate::path::eq_file(cor_data_output.path()));

    temp.close()?;

    Ok(())
}

#[test]
fn test_data_mif_alignment() -> Result<(), Box<dyn std::error::Error>> {
    let temp = assert_fs::TempDir::new()?;

    let output_path = temp.child("a.mif");
    let cor_instr_output = temp.child("correct_output.mif");
    let cor_data_output = temp.child("correct_output.mem.mif");

    let input = r#"
.data
BYTEDATA:
    .byte   1
DWORDDATA:
    .dword  20, 15
.text
SmallExample:
    addi    sp, zero, 0x300
    la      t0, BYTEDATA        ; load address of bytes and halfs
    la      t1, DWORDDATA       ; load dword data address
GETIT:
    j       GETIT               ; haha get it?
"#;

    let mut data_vec: Vec<u32> = vec![];
    data_vec.push(1_u32);
    data_vec.push(20_u32);
    data_vec.push(0);
    data_vec.push(15_u32);
    data_vec.push(0);

    let instr_vec: Vec<u32>;

    #[cfg(feature = "raw_nop")] {
        instr_vec = Vec::from([
            0b00_11000_00000_00000_00000_01000_10011,
            0b00_00000_00000_00000_00000_10100_10111, // la BYTEDATA (0)
            0b00_00000_00000_00000_00000_00000_10011,
            0b00_00000_00000_00000_00000_00000_10011,
            0b00_00000_00000_00000_00000_00000_10011,
            0b11_11111_10000_00101_00000_10100_10011, // la
            0b00_00000_00000_00000_00000_11000_10111, // la DWORDDATA (4)
            0b00_00000_00000_00000_00000_00000_10011,
            0b00_00000_00000_00000_00000_00000_10011,
            0b00_00000_00000_00000_00000_00000_10011,
            0b11_11111_00000_00110_00000_11000_10011, // la
            0b00_00000_00000_00000_00000_00011_01111
        ]);
    }

    #[cfg(not(feature = "raw_nop"))] {
        instr_vec = Vec::from([
            0b00_11000_00000_00000_00000_01000_10011, // addi
            0b00_00000_00000_00000_00000_10100_10111, // la BYTEDATA (0)
            0b11_11111_11000_00101_00000_10100_10011, // la
            0b00_00000_00000_00000_00000_11000_10111, // la DWORDDATA (4)
            0b11_11111_10100_00110_00000_11000_10011, // la
            0b00_00000_00000_00000_00000_00011_01111  // j GETIT
        ]);
    }

    let mut output_data_str = "DEPTH = 1024;\nWIDTH = 8;\nADDRESS_RADIX = DEC;\nDATA_RADIX = BIN;\nCONTENT\nBEGIN\n".to_string();
    let mut output_instr_str = "DEPTH = 1024;\nWIDTH = 8;\nADDRESS_RADIX = DEC;\nDATA_RADIX = BIN;\nCONTENT\nBEGIN\n".to_string();

    let mut counter: usize = 0;

    for instr in instr_vec.iter() {
        let mach_instr = instr.to_le_bytes();
        output_instr_str.push_str(&format!("{counter}\t: {:08b} {:08b} {:08b} {:08b};\n", mach_instr[0], mach_instr[1], mach_instr[2], mach_instr[3]));
        counter += 4;
    }

    counter = 0;

    for data in data_vec.iter() {
        let mach_data = data.to_le_bytes();
        output_data_str.push_str(&format!("{counter}\t: {:08b} {:08b} {:08b} {:08b};\n", mach_data[0], mach_data[1], mach_data[2], mach_data[3]));
        counter += 4;
    }

    output_data_str.push_str("END;\n");
    output_instr_str.push_str("END;\n");

    cor_data_output.write_str(&output_data_str)?;
    cor_instr_output.write_str(&output_instr_str)?;

    let mut parser_builder = ParseLinkBuilder::new()
        .sp_init(false);
    parser_builder.add_code(input.to_string());

    let assembly_code = parser_builder.parse_link_optimize().unwrap();
    translator::translate_and_present(&output_path.to_path_buf(), assembly_code, false, "mif", (1024, 8)).unwrap();

    /*let get_vec = std::fs::read_to_string(output_path.path())?;

    panic!("{get_vec}");*/

    //let get_vec = std::fs::read_to_string(output_path.path())?;

    /*for (counter, var) in get_vec.chunks(4).enumerate() {
        let read_val = u32::from_le_bytes(var.try_into().unwrap());
        let cor_val = *instr_vec.get(counter).unwrap();
        if cor_val != read_val {
            println!("Line: {}\nExpected:\t{:#034b}\nGot:\t\t{:#034b}", counter, cor_val, read_val);
        }
    }*/

    output_path
        .assert(predicate::path::is_file())
        .assert(predicate::path::eq_file(cor_instr_output.path()));

    temp.child("a.mem.mif")
        .assert(predicate::path::is_file())
        .assert(predicate::path::eq_file(cor_data_output.path()));

    temp.close()?;

    Ok(())
}