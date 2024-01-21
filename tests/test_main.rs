/*
 * Copyright (c) 2023 Steven Becker
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

use assert_cmd::prelude::*;
use predicates::prelude::*;
use assert_fs::prelude::*;
use std::process::Command;

#[test]
fn test_input_not_existing() -> Result<(), Box<dyn std::error::Error>> {
    let mut cmd = Command::cargo_bin("assembler")?;

    cmd.arg("-i").arg("non_existent_random_file_that_does_not_exist");
    cmd.assert()
        .failure()
        .stderr(predicate::str::contains("[Error] Could not read a file"));

    Ok(())
}

#[test]
fn test_translate_test_file() -> Result<(), Box<dyn std::error::Error>> {
    let temp = assert_fs::TempDir::new()?;
    let input = temp.child("test_assembly_file.asm");
    input.write_str(r#"SmallExample: addi sp, zero, 0x8000
li x25, 150006
push x25
pop x25
ret
"#)?;

    let mut cmd = Command::cargo_bin("assembler")?;
    cmd.arg("-i").arg(input.path()).arg("-o").arg(temp.path().join("a.bin"));
    cmd.assert()
        .success();

    temp.child("a.bin").assert(predicate::path::is_file());
    // TODO: Compare file content

    temp.close()?;

    Ok(())
}

#[ignore]
#[test]
fn test_translate_multiple_files() -> Result<(), Box<dyn std::error::Error>> {
    todo!();
}

#[ignore]
#[test]
fn test_translate_no_nop() -> Result<(), Box<dyn std::error::Error>> {
    todo!();
}

#[ignore]
#[test]
fn test_translate_failure() -> Result<(), Box<dyn std::error::Error>> {
    todo!();
}
