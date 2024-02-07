# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

<!-- next-header -->

## [Unreleased] - ReleaseDate

### Added

- Simple progress bar for tracking... well, progress!
  - Progress bars are always written to stderr
- Simple Logging
  - There are multiple log levels: [error, warn, info, debug, trace, off] with error being the highest priority (always on unless log_level == off) and trace being the lowest priority (always off unless log_level == trace)
  - You can choose the log level by using the env "RUST_LOG", ex. `RUST_LOG=warn assembler ...`
  - Progress information and status messages are available via the `info` log level (default)
  - Logs are always written to stderr
  - Log level "debug" gives you more insight in the proceedings of the assembler, especially what instruction generates which machine code
- More test coverage
- New hardware supported instructions
  - `mulhu R1, R2, R3` - Unsigned high multiplication
  - `mulhsu R1, R2, R3` - Unsigned and signed high multiplication
  - `div R1, R2, R3` - Signed division
  - `divu R1, R2, R3` - Unsigned division (previously subroutine only)
  - `rem R1, R2, R3` - Signed remainder
  - `remu R1, R2, R3` - Unsigned remainder (previously subroutine only)
- More examples
  - `fakultaet_64bit.asm` which uses 64 bits to store the solution
  - `assembler_crasher_2000.asm` which is a very long assembly file **(Do NOT use debug log level)**
  - `hardware_test.asm` which is a program to test the hardware (end of execution, register x6 == 0xDEADBEEF)
- Taskfile.yaml for release automation (dev-ops)

### Changed

- Internal representation of some instructions (renamed)
  - No changes in behavior

### Removed

- Multiplication subroutine and handling
  - Multiplication was hardware supported before and code has been commented out
  - No changes in behavior
- Division and Remainder subroutine and handling
  - These are now handled in hardware
  - No changes in behavior

### Fixed

- Running tests with all-features flag
- Bug that resulted in wrong jump and load addresses when using read after write nop insertion compilation flag
- Parsing bug that disallowed certain longer instructions because they were overshadowed (ex. lh before lhu)

## [1.1.2] - 2024-02-03

### Fixed

- Macro instructions with immediates that get expanded to multiple instructions
  - In certain situations these got optimized to one instruction instead of two, which resulted in wrong immediates
- Parsing instructions that also have shorter instructions as substring
  - There were instructions that could not get parsed because the shorter instructions have been matched incorrectly

## [1.1.1] - 2024-02-02

### Added

- Aarch64 (arm64) MacOS target for binaries

### Fixed

- Local labels not being found in nop insertion optimization step
- Func3 of Store instructions

## [1.1.0] - 2024-02-01

### Added

- Simple readme.md
- Cargo binstall metadata
  - binstall command is now shorter!
- Examples for assembly code under the "examples" directory
- New CLI arguments
  - `--format` or `-f` that specifies in which format the output should appear
    - Possible values are "raw" and "mif"
  - `--depth` that specifies the address count of the "mif" format file
    - Possible values are 1 to 65535
  - `--width` that specifies the width of a word
    - Possible values are 8 and 32
  - The default format is now "mif"
- Better error handling for output file writes
- Ability to write single line comments in the assembly code
  - Use '; < COMMENT >' to comment something in the code, works inline as well as before
  - Example:
    ```
    ; WORKS
        ; WORKS
    nop     ; WORKS
    ; WORKS LIKE A CHAMP ^^
    ```
- Repeat Macro, which can be used to.. well repeat instructions and macros!
  - Use 'rep < DEC >, [ MACRO | INSTRUCTION ]' to repeat the macro or instruction DEC amount of times
  - Nested repeat macros are not supported!
  - Example:
    ```
    ; WORKS
    rep 20, nop
    ; ERROR
    rep 20, rep 40, nop
    ```
- More tests

### Changed

- Default output path
  - Default output path of "mif" files is now "./a.mif"
  - Default output path of "raw" files is now "./a.bin"

### Fixed

- Writing output file if the file cannot be written to due to an error
  - File is now not written
- Bit masking for immediates in shift operations with immediates
  - This resulted in incorrect amount of shifts
- Endianness
  - Little endian is now used for binary output
- `call <LABL>` and `tail <LABL>`
  - Using these resulted in wrong jumps

## [1.0.1] - 2024-01-26

### Added

- Gitea Codeowners file for responsibility management
  - No changes in program behavior

### Changed

- Build option flags for the release build
  - Binaries are now significantly smaller & probably faster in execution as well
  - Release builds now take a little bit more time

### Removed

- A doc file from the repo, which contained outdated information
  - No changes in program behavior
- Part of help string for "--no-nop-insertion" flag, which stated that this option was not respected
  - The flag was respected before, so no changes in program behavior

### Fixed

- NOP insertion for load instructions
  - The wrong register was being used to look for hazards

## [1.0.0] - 2024-01-23

### Added

- Support for most of the RV32I instructions
  - No support for ecall and ebreak (yet)
- Subroutines for some instructions of the RV32M instruction set, that are not supported by hardware
  - remu
  - div (subject of name change in later releases)
- Some Macros for easier use, for example:
  - Stack operations (push, pop)
  - Load address (la), load immediate (li), call, tail, mv...
- Linker for linking multiple assembly files to one binary
  - Global and local (file only) labels
- Simple CLI
  - Currently three arguments: input (multiple paths), output (optional, one path), no-nop-insertion (flag)
- Basic Optimizer for automatic insertion of nop instructions to circumvent data dependencies
- Use of all hardware features available
  - Forwarding unit, Multiplication hardware instruction

<!-- next-url -->
[Unreleased]: https://git.mafiasi.de/Prj-MR/Assembler/compare/1.1.2...HEAD
[1.1.2]: https://git.mafiasi.de/Prj-MR/Assembler/compare/1.1.1...1.1.2
[1.1.1]: https://git.mafiasi.de/Prj-MR/Assembler/compare/1.1.0...1.1.1
[1.1.0]: https://git.mafiasi.de/Prj-MR/Assembler/compare/1.0.1...1.1.0
[1.0.1]: https://git.mafiasi.de/Prj-MR/Assembler/compare/1.0.0...1.0.1
[1.0.0]: https://git.mafiasi.de/Prj-MR/Assembler/compare/05d33c7556d2d3d08d2bc21aa930810ab19428c1...1.0.0
