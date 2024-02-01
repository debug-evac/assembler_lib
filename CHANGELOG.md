# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

<!-- next-header -->

## [Unreleased] - ReleaseDate

### Added

- Cargo binstall metadata
  - binstall command is now shorter and supports verifying signatures automatically!
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

### Changed

- Default output path
  - Default output path of "mif" files is now "./a.mif"
  - Default output path of "raw" files is now "./a.bin"

### Fixed

- Writing output file if the file cannot be written to due to an error

### Fixed

- Bit masking for immediates in shift operations with immediates
  - This resulted in incorrect amount of shifts
- Endianness
  - Little endian is now used for binary output

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
[Unreleased]: https://git.mafiasi.de/Prj-MR/Assembler/compare/1.0.1...HEAD
[1.0.1]: https://git.mafiasi.de/Prj-MR/Assembler/compare/1.0.0...1.0.1
[1.0.0]: https://git.mafiasi.de/Prj-MR/Assembler/compare/05d33c7556d2d3d08d2bc21aa930810ab19428c1...1.0.0