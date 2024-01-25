# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

<!-- next-header -->

## [Unreleased] - ReleaseDate

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
[Unreleased]: https://git.mafiasi.de/Prj-MR/Assembler/compare/1.0.0...HEAD
[1.0.0]: https://git.mafiasi.de/Prj-MR/Assembler/compare/05d33c7556d2d3d08d2bc21aa930810ab19428c1...1.0.0