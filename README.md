# Assembler Library

This is a library that was programmed within the project "Mikrorechner" in order to assemble and disassemble code for a self-written RISC-V CPU.
It uses a modified MIPS assembly syntax and features labels, macros, automatic nop insertion, a simple linker, instruction minimization for certain macros and output formatting to raw or mif.

## Features

- [x] Support for all instructions of the RV32I and RV32M extensions
- [x] Support for some macros for easier programming, especially:
  - [x] Stack operations (push, pop)
  - [x] Load address (la), load immediate (li), call, tail & mv
- [x] Simple linker to link multiple input assembly files
  - [x] Global and local (file scope) labels
- [x] Rudimentary optimizer for automatic insertions of nop instructions when data hazards are expected
- [x] Rudimentary user output (wip)
- [x] Support for assembler directives that are used to store constants in memory
- [x] Python module for easier integration into the compiler
- [ ] Improvement of error reporting (currently very bare bones)
- [ ] Better internal code documentation
- [ ] Disassembler-Mode (planned for release 2.0.0 or a minor release of 2.0.0)

## Out of scope

These features may or may not be implemented.

- Simple peephole optimization, that for example maps `add x7, x6, x6` to `slli x7, x6, 1` (since shifts are faster than additions)

## Installation

The assembler_lib library can be used in Python or Rust. It also can be compiled to a C library, however steps for that are not outlined here.

**This crate does not contain the binary! Head over to [assembler repo](https://git.mafiasi.de/Prj-MR/assembler) for the `assembler` binary.**

### Python Module

Since release 1.3.0, a python module is provided which contains the functionality of this library. For installation, `pip` is required.

You can use the following command to install this as a python module:

```sh
pip install --index-url https://{username}:{password}@git.mafiasi.de/api/packages/Prj-MR/pypi/simple --no-deps assembler_lib
```

For more information on installation, see [here](https://docs.gitea.com/next/usage/packages/pypi/).

You can find information on how to use this module in the CHANGELOG.md.

### Rust library

#### Using `cargo add` with Cargo registry

To use the Cargo registry, you need to create two configuration files. The process is described for Linux. If you are using another operating system, you will need to modify some steps to make them fit.

##### Linux

Create a file at `~/.cargo/config.toml` with the following content:

```
[registry]
global-credential-providers = ["cargo:token", "cargo:libsecret"]

[registries.mafiasi-gitea] 
index = "sparse+https://git.mafiasi.de/api/packages/Prj-MR/cargo/"
```

This defines a new registry to use for cargo commands and authentication methods for these registries. `Crate.io` is the default registry, which is **NOT USED** for this crate. If you do not have a system keyring installed, remove `"cargo:libsecret"` from the credential-providers.

Create another file at `~/.cargo/credentials.toml` with the following content:

```
[registries.mafiasi-gitea]
token = "Bearer {token}"
```

You need to create a token in Gitea and replace the placeholder with it. You can create a token for Mafiasi Gitea under this URL: [https://git.mafiasi.de/user/settings/applications](https://git.mafiasi.de/user/settings/applications). The token needs to have read/write permission for package. **DO NOT SHARE THE TOKEN WITH OTHERS!**

Once done, you can now use `cargo add assembler_lib --registry mafiasi-gitea`. If you want to install a particular version, use `cargo add assembler_lib@VERSION --registry mafiasi-gitea`, example `cargo add assembler_lib@1.0.0 --registry mafiasi-gitea` for Version 1.0.0.

#### Using `cargo add` with Git

This method is **highly discouraged**. You can use the following command to add this library with git: `cargo add --git https://git.mafiasi.de/Prj-MR/assembler_lib --tag {VERSION}`. Version is the version that you want to add. You can otherwise use the stable branch with `cargo add --git https://git.mafiasi.de/Prj-MR/assembler_lib --branch stable`. Using the main branch is discouraged.

## Usage

Work-in-Progress.

## Support

Please open an issue if you encounter any bugs or irregularities and include information on how to reproduce that. We will promptly fix it then!

## Contributing

Pull requests are welcome. For major changes, please open an issue first
to discuss what you would like to change.

Please make sure to update tests as appropriate.

## License

[MPL-2.0](https://www.mozilla.org/en-US/MPL/2.0/)
