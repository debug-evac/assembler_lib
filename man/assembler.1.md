assembler(1) -- assemble code to self-written RISC-V based CPU
==============================================================

<!-- THIS IS A .RONN FILE -->

## SYNOPSIS

`assembler` [<OPTIONS>...] `--input` <file>...<br>
`assembler` `-i`|`--input` <file>...<br>
`assembler` `-i`|`--input` <file>... [`-o`|`--output` <file>]<br>
`assembler` `-i`|`--input` <file>... [`-f`|`--format` [`debug`|`mif`|`raw`]]
<br>
`assembler` `-i`|`--input` <file>... [`-f`|`--format` `mif`] [`-c`]

## DESCRIPTION

**Assembler** translates assembly code to machine code for a self-written
RISC-V based CPU. assembler-format(5) is based on a modified MIPS syntax that
includes instructions and macros from RISC-V, especially RV32I and RV32M.

By default, `assembler` translates one or more input <file>s to binary or MIF
output files. The `-f` or `--format` option dictates which format should be
used for the output. The output is either binary (`raw`), MIF in accordance to
src_mif(5) (`mif`) or none (`debug`), in which case it will be printed to
stderr(3). The MIF format is used by default. Output files are named after and
written to the same directory as input <file>s.

The `-o` or `--output` option change the directory and file name of the output
files. If the input files contain .data sections, a second output <file> is
generated that contains the data.

WARNING: Parser errors currently are very rudimentary, not simple and not
helpful. Make sure to use assembler-format(5) correctly. As last resort, you
may open an issue in our repository with sufficient details.

## FILES

The `assembler` command expects input to be valid assembler-format(5) code.
Source files are normally named <name>.asm (ex. `example.asm`).

Source files must be in ASCII or UTF-8 encoding. Other encodings have not been
tested and may not work.

When assembling output files, destination filenames are either a.<ext> for text
output and a.mem.<ext> for data output or <name> for text output and
<name>.mem.<ext> for data output, if the `-o` or `--output` option is used.
Data output is only generated, if .data sections are used in the input <file>s.
If specifying a output with an extension, the text output is written to that
location but the data output is always written to the stem of the filename with
the extension mem.<ext>. For example, executing
`assembler -i example.asm -o test.example` will write the text output to
`test.example` and the code output to `test.mem.mif`.

## OPTIONS

These options control the format, location and type of the output.

  * `-f`, `--format`:
    Specify the format of the output. Valid values are `[raw, mif, debug]`.
    The default is `mif`. `debug` only prints the output to stderr(3). As the
    name implies, it should only be used for debugging purposes. Do not expect
    stability in the format.

    `raw` writes the machine code and data as binary to the output files.

    `mif` writes and formats the machine code and data as MIF, see src_mif(5)
    for details. The MIF format can be commented with the instruction assembly
    names using option `-c` or `--comment`. The memory depth and word width can
    be changed using `--depth` and `--width` respectively.

  * `-c`, `--comment`:
    Flag to indicate that MIF output should be commented. By default MIF output
    is not commented. When used,
