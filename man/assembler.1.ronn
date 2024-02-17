assembler(1) -- assemble code to self-written RISC-V based CPU
==============================================================

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
stderr. Output files are named after and written to the same directory as input
<file>s.

The `-o` or `--output` option change the directory and file name of the output
files. If the input files contain .data sections, a second output <file> is
generated that contains the data. The data output <file>'s name is the same as
the text output <file> name, but it's extension is either .mem.mif or .mem.bin
depending on the format used (ex. <file>.mem.mif, if `-f`=`mif`).

WARNING: Parser errors currently are very rudimentary, not simple and not
helpful. Make sure to use assembler-format(5) correctly. As last resort, you
may open an issue in our repository with sufficient details.

## FILES

The `assembler` command expects input to be valid assembler-format(5) code.
Source files are normally named <name>.asm (ex. `example.asm`).

Source files must be in ASCII or UTF-8 encoding. Other encodings have not been
tested and may not work.


