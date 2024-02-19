assembler-format(5) -- format of input assembly files
=====================================================

<!-- THIS IS A .RONN FILE -->

## SYNOPSIS

    ; is a single line comment, you can write everything here
    .data
    ; You can use labels here as well as in memory locations
    LABEL:
        .byte   1, 2, 3, LABEL      ; 8 bit numbers
        .half   1, 2, 3, LABEL      ; 16 bit numbers
        .word   1, 2, 3, LABEL      ; 32 bit numbers
        .dword  1, 2, 3, LABEL      ; 64 bit numbers
        .eqv    CONST, 20           ; CONST is set to 20

    .text
    ; You can also use labels here in the .text section
    ; Write instructions and macros here
    START:
        nop
        addi    zero, zero, 0
        addi    t1, zero, 2
        mul     t0, sp, t1
    LOD:
        j       LOD                 ; You can use labels in macros

## DESCRIPTION

The assembler(1) command converts assembly files to output in binary or mif
format. The syntax is based on MIPS assembly syntax and is modified to lessen
programmer burden and increase compatibility to already established tools and
conventions.

## SYNTAX

In general, every line contains nothing (blanks), a section, a label or a label
and operation. Comments can be added to the end of any line or inserted into a
line that otherwise would contain nothing (blanks). The assembler is leniant
with the placement of these components to eachother. A operation is an
instruction, macro or directive.

A single line:

    LABEL: <OPERATION> ; COMMENT

Multiple lines that fall into a single line:

    LABEL:
                    ; COMMENT
    ; COMMENT

                            <OPERATION>
         ; COMMENT

Similiar to above, but in a different format:

    LABEL: ; COMMENT
        <OPERATION>                     ; COMMENT
        ; COMMENT

All three examples are equal, if the operations are equal.

See [OPERATIONS][] and [LABEL DEFINITIONS][] for details.

## TEXT AND DATA SECTIONS

Sections are ordered and used to outline section of particular kind.
Sections of any kind can only be defined once. Currently only data and text
sections are supported with no plans for supporting other sections.

The use of sections are optional and a assembly file containing no explicit
sections, implicitly defines a text section spanning the whole file.

Assembly files must contain a text section and optionally can contain a
non-empty data section.

Data sections must come before text sections.

Valid assembly file that includes a data and text section:

    .data                           ; Outlines a data section spanning to the
        <DIRECTIVE>                 ; next text section
        <DIRECTIVE>

    .text                           ; Outlines a text section spanning to the
        <INSTRUCTION>               ; end of the file
        <INSTRUCTION>

Valid assembly file that only includes a text section:

    .text
        <INSTRUCTION>
        <INSTRUCTION>

Valid assembly file that implicitly defines a text section:

    START:
        <INSTRUCTION>
        <INSTRUCTION>

Invalid assembly file since the data section is empty (this may be changed
in the future):

    .data
    .text
        <INSTRUCTION>

Invalid assembly file since it only contains a data section:

    .data
        <DIRECTIVE>
        <DIRECTIVE>

See [DIRECTIVES][], [MACROS][] and [INSTRUCTIONS][] for details.

## LABEL DEFINITIONS

Label definitions are label references that are suffixed with a colon. Labels
must be at least one character long and can only contain alphanumeric
characters. The first character must be alphabetic.

Correct label definitions include the following:

    Label0:
    A:
    LabelVeryNice:
    Example215:

Incorrect labels definitions include:

    :
    __TEST:
    0Lol:
    Lol

Do note that currently there is no validation of label types and if the label
value fits into the instruction. Errors for the former and warnings of the
latter are planned.

## OPERATIONS

Operations is a definition used to describe instructions, macros and
directives. See [INSTRUCTIONS][], [DIRECTIVES][] and [MACROS][] for
details.

## REGISTERS

Some instructions and macros require registers to perform actions. There are 31
registers that can be used. Registers can be referenced by either the register
number prefixed with an `x`, meaning `x0` to `x31`, or their ABI name.

  * `x0`, `zero`:
    Immutable register that is always zero.

  * `x1`, `ra`:
    Return address, callee-saved.

  * `x2`, `sp`:
    Stack pointer, callee-saved.

  * `x3`, `gp`:
    General purpose register, caller-saved. Global pointer according to RISC-V
    spec, but not used as such here.

  * `x4`, `tp`:
    General purpose register, caller-saved. Thread pointer according to RISC-V
    spec, but not used as such here.

  * `x`[`5`|`6`|`7`], `t`[`0`|`1`|`2`]:
    Temporary register, caller-saved.

  * `x8`, `s0`, `fp`:
    Saved register or frame pointer, callee-saved.

  * `x9`, `s1`:
    Saved register, callee-saved.

  * `x`[`10`|`11`], `a`[`0`|`1`]:
    Register for return values and function arguments, caller-saved.

  * `x`[`12`|`13`|`14`|`15`|`16`|`17`], `a`[`2`|`3`|`4`|`5`|`6`|`7`]:
    Register for function arguments, caller-saved.

  * `x`[`18`|`19`|`20`|`21`|`22`|`23`|`24`|`25`|`26`|`27`], `s`[`2`|`3`|`4`|`5`|`6`|`7`|`8`|`9`|`10`|`11`]:
    Saved register, callee-saved.

  * `x`[`28`|`29`|`30`|`31`], `t`[`3`|`4`|`5`|`6`]:
    Temporary register, caller-saved.

## IMMEDIATES

Some instructions, macros and directives require immediates to perform actions.
Immediates are either in decimal, binary or hexadecimal format. By default the
decimal format is assumed. To interpret immediates as binary or hexadecimal, the
prefix `0b` and `0x` must be used respectively. Optionally binary or hexadecimal
can be interpreted as signed numbers by suffixing the immediate with `s` or `S`.

The following immediates are valid:

    0b10          ; 2
    0b10s         ; -2
    0x14          ; 20
    0x14s         ; 20
    205
    -12

The following immediates are invalid:

    0.1           ; floating point not supported (yet)
    b100
    x1516
    02x3
    50-20         ; expressions are not supported

## DIRECTIVES

Directives are used in data sections and always prefixed with a dot (`.`). Some
common directives are supported and mainly the ones that can be used to store
data in the data section. The arguments can be either immediates or labels. The
arguments are separated by commas (`,`) or commas with a space (`, `). The
directive instruction and arguments are separated by one or more spaces.

For some directives the argument is a string, which is delimited by quotation
marks. Otherwise general rules apply here as well.

The order of the directives in the assembly file dictate the order of the data
in memory. Data starts at address 0 and grows upwards. The first directive is
written to address 0.

Currently the following directives are supported:

  * `.byte` <REG>|<LABEL>,[<REG>|<LABEL>]...
    The registers and labels are stored as 8 bit values in memory.

  * `.half` <REG>|<LABEL>,[<REG>|<LABEL>]...
    The registers and labels are stored as 16 bit values in memory.

  * `.word` <REG>|<LABEL>,[<REG>|<LABEL>]...
    The registers and labels are stored as 32 bit values in memory.

  * `.dword` <REG>|<LABEL>,[<REG>|<LABEL>]...
    The registers and labels are stored as 64 bit values in memory.

  * `.space` <DECIMAL>
    Reserve space for data. The <DECIMAL> denotes the space reserved in bytes.
    It must be a decimal and cannot be negative.

  * `.ascii` `"`<STRING>`"`
    The <STRING> is stored as consecutive 8 bit values. The <STRING> should only
    contain ASCII characters. All characters are translated to their ASCII code.
    The <STRING> is not null terminated.

  * `.asciz` `"`<STRING>`"`
    Same as `.ascii` but the <STRING> is null terminated.

  * `.string` `"`<STRING>`"`
    Alias for `.asciz`.

  * `.eqv` <LABEL>, <IMMEDIATE>
    The value of the <LABEL> is <IMMEDIATE>. A <LABEL> emitted this way is a
    constant that is not written in memory and can be used like a immediate.

These are valid directives:

    .byte   1, 3,2,LABEL
    .half 20, 15, LABEL
    .space 10
    .eqv                          LABEL,30

These are invalid directives:

    .non                  ; unknown directive
    .byte                 ; no arguments
    .half 30 15
    .space 0b10           ; argument can only be decimal
    .asciz "STRING        ; missing closing quotation mark

## MACROS

Macros are pseudo-instructions that are not and cannot be translated to machine
code as is. The syntax is similiar to [INSTRUCTIONS][]. Some of the common
macros are supported. The arguments can be immediates, registers and labels. The
arguments are separated by commas (`,`) or commas with a space (`, `). The
macro instruction and arguments are separated by one or more spaces.

Macros are expanded and mapped to instructions that are machine translatable.
Macros cannot be defined by programmers.

## INSTRUCTIONS

## SEE ALSO

assembler(1)
