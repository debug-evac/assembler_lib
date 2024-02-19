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

  * `x5`, `t0`<br>`x6`, `t1`<br>`x7`, `t2`:
    Temporary register, caller-saved.

  * `x8`, `s0`, `fp`:
    Saved register or frame pointer, callee-saved.

  * `x9`, `s1`:
    Saved register, callee-saved.

  * `x10`, `a0`<br>`x11`, `a1`:
    Register for return values and function arguments, caller-saved.

  * `x12`, `a2`<br>`x13`, `a3`<br>`x14`, `a4`<br>`x15`, `a5`<br>`x16`, `a6`<br>`x17`, `a7`:
    Register for function arguments, caller-saved.

  * `x18`, `s2`<br>`x19`, `s3`<br>`x20`, `s4`<br>`x21`, `s5`<br>`x22`, `s6`<br>`x23`, `s7`<br>`x24`, `s8`<br>`x25`, `s9`<br>`x26`, `s10`<br>`x27`, `s11`:
    Saved register, callee-saved.

  * `x28`, `t3`<br>`x29`, `t4`<br>`x30`, `t5`<br>`x31`, `t6`:
    Temporary register, caller-saved.

## IMMEDIATES

Some instructions, macros and directives require immediates to perform actions.


## DIRECTIVES

## MACROS

## INSTRUCTIONS

## SEE ALSO

assembler(1)
