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
directives. The arguments can be immediates, registers and/or labels. The
arguments are separated by commas (`,`) or commas with a space (`, `). The
operation name and arguments are separated by one or more spaces.

See [INSTRUCTIONS][], [DIRECTIVES][] and [MACROS][] for details.

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
data in the data section.

For some directives the argument is a string, which is delimited by quotation
marks. Otherwise general rules apply here as well.

The order of the directives in the assembly file dictate the order of the data
in memory. Data starts at address 0 and grows upwards. The first directive is
written to address 0.

Currently the following directives are supported:

  * `.byte` <REG>|<LABEL>,[<REG>|<LABEL>]...:
    The registers and labels are stored as 8 bit values in memory.

  * `.half` <REG>|<LABEL>,[<REG>|<LABEL>]...:
    The registers and labels are stored as 16 bit values in memory.

  * `.word` <REG>|<LABEL>,[<REG>|<LABEL>]...:
    The registers and labels are stored as 32 bit values in memory.

  * `.dword` <REG>|<LABEL>,[<REG>|<LABEL>]...:
    The registers and labels are stored as 64 bit values in memory.

  * `.space` <DECIMAL>:
    Reserve space for data. The <DECIMAL> denotes the space reserved in bytes.
    It must be a decimal and cannot be negative.

  * `.ascii` `"`<STRING>`"`:
    The <STRING> is stored as consecutive 8 bit values. The <STRING> should only
    contain ASCII characters. All characters are translated to their ASCII code.
    The <STRING> is not null terminated.

  * `.asciz` `"`<STRING>`"`:
    Same as `.ascii` but the <STRING> is null terminated.

  * `.string` `"`<STRING>`"`:
    Alias for `.asciz`.

  * `.eqv` <LABEL>, <IMMEDIATE>:
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
macros are supported.

Macros are expanded and mapped to instructions that are machine translatable.
Macros cannot be defined by programmers.

The first register for macros that have them is always the register that is
written to.

Currently the following macros are supported:

  * `srr` <REGISTER>, <REGISTER>, <IMMEDIATE>:
    Shift right rotate. This is implemented as a subroutine thus saving registers
    is required. Saving registers is not done automatically!

  * `slr` <REGISTER>, <REGISTER>, <IMMEDIATE>:
    Shift left rotate. This is implemented as a subroutine thus saving registers
    is required. Saving registers is not done automatically!

  * `li` <REGISTER>, <IMMEDIATE>|<LABEL>:
    Load immediate. <REGISTER> is set to the <IMMEDIATE> or <LABEL>.

  * `la` <REGISTER>, <IMMEDIATE>|<LABEL>:
    Load address. <REGISTER> is set to either the <IMMEDIATE> or the address of
    the <LABEL>.

  * `call` <IMMEDIATE>|<LABEL>:
    Jump to a far-away label and handle it as a subroutine. The return address
    is written to register `ra`. Returning is possible by using the macro `ret`
    or by the equivalent `jal` instruction.

  * `tail` <IMMEDIATE>|<LABEL>:
    Jump to a far-away label. The return address is voided. Returning is not
    possible.

  * `push` <REGISTER>, [<REGISTER>]...:
    Save the content of these registers onto the stack. Requires initialization
    of the stack pointer register `sp`. Multiple registers can be specified to
    reduce the subtraction overhead. The registers are saved in the given order.
    The first register is saved at the bottom, the last register at the top of
    stack.

  * `pop` <REGISTER>, [<REGISTER>]...:
    Load the content of the stack into the registers. Requires initialization
    of the stack pointer register `sp`. Multiple registers can be specified to
    reduce the addition overhead. The content is loaded into the registers in
    the given order. The first register receives the content of the top of
    stack, the last register of the bottom.

  * `rep` <DECIMAL>, <INSTRUCTION>|<MACRO>:
    Repeat the <INSTRUCTION> or <MACRO> <DECIMAL> times. The decimal must be
    positive and greater than 0. Repeats cannot be nested, meaning a repeat
    cannot contain a repeat.

  * `mv` <REGISTER>, <REGISTER>:
    Copies the content of the latter register into the former register. This is
    mapped to either the instruction `addi` or `add`.

  * `nop`:
    No operation. It does not do anything. This is mapped to either the
    instruction `addi zero, zero, 0` or `add zero, zero, zero`.

  * `ret`:
    Used to return from a subroutine. This is mapped to the instruction
    `jalr zero, ra, 0`.

  * `j` <IMMEDIATE>|<LABEL>:
    Jump to the <LABEL> or <IMMEDIATE>. This is mapped to the instruction
    `jal zero,` <OFFSET>.

  * `jal` <IMMEDIATE>|<LABEL>:
    Jump and link to the <LABEL> or <IMMEDIATE>. This is mapped to the
    instruction `jal ra,` <OFFSET>.

  * `jr` <REGISTER>:
    Jump to the address in the <REGISTER>. This is mapped to the instruction
    `jalr zero,` <REGISTER>`, 0`.

  * `jalr` <REGISTER>:
    Jump and link to the address in the <REGISTER>. This is mapped to the
    instruction `jalr ra,` <REGISTER>`, 0`.

See
[RISC-V Specification](https://www.cs.sfu.ca/~ashriram/Courses/CS295/assets/notebooks/RISCV/RISCV_CARD.pdf)
(<https://www.cs.sfu.ca/~ashriram/Courses/CS295/assets/notebooks/RISCV/RISCV_CARD.pdf>)
for details.

## INSTRUCTIONS

An instruction is machine code in human-readable form. The syntax is similiar to
[MACROS][]. All instructions of the RV32I and RV32M extensions are supported.

The first register for instructions that have them is always the register that
is written to. A limitation to that rule are branch instructions.

These instructions are used to perform arithmetical, logical and shift
operations with registers:

  * `add` <REGISTER>, <REGISTER>, <REGISTER>:
    Addition of the two latter <REGISTER>s.

  * `sub` <REGISTER>, <REGISTER>, <REGISTER>:
    Subtraction. The minuend is the content of the second <REGISTER>, the
    subtrahend is the content of the last <REGISTER>.

  * `xor` <REGISTER>, <REGISTER>, <REGISTER>:
    Logical bitwise exclusive or of the second and third <REGISTER>.

  * `or` <REGISTER>, <REGISTER>, <REGISTER>:
    Logical bitwise or of the second and third <REGISTER>.

  * `and` <REGISTER>, <REGISTER>, <REGISTER>:
    Logical bitwise or of the second and third <REGISTER>.

  * `sll` <REGISTER>, <REGISTER>, <REGISTER>:
    Logical left shift of the second <REGISTER> by the third <REGISTER>.

  * `srl` <REGISTER>, <REGISTER>, <REGISTER>:
    Logical right shift of the second <REGISTER> by the third <REGISTER>.

  * `sra` <REGISTER>, <REGISTER>, <REGISTER>:
    Arithmetical right shift of the second <REGISTER> by the third <REGISTER>.

  * `slt` <REGISTER>, <REGISTER>, <REGISTER>:
    The first <REGISTER> is set to one (1), if the second <REGISTER> is less
    than the last <REGISTER>.

  * `sltu` <REGISTER>, <REGISTER>, <REGISTER>:
    The first <REGISTER> is set to one (1), if the second <REGISTER> is less
    than the last <REGISTER>. The content of the <REGISTER>s compared are
    interpreted as unsigned numbers.

  * `xnor` <REGISTER>, <REGISTER>, <REGISTER>:
    Logical bitwise negated exclusive or of the second and third <REGISTER>.
    Note that this is not defined in the RISC-V standard.

  * `equal` <REGISTER>, <REGISTER>, <REGISTER>:
    Compares the second and third <REGISTERS> and sets the first <REGISTER> to
    one (1), if they are equal. Note that this is not defined in the RISC-V
    standard.

  * `mul` <REGISTER>, <REGISTER>, <REGISTER>:
    Multiplication of the second and third <REGISTER>. The first <REGISTER> is
    set to the lower 32 bits of the result.

  * `mulh` <REGISTER>, <REGISTER>, <REGISTER>:
    High Multiplication of the second and third <REGISTER>. The first <REGISTER>
    is set to the higher 32 bits of the result. The content of both <REGISTER>s
    is interpreted as signed numbers.

  * `mulhu` <REGISTER>, <REGISTER>, <REGISTER>:
    High Multiplication of the second and third <REGISTER>. The first <REGISTER>
    is set to the higher 32 bits of the result. The content of both <REGISTER>s
    is interpreted as unsigned numbers.

  * `mulhsu` <REGISTER>, <REGISTER>, <REGISTER>:
    High Multiplication of the second and third <REGISTER>. The first <REGISTER>
    is set to the higher 32 bits of the result. The content of the second
    <REGISTER> is interpreted as a signed number, the content of the third
    <REGISTER> is interpreted as a unsigned number.

  * `div` <REGISTER>, <REGISTER>, <REGISTER>:
    Division of the second and third <REGISTER>s. The second <REGISTER> is the
    dividend and the third <REGISTER> is the divisor. The content of both
    <REGISTER>s are interpreted as signed numbers.

  * `divu` <REGISTER>, <REGISTER>, <REGISTER>:
    Division of the second and third <REGISTER>s. The second <REGISTER> is the
    dividend and the third <REGISTER> is the divisor. The content of both
    <REGISTER>s are interpreted as unsigned numbers.

  * `rem` <REGISTER>, <REGISTER>, <REGISTER>:
    Modulo operation of the second and third <REGISTER>s. The second <REGISTER>
    is the dividend and the third <REGISTER> is the divisor. The content of both
    <REGISTER>s are interpreted as signed numbers.

  * `remu` <REGISTER>, <REGISTER>, <REGISTER>:
    Modulo operation of the second and third <REGISTER>s. The second <REGISTER>
    is the dividend and the third <REGISTER> is the divisor. The content of both
    <REGISTER>s are interpreted as unsigned numbers.

These instructions are used to perform arithmetical, logical and shift
operations with immediates:

Shift operations can only take 4 bit immediates.

Note that some instructions cannot use labels. This is WIP.

  * `addi` <REGISTER>, <REGISTER>, <IMMEDIATE>|<LABEL>:
    Addition of the second <REGISTER> and the <IMMEDIATE> or <LABEL>.

  * `xori` <REGISTER>, <REGISTER>, <IMMEDIATE>:
    Logical bitwise exclusive or of the second <REGISTER> and the <IMMEDIATE>.

  * `ori` <REGISTER>, <REGISTER>, <IMMEDIATE>:
    Logical bitwise or of the second <REGISTER> and the <IMMEDIATE>.

  * `andi` <REGISTER>, <REGISTER>, <IMMEDIATE>:
    Logical bitwise or of the second <REGISTER> and the <IMMEDIATE>.

  * `slli` <REGISTER>, <REGISTER>, <IMMEDIATE>|<LABEL>:
    Logical left shift of the second <REGISTER> by the <IMMEDIATE> or <LABEL>.

  * `srli` <REGISTER>, <REGISTER>, <IMMEDIATE>|<LABEL>:
    Logical right shift of the second <REGISTER> by the <IMMEDIATE> or <LABEL>.

  * `srai` <REGISTER>, <REGISTER>, <IMMEDIATE>|<LABEL>:
    Arithmetical right shift of the second <REGISTER> by the <IMMEDIATE> or
    <LABEL>.

  * `slti` <REGISTER>, <REGISTER>, <IMMEDIATE>:
    The first <REGISTER> is set to one (1), if the second <REGISTER> is less
    than the <IMMEDIATE>.

  * `sltiu` <REGISTER>, <REGISTER>, <IMMEDIATE>:
    The first <REGISTER> is set to one (1), if the second <REGISTER> is less
    than the <IMMEDIATE>. The content of the <REGISTER>s compared are
    interpreted as unsigned numbers.

These instructions are used to manipulate memory content:

The target byte and half are always the LSBs of the target register.

  * `lb` <REGISTER>, <REGISTER>, <IMMEDIATE>|<LABEL>:
    Loads a byte from memory at the address which is the sum of the second
    <REGISTER> and the <IMMEDIATE> or <LABEL>.

  * `lh` <REGISTER>, <REGISTER>, <IMMEDIATE>|<LABEL>:
    Loads a half (16 bits) from memory at the address which is the sum of the
    second <REGISTER> and the <IMMEDIATE> or <LABEL>.

  * `lw` <REGISTER>, <REGISTER>, <IMMEDIATE>|<LABEL>:
    Loads a word (32 bits) from memory at the address which is the sum of the
    second <REGISTER> and the <IMMEDIATE> or <LABEL>.

  * `lbu` <REGISTER>, <REGISTER>, <IMMEDIATE>|<LABEL>:
    Loads a byte from memory at the address which is the sum of the second
    <REGISTER> and the <IMMEDIATE> or <LABEL>. The byte is zero extended to 32
    bits.

  * `lhu` <REGISTER>, <REGISTER>, <IMMEDIATE>|<LABEL>:
    Loads a half from memory at the address which is the sum of the second
    <REGISTER> and the <IMMEDIATE> or <LABEL>. The half is zero extended to 32
    bits.

  * `sb` <REGISTER>, <REGISTER>, <IMMEDIATE>|<LABEL>:
    Stores a byte into memory at the address which is the sum of the second
    <REGISTER> and the <IMMEDIATE> or <LABEL>.

  * `sh` <REGISTER>, <REGISTER>, <IMMEDIATE>|<LABEL>:
    Stores a half into memory at the address which is the sum of the second
    <REGISTER> and the <IMMEDIATE> or <LABEL>.

  * `sw` <REGISTER>, <REGISTER>, <IMMEDIATE>|<LABEL>:
    Stores a word into memory at the address which is the sum of the second
    <REGISTER> and the <IMMEDIATE> or <LABEL>.

These instructions are used to control the logic flow of the program:

PC-Relative addressing is used for all instructions but `jalr`. For `jalr`
absolute addressing is used.

  * `beq` <REGISTER>, <REGISTER>, <IMMEDIATE>|<LABEL>:
    Branch if the <REGISTER>s are equal.

  * `bne` <REGISTER>, <REGISTER>, <IMMEDIATE>|<LABEL>:
    Branch if the <REGISTER>s are not equal.

  * `blt` <REGISTER>, <REGISTER>, <IMMEDIATE>|<LABEL>:
    Branch if the content of the first <REGISTER> is less than the content of
    the last <REGISTER>. The content of the <REGISTER>s are interpreted as
    signed numbers.

  * `bltu` <REGISTER>, <REGISTER>, <IMMEDIATE>|<LABEL>:
    Branch if the content of the first <REGISTER> is less than the content of
    the last <REGISTER>. The content of the <REGISTER>s are interpreted as
    unsigned numbers.

  * `bge` <REGISTER>, <REGISTER>, <IMMEDIATE>|<LABEL>:
    Branch if the content of the first <REGISTER> is greater than or equal to
    the content of the last <REGISTER>. The content of the <REGISTER>s are
    interpreted as signed numbers.

  * `bgeu` <REGISTER>, <REGISTER>, <IMMEDIATE>|<LABEL>:
    Branch if the content of the first <REGISTER> is greater than or equal to
    the content of the last <REGISTER>. The content of the <REGISTER>s are
    interpreted as unsigned numbers.

  * `jal` <REGISTER>, <IMMEDIATE>|<LABEL>:
    Jump and link to the address which is the sum of the program counter and the
    <IMMEDIATE> or <LABEL>. The return address is written to the <REGISTER>.

  * `jalr` <REGISTER>, <REGISTER>, <IMMEDIATE>|<LABEL>:
    Jump and link to the address which is the sum of the second <REGISTER> and
    the <IMMEDIATE> or <LABEL>. The return address is written to the <REGISTER>.

These instructions cannot be categorized:

  * `lui` <REGISTER>, <IMMEDIATE>|<LABEL>:
    Load upper immediate. The upper 20 bits of the <REGISTER> is set to the
    <IMMEDIATE> or <LABEL>. The lower 12 bits are zero.

  * `auipc` <REGISTER>, <IMMEDIATE>|<LABEL>:
    Add upper immediate to program counter. The upper 20 bits of the <IMMEDIATE>
    or <LABEL> is added to the program counter and the result is written to the
    <REGISTER>.

See
[RISC-V Specification](https://www.cs.sfu.ca/~ashriram/Courses/CS295/assets/notebooks/RISCV/RISCV_CARD.pdf)
(<https://www.cs.sfu.ca/~ashriram/Courses/CS295/assets/notebooks/RISCV/RISCV_CARD.pdf>)
for details.

## SEE ALSO

assembler(1),
[RISC-V Specification](https://www.cs.sfu.ca/~ashriram/Courses/CS295/assets/notebooks/RISCV/RISCV_CARD.pdf)
(<https://www.cs.sfu.ca/~ashriram/Courses/CS295/assets/notebooks/RISCV/RISCV_CARD.pdf>)
