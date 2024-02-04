; Hardware test program
; Can be used to attest that the underlying hardware is working correctly
; This program uses (means executing) at least THREE instructions of each type
; This program writes 0xDEADBEEF at Register x6 (t1) and at some memory location
; (not really that important)
    li      sp, 4000
TEST:
    xori    t3, zero, 111
    la      s2, TEST
    andi    s2, s2, 31
    slti    s3, s2, 30
    addi    s4, zero, 9
    slli    s3, s3, 4
    add     s3, s3, s4
    sll     t2, t3, s3
    or      t2, t2, t3
    addi    sp, sp, -4
    sw      t2, sp, 0
    and     t2, t2, zero
    addi    t3, zero, 2
TWOLOOP:
    slli    t1, t1, 2
    slti    t2, t2, 80
    slli    t2, t2, 1
    add     t1, t1, t2
    addi    t3, t3, -1
    bne     t3, zero, TWOLOOP
    addi    t2, zero, 4
    sll     t1, t1, t2
    addi    t1, t1, 13
    sb      t1, sp, 2
    lhu     t1, sp, 0
    addi    t2, zero, 2
    addi    t3, zero, 2
    addi    t4, zero, 4
FOURLOOP:
    mul     t2, t2, t3
    addi    t4, t4, -1
    beq     t4, zero, FORWARD
    jal     FOURLOOP
FORWARD:
    addi    t2, t2, -1
    addi    t3, zero, 2
    slli    t3, t3, 5
    add     t2, t2, t3
    slli    t2, t2, 2
    sltiu   t4, zero, 2
    or      t2, t2, t4
    slli    t2, t2, 7
    or      t1, t1, t2
    sh      t1, sp, 0
    lw      t1, sp, 0
