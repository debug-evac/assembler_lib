    addi    a2, zero, 17
    addi    a0, zero, 0
    addi    a1, zero, 1
    addi    a3, zero, 2
    beq     zero, a2, END
FIB: 
    add     a0, a0, a1
    mul     a1, a1, a3
    addi    a2, a2, -1
    bne     zero, a2, FIB
END: 
    ret
