    addi    a1, zero, 12
    add     a0, zero, a1
FAK:    
    addi    a1, a1, -1
    mul     a0, a0, a1
    bne     a1, zero, FAK
    ret
