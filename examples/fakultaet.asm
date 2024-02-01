; Fakultät
; Dieses Programm multipliziert alle Zahlen von 12 bis einschließlich 1 auf
    addi    a2, zero, 12
    mv      a0, a2
    srli    a3, a2, 1
    beq     a3, zero, LOD
    addi    a2, a2, -1
FAK:    
    mul     a0, a0, a2
    addi    a2, a2, -1
    bne     a2, zero, FAK
LOD:
    j	    LOD