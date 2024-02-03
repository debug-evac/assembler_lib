; Fakultät
; Dieses Programm multipliziert alle Zahlen von 17 bis einschließlich 1 auf
; Das Ergebnis ist eine 64 bit Zahl (C Datentyp long long)
    addi    a2, zero, 17
    mv      a0, a2
    srli    a3, a2, 1
    beq     a3, zero, LOD
    addi    a2, a2, -1
FAK:
    mul     a1, a1, a2
    mulhu   a3, a0, a2
    add     a1, a1, a3
    mul     a0, a0, a2
    addi    a2, a2, -1
    bne     a2, zero, FAK
LOD:
    j       LOD
