; Greatest common divisor (Größter gemeinsamer Teiler)
; Dieses Programm sucht den GGT der beiden Zahlen in a0 und a1
    addi    a0, zero, 120
    addi    a1, zero, 0
    beq     a0, zero, RETABSA1
    beq     a1, zero, RETABSA0
LOOP:
    rem     t0, a0, a1
    mv      a0, a1
    mv      a1, t0
    bne     a1, zero, LOOP
    j       RETABSA0
RETABSA1:
    mv      a0, a1
RETABSA0:
    mv      a1, zero
    bge     a0, zero, LOD
    neg     a0, a0
LOD:
    j       LOD
