; Fibonacci
; Berechnet die n-te (hier 17) Fibonacci Zahl
    addi    a0, zero, 17
    addi    t3, zero, 3
    blt     a0, t3, LESSTHAN3       ; branch if num < 3
    addi    a2, a0, -1
    addi    t0, zero, 0
    addi    t1, zero, 1
FIB:
    add     a0, t1, t0              ; sum last two numbers
    mv      t0, t1
    mv      t1, a0
    addi    a2, a2, -1
    bne     zero, a2, FIB           ; loop until counter reaches 0
LOD:
    j LOD                           ; LOOP OF DEATH
LESSTHAN3:                          ; Handle first 3 numbers - 0 == 0, 1 == 1, 2 == 1
    andi    t3, a0, 2
    beq     t3, zero, LOD
    addi    a0, zero, 1
    j       LOD