; Fakultät
; Dieses Programm multipliziert alle Zahlen von 30 bis einschließlich 1 auf
; Das Ergebnis ist eine 128 bit Zahl, welche im LE Format in den Speicher geschrieben wird
.data
.VAL:
    .dword 0, 0
.text
    addi    a4, zero, 30
    mv      a0, a4
    srli    a5, a4, 1
    beq     a5, zero, LOD
    addi    a4, a4, -1
FAK:
    mul	    a3, a3, a4		; A x E
    mulhu   a5, a2, a4		; Carry of B x E
    add     a3, a3, a5		; Add carry to A x E
    mul     a2, a2, a4		; B x E
    mulhu   a5, a1, a4		; Carry of C x E
    add	    a2, a2, a5		; Add carry to B x E
    mul	    a1, a1, a4		; C x E
    mulhu   a5, a0, a4		; Carry of D x E
    add	    a1, a1, a5		; Add carry to C x E
    mul	    a0, a0, a4		; D x E
    addi    a4, a4, -1
    bne	    a4, zero, FAK
    la	    a4, VAL         ; Store result in memory and return pointer to value
    sw	    a0, a4, 0
    sw	    a1, a4, 4
    sw      a2, a4, 8
    sw	    a3, a4, 12
    mv	    a1, zero
    mv	    a0, a4
LOD:
    j       LOD
