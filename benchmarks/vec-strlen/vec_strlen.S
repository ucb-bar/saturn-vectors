.text
.balign 4
.global strlen_rvv
strlen_rvv:
    mv a3, a0
loop:
    vsetvli a1, x0, e8, m8, ta, ma
    vle8ff.v v8, (a3)
    csrr a1, vl
    vmseq.vi v0, v8, 0
    vfirst.m a2, v0
    add a3, a3, a1
    bltz a2, loop
    add a0, a0, a1
    add a3, a3, a2
    sub a0, a3, a0
    ret
