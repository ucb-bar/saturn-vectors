    .text
    .balign 4
    .global vec_transpose
# RV64IDV system
#
# void
# vec_transpose(size_t n,
#          size_t m,
#          const float*a,   // m * n matrix
#          float*b,         // n * m matrix
#

############### UNOPTIMIZED ######################

#define n a0
#define m a1
#define ap a2
#define bp a3

#define astride t0
#define bstride t1
#define nvl t2
#define amp t3
#define bmp t4
#define mt t5
#define nt t6

#define bnp a4


vec_transpose:
    # Check for zero size matrices        
    beqz n, exit
    beqz m, exit

    # Convert elements strides to byte strides.
    slli astride, n, 2
    slli bstride, m, 2

    slti t6, m, 4
    bnez t6, end_rows

a_row_loop:
    mv mt, m
    
    mv amp, ap
    mv bmp, bp

a_col_loop:
    vsetvli nvl, mt, e32, m1, ta, ma

    mv bnp, bmp

    // Load the input matrix using strided segment loads
    vlsseg4e32.v v0, (amp), astride

    // Store the transposed output matrix using unit stride stores
    vse32.v v0, (bnp)
    add bnp, bnp, bstride
    vse32.v v1, (bnp)
    add bnp, bnp, bstride
    vse32.v v2, (bnp)
    add bnp, bnp, bstride
    vse32.v v3, (bnp)

    slli a5, nvl, 2
    add bmp, bmp, a5

    mul a5, astride, nvl
    add amp, amp, a5

    sub mt, mt, nvl

    bnez mt, a_col_loop

    addi n, n, -4

    slli a5, bstride, 2
    add bp, bp, a5

    addi ap, ap, 16

    bnez n, a_row_loop


end_rows:

exit:
    ret
