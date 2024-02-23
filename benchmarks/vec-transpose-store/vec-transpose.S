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
#define bnp t4
#define mt t5
#define nt t6
#define anp a4


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
    mv nt, n
    
    mv anp, ap
    mv bnp, bp

a_col_loop:
    vsetvli nvl, nt, e32, m1, ta, ma

    mv amp, anp

    // Load the input matrix with unit stride
    vle32.v v0, (amp)
    add amp, amp, astride
    vle32.v v1, (amp)
    add amp, amp, astride
    vle32.v v2, (amp)
    add amp, amp, astride
    vle32.v v3, (amp)

    // Output the transpose using strided segment store
    vssseg4e32.v v0, (bnp), bstride

    slli a5, nvl, 2
    add anp, anp, a5

    mul a5, bstride, nvl
    add bnp, bnp, a5

    sub nt, nt, nvl

    bnez nt, a_col_loop

    mv nt, n
    addi m, m, -4

    slli a5, astride, 2
    add ap, ap, a5

    addi bp, bp, 16

    bnez m, a_row_loop


end_rows:
    # Not done

exit:
    ret
