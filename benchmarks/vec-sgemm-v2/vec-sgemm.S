    .text
    .balign 4
    .global vec_sgemm_nn
    .type vec_sgemm_nn,@function
#
# void
# vec_sgemm_nn(size_t n,
#          size_t m,
#          size_t k,
#          const float*a,   // m * k matrix
#          size_t lda,
#          const float*b,   // k * n matrix
#          size_t ldb,
#          float*c,         // m * n matrix
#          size_t ldc)
#
#  c += a*b (alpha=1, no transpose on input matrices)
#  matrices stored in C row-major order



# With LMUL=4, load 4 rows of C and 4 rows of B
# Load 16 scalars from A into FP registers

#define n a0
#define m a1
#define k a2
#define ap a3
#define astride a4
#define bp a5
#define bstride a6
#define cp a7
#define cstride t0
#define kt t1
#define nt t2
#define bnp t3
#define cnp t4
#define akp t5
#define bkp s0
#define nvl s1
#define ccp s2
#define amp s3

#define a00 ft0
#define a10 ft1
#define a20 ft2
#define a30 ft3
#define a01 ft4
#define a11 ft5
#define a21 ft6
#define a31 ft7
#define a02 fa0
#define a12 fa1
#define a22 fa2
#define a32 fa3
#define a03 fa4
#define a13 fa5
#define a23 fa6
#define a33 fa7

#define FRAMESIZE 32

vec_sgemm_nn:
    ld cstride, 0(sp)   # Get arg from stack frame
    addi sp, sp, -FRAMESIZE
    sd s0, 0(sp)
    sd s1, 8(sp)
    sd s2, 16(sp)
    sd s3, 24(sp)

    # Check for zero size matrices        
    beqz n, exit
    beqz m, exit
    beqz k, exit

    # Convert elements strides to byte strides.
    slli astride, astride, 2
    slli bstride, bstride, 2
    slli cstride, cstride, 2

    slti t6, m, 4
    bnez t6, m_remainder_m_loop

c_row_loop:   # Loop across rows of C blocks
    mv nt, n  # Initialize n counter for next row of C blocks
    mv bnp, bp # Initialize B n-loop pointer to start
    mv cnp, cp # Initialize C n-loop pointer

c_col_loop:  # Loop across columns of C
    vsetvli nvl, nt, e32, m4, ta, ma  # 32-bit vectors, LMUL=4

    # Not enough remaining k elements to unroll by 4
    mv kt, k # Initialize inner loop counter
    slti t6, kt, 4 
    bnez t6, k_loop_remainder

    mv akp, ap   # reset pointer into A to beginning
    mv bkp, bnp  # step to next column in B matrix

    # Initalize current C submatrix block from memory.
    vle32.v  v0, (cnp); add ccp, cnp, cstride;
    flw a00, (akp); add amp, akp, astride;
    flw a10, (amp); add amp, amp, astride;
    vle32.v  v4, (ccp); add ccp, ccp, cstride;
    flw a20, (amp); add amp, amp, astride;
    flw a30, (amp); add akp, akp, 4
    vle32.v  v8, (ccp); add ccp, ccp, cstride;
    flw a01, (akp); add amp, akp, astride;
    flw a11, (amp); add amp, amp, astride;
    vle32.v  v12, (ccp);
    flw a21, (amp); add amp, amp, astride;
    flw a31, (amp); add akp, akp, 4

    # Get vector from B matrix
    vle32.v v16, (bkp); add bkp, bkp, bstride
    flw a02, (akp); add amp, akp, astride;
    flw a12, (amp); add amp, amp, astride;
    vle32.v v20, (bkp); add bkp, bkp, bstride
    flw a22, (amp); add amp, amp, astride;
    flw a32, (amp); add akp, akp, 4
    vle32.v v24, (bkp); add bkp, bkp, bstride
    flw a03, (akp); add amp, akp, astride;
    flw a13, (amp); add amp, amp, astride;
    vle32.v v28, (bkp); add bkp, bkp, bstride
    flw a23, (amp); add amp, amp, astride;
    flw a33, (amp); add akp, akp, 4

    # Inner loop scheduled assuming 4-clock occupancy of vfmacc instruction and single-issue pipeline
    # Software pipeline loads

k_loop:
    # Compute current block of FMAs
    vfmacc.vf v0, a00, v16 
    vfmacc.vf v0, a01, v20 
    vfmacc.vf v0, a02, v24 
    vfmacc.vf v0, a03, v28

    vfmacc.vf v4, a10, v16 
    vfmacc.vf v4, a11, v20 
    vfmacc.vf v4, a12, v24 
    vfmacc.vf v4, a13, v28 

    vfmacc.vf v8, a20, v16 
    vfmacc.vf v8, a21, v20 
    vfmacc.vf v8, a22, v24 
    vfmacc.vf v8, a23, v28 

    vfmacc.vf v12, a30, v16 
    vfmacc.vf v12, a31, v20 
    vfmacc.vf v12, a32, v24 
    vfmacc.vf v12, a33, v28 

    addi kt, kt, -4
    blez kt, k_loop_remainder

    # Load values from A for the next iteration
    flw a00, (akp); add amp, akp, astride;
    flw a10, (amp); add amp, amp, astride;
    flw a20, (amp); add amp, amp, astride;
    flw a30, (amp); add akp, akp, 4
    flw a01, (akp); add amp, akp, astride;
    flw a11, (amp); add amp, amp, astride;
    flw a21, (amp); add amp, amp, astride;
    flw a31, (amp); add akp, akp, 4
    flw a02, (akp); add amp, akp, astride;
    flw a12, (amp); add amp, amp, astride;
    flw a22, (amp); add amp, amp, astride;
    flw a32, (amp); add akp, akp, 4
    flw a03, (akp); add amp, akp, astride;
    flw a13, (amp); add amp, amp, astride;
    flw a23, (amp); add amp, amp, astride;
    flw a33, (amp); add akp, akp, 4

    vle32.v v16, (bkp); add bkp, bkp, bstride
    vle32.v v20, (bkp); add bkp, bkp, bstride
    vle32.v v24, (bkp); add bkp, bkp, bstride
    vle32.v v28, (bkp); add bkp, bkp, bstride
    
    j k_loop

k_loop_remainder:
    beqz kt, 1f
    addi kt, kt, 4

k_loop_remainder_loop:
    # Proceed at one k element per loop
    addi kt, kt, -1
    vle32.v v16, (bkp)
    flw a00, (akp); add amp, akp, astride;
    flw a10, (amp); add amp, amp, astride;
    flw a20, (amp); add amp, amp, astride;
    flw a30, (amp)

    vfmacc.vf v0, a00, v16 
    vfmacc.vf v4, a10, v16 
    vfmacc.vf v8, a20, v16 
    vfmacc.vf v12, a30, v16 

    addi akp, akp, 4
    add bkp, bkp, bstride

    bnez kt, k_loop_remainder_loop

1:  vse32.v  v0, (cnp); add ccp, cnp, cstride;
    vse32.v  v4, (ccp); add ccp, ccp, cstride;
    vse32.v  v8, (ccp); add ccp, ccp, cstride;
    vse32.v v12, (ccp)

    slli t6, nvl, 2
    add cnp, cnp, t6
    add bnp, bnp, t6
    sub nt, nt, nvl                          # Decrement element count in n dimension
    bnez nt, c_col_loop

    # Move to the next set of rows
    addi m, m, -4

    slli t6, astride, 2  # Multiply astride by 4
    add ap, ap, t6       # Move A matrix pointer down 4 rows
    slli t6, cstride, 2  # Multiply cstride by 4
    add cp, cp, t6       # Move C matrix pointer down 4 rows
    
    slti t6, m, 4
    beqz t6, c_row_loop
    
    beqz m, exit

m_remainder_m_loop:
    mv cnp, cp
    mv bnp, bp
    mv nt, n  

m_remainder_n_loop:
    vsetvli nvl, nt, e32, m4, ta, ma  # 32-bit vectors, LMUL=4

    # Not enough remaining k elements to unroll by 4
    mv kt, k # Initialize inner loop counter
    slti t6, kt, 4 
    bnez t6, m_remainder_k_loop_remainder

    mv akp, ap   # reset pointer into A to beginning
    mv bkp, bnp  # step to next column in B matrix

    vle32.v  v0, (cnp)

    # Get vectors from B matrix
    vle32.v v16, (bkp); add bkp, bkp, bstride
    vle32.v v20, (bkp); add bkp, bkp, bstride
    vle32.v v24, (bkp); add bkp, bkp, bstride
    vle32.v v28, (bkp); add bkp, bkp, bstride

    # Inner loop scheduled assuming 4-clock occupancy of vfmacc instruction and single-issue pipeline
    # Software pipeline loads
    flw a00, (akp); addi akp, akp, 4
    flw a01, (akp); addi akp, akp, 4
    flw a02, (akp); addi akp, akp, 4
    flw a03, (akp); addi akp, akp, 4

m_remainder_k_loop:
    vfmacc.vf v0, a00, v16
    vfmacc.vf v0, a01, v20
    vfmacc.vf v0, a02, v24
    vfmacc.vf v0, a03, v28

    addi kt, kt, -4
    blez kt, m_remainder_k_loop_remainder
    
    flw a00, (akp); addi akp, akp, 4
    flw a01, (akp); addi akp, akp, 4
    flw a02, (akp); addi akp, akp, 4
    flw a03, (akp); addi akp, akp, 4

    vle32.v v16, (bkp); add bkp, bkp, bstride
    vle32.v v20, (bkp); add bkp, bkp, bstride
    vle32.v v24, (bkp); add bkp, bkp, bstride
    vle32.v v28, (bkp); add bkp, bkp, bstride

    j m_remainder_k_loop

m_remainder_k_loop_remainder:
    addi kt, kt, 4
    beqz kt, 1f

m_remainder_k_loop_remainder_loop:

    addi kt, kt, -1
    vle32.v v16, (bkp)
    flw a00, (akp); add amp, akp, astride;

    vfmacc.vf v0, a00, v16 

    addi akp, akp, 4
    add bkp, bkp, bstride

    bnez kt, m_remainder_k_loop_remainder_loop

1:  vse32.v v0, (cnp)
    
    slli t6, nvl, 2
    add cnp, cnp, t6
    add bnp, bnp, t6
    sub nt, nt, nvl
    bnez nt, m_remainder_n_loop

    addi m, m, -1
    add ap, ap, astride
    add cp, cp, cstride

    bnez m, m_remainder_m_loop 

exit:
    ld s0, 0(sp)
    ld s1, 8(sp)
    ld s2, 16(sp)
    ld s3, 24(sp)
    addi sp, sp, FRAMESIZE
    ret
