    .text
    .balign 4
    .global vec_sgemm_nn
# RV64IDV system
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

# Use args as additional temporaries
#define ft12 fa0
#define ft13 fa1
#define ft14 fa2
#define ft15 fa3

#define FRAMESIZE 32

# This version holds a 4*VLMAX*4 block of C matrix in vector registers
# in inner loop, but otherwise does not cache or TLB tiling.

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
    bnez t6, end_rows

c_row_loop: # Loop across rows of C blocks

    mv nt, n  # Initialize n counter for next row of C blocks

    mv bnp, bp # Initialize B n-loop pointer to start
    mv cnp, cp # Initialize C n-loop pointer

c_col_loop: # Loop across one row of C blocks
    vsetvli nvl, nt, e32, m4, ta, ma  # 32-bit vectors, LMUL=4

    # Initalize current C submatrix block from memory.
    vle32.v  v0, (cnp); add ccp, cnp, cstride;
    mv akp, ap   # reset pointer into A to beginning
    vle32.v  v4, (ccp); add ccp, ccp, cstride;
    mv bkp, bnp  # step to next column in B matrix
    vle32.v  v8, (ccp); add ccp, ccp, cstride;
    vle32.v  v12, (ccp);

    # Get vector from B matrix
    vle32.v v16, (bkp)
    mv kt, k # Initialize inner loop counter

    # Inner loop scheduled assuming 4-clock occupancy of vfmacc instruction and single-issue pipeline
    # Software pipeline loads
    flw ft0, (akp); add amp, akp, astride;
    flw ft1, (amp); add amp, amp, astride;
    flw ft2, (amp); add amp, amp, astride;
    flw ft3, (amp); add amp, amp, astride;

    # Loop on inner dimension for current C block
k_loop:
    vfmacc.vf v0, ft0, v16
    add bkp, bkp, bstride
    addi kt, kt, -1            # Decrement k counter
    addi akp, akp, 4
    beqz kt, 1f
    flw ft0, (akp)
    add amp, akp, astride
1:  vfmacc.vf v4, ft1, v16
    beqz kt, 1f
    flw ft1, (amp)
    add amp, amp, astride
1:  vfmacc.vf v8, ft2, v16
    beqz kt, 1f
    flw ft2, (amp)
    add amp, amp, astride
1:  vfmacc.vf v12, ft3, v16
    beqz kt, 1f                   # Exit the loop
    flw ft3, (amp)
    add amp, amp, astride
    vle32.v v16, (bkp)            # Get next vector from B matrix, overlap loads with jump stalls
    j k_loop

1:  vse32.v  v0, (cnp); add ccp, cnp, cstride;
    slli t6, nvl, 2
    vse32.v  v4, (ccp); add ccp, ccp, cstride;
    add cnp, cnp, t6                         # Move C block pointer over
    vse32.v  v8, (ccp); add ccp, ccp, cstride;
    add bnp, bnp, t6                         # Move B block pointer over
    sub nt, nt, nvl                          # Decrement element count in n dimension
    vse32.v v12, (ccp);

    # Following tail instructions should be scheduled earlier in free slots during C block save.
    # Leaving here for clarity.

    # Bump pointers for loop across blocks in one row
    bnez nt, c_col_loop                      # Any more to do?

    # Move to next set of rows
    addi m, m, -4        # Did 4 rows above
    slli t6, astride, 2  # Multiply astride by 4
    add ap, ap, t6       # Move A matrix pointer down 4 rows
    slli t6, cstride, 2  # Multiply cstride by 4
    add cp, cp, t6       # Move C matrix pointer down 4 rows
    
    slti t6, m, 4
    beqz t6, c_row_loop

    # Handle end of matrix with fewer than 4 rows.
    # Can use smaller versions of above decreasing in powers-of-2 depending on code-size concerns.
end_rows:
    # Not done.

exit:
    ld s0, 0(sp)
    ld s1, 8(sp)
    ld s2, 16(sp)
    ld s3, 24(sp)
    addi sp, sp, FRAMESIZE
    ret
