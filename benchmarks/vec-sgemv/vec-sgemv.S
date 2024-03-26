    .text
    .balign 4
    .global vec_sgemv
# RV64IDV system
#
# void
# vec_sgemv(size_t m,
#          size_t n,
#          const float* v,   // m-length vector
#          const float* m,   // m * n matrix
#          float*c)          // m-length vector
#
#  c += a*b (V^T * M)
#  matrix stored in row-major order

#define m a0
#define n a1
#define vp a2
#define mp a3
#define cp a4

#define vt t1
#define mpm t2
#define nvl t3
#define nstride t4
#define mt t5
#define nvlb t6


vec_sgemv:
    # Check for zero size matrices        
    beqz m, exit
    beqz n, exit

    # Convert elements strides to byte strides.
    slli nstride, n, 2

    slti mt, mp, 2
    bnez mt, exit

c_col_loop:
    vsetvli nvl, n, e32, m8, ta, ma # 32-bit vectors, LMUL=8
    
    mv mt, m   # reset the row pointer
    mv vt, vp  # reset the vector pointer
    
    # Load vector from the C matrix
    vle32.v v16, (cp) 

    vle32.v v0, (mp)
    add mpm, mp, nstride
    vle32.v v8, (mpm)
    add mpm, mpm, nstride

    flw ft0, (vp)
    flw ft1, 4(vp)
    addi vt, vp, 8

m_loop:
    vfmacc.vf v16, ft0, v0      # Compute the first FMA of the loop against V scalar
    vle32.v v0, (mpm)           # Load the next row of the matrix
    flw ft0, (vt)               # Load the next scalar from V
    add mpm, mpm, nstride       # Bump the M pointer for the next M vector load
    addi mt, mt, -2             # Completing two rows of M in this loop
    vfmacc.vf v16, ft1, v8      # Compute the second FMA of the loop
    vle32.v v8, (mpm)           # Load the next row of the matrix
    add mpm, mpm, nstride       # Bump the M pointer
    flw ft1, 4(vt)              # Load the next V scalar
    addi vt, vt, 8              # Bump the pointer for the next scalar load
    slti t0, mt, 4
    bnez t0, 1f
    j m_loop

1:  vfmacc.vf v16, ft0, v0 
    addi mt, mt, -2
    vfmacc.vf v16, ft1, v8

    beqz mt, 1f

    vle32.v v0, (mpm)
    flw ft0, (vt)
    vfmacc.vf v16, ft0, v0

1:  vse32.v v16, (cp)           # Store the vector of results
    
    slli nvlb, nvl, 2           # Current vl in bytes 
    add cp, cp, nvlb            # Bump the output pointer
    add mp, mp, nvlb            # Bump the matrix pointer to the next set of columns

    sub n, n, nvl               # Track how much of output we've computed
    bnez n, c_col_loop          # Done with entire computation?

exit:
    ret
