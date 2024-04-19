  .text
  .balign 4
  .global vec_div_approx

# v1 = v1 / v2 to almost 23 bits of precision.

vec_div_approx: 
  vsetvli t1, a0, e32, m4, ta, mu
  vle32.v v4, (a1) # load x values
  vle32.v v8, (a2) # load y values
    sub a0, a0, t1
    slli t1, t1, 2
  vfrec7.v v12, v8 # Estimate 1/v2
    li t0, 0x40000000
  vmv.v.x v16, t0 # Splat 2.0
  vfnmsac.vv v16, v8, v12 # 2.0 - v2 * est(1/v2)
  vfmul.vv v12, v12, v16 # Better estimate of 1/v2
  vmv.v.x v16, t0 # Splat 2.0
  vfnmsac.vv v16, v8, v12 # 2.0 - v2 * est(1/v2)
  vfmul.vv v12, v12, v16 # Better estimate of 1/v2
  vfmul.vv v4, v4, v12 # Estimate of v1/v2
  vse32.v v4, (a1)
    add a2, a2, t1 # Bump pointer
    add a1, a1, t1 # Bump pointer
    bnez a0, vec_div_approx
    ret
