  .text
  .balign 4
  .global vec_div_approx

# v1 = v1 / v2 to almost 23 bits of precision.

vec_div_approx: 
  vsetvli t1, a0, e32, m1, ta, mu
  vle32.v v1, (a1) # load x values
  vle32.v v2, (a2) # load y values
    sub a0, a0, t1
    slli t1, t1, 2
  vfrec7.v v3, v2 # Estimate 1/v2
    li t0, 0x40000000
  vmv.v.x v4, t0 # Splat 2.0
  vfnmsac.vv v4, v2, v3 # 2.0 - v2 * est(1/v2)
  vfmul.vv v3, v3, v4 # Better estimate of 1/v2
  vmv.v.x v4, t0 # Splat 2.0
  vfnmsac.vv v4, v2, v3 # 2.0 - v2 * est(1/v2)
  vfmul.vv v3, v3, v4 # Better estimate of 1/v2
  vfmul.vv v1, v1, v3 # Estimate of v1/v2
  vse32.v v1, (a1)
    add a2, a2, t1 # Bump pointer
    add a1, a1, t1 # Bump pointer
    bnez a0, vec_div_approx
    ret
