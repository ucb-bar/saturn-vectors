  .text
  .balign 4
  .global vec_root_approx

# v1 = sqrt(v1) to almost 23 bits of precision.

vec_root_approx: 
  vsetvli t1, a0, e32, m1, ta, mu
  vle32.v v1, (a1) # load x values
    sub a0, a0, t1
    slli t1, t1, 2
    fmv.w.x ft0, x0           # Mask off zero inputs
  vmfne.vf v0, v1, ft0        #   to avoid div by zero
  vfrsqrt7.v v2, v1, v0.t     # Estimate 1/sqrt(x)
  vmfne.vf v0, v2, ft0, v0.t  # Additionally mask off +inf inputs
    li t0, 0x40400000
  vmv.v.x v4, t0              # Splat 3.0
  vfmul.vv v3, v1, v2, v0.t   # x * est
  vfnmsub.vv v3, v2, v4, v0.t # - x * est * est + 3
  vfmul.vv v3, v3, v2, v0.t   # est * (-x * est * est + 3)
    li t0, 0x3f000000
    fmv.w.x ft0, t0           # 0.5
  vfmul.vf v2, v3, ft0, v0.t  # Estimate to 14 bits
  vfmul.vv v3, v1, v2, v0.t   # x * est
  vfnmsub.vv v3, v2, v4, v0.t # - x * est * est + 3
  vfmul.vv v3, v3, v2, v0.t   # est * (-x * est * est + 3)
  vfmul.vf v2, v3, ft0, v0.t  # Estimate to 23 bits
  vfmul.vv v1, v2, v1, v0.t   # x * 1/sqrt(x)
  vse32.v v1, (a1)
    add a1, a1, t1 # Bump pointer
    bnez a0, vec_root_approx # Any more?
    ret