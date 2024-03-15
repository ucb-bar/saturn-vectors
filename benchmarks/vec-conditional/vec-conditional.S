  .text
  .balign 4
  .global vec_conditional

# (int16) z[i] = ((int8) x[i] < 5) ? (int16) a[i] : (int16) b[i];
#
vec_conditional:
  vsetvli t0, a0, e8, m1, ta, ma # Use 8b elements.
  vle8.v v0, (a1)         # Get x[i]
    sub a0, a0, t0        # Decrement element count
    add a1, a1, t0        # x[i] Bump pointer
  vmslt.vi v0, v0, 5      # Set mask in v0
  vsetvli x0, x0, e16, m2, ta, mu  # Use 16b elements.
    slli t0, t0, 1        # Multiply by 2 bytes
  vle16.v v2, (a2), v0.t  # z[i] = a[i] case
  vmnot.m v0, v0          # Invert v0
    add a2, a2, t0        # a[i] bump pointer
  vle16.v v2, (a3), v0.t  # z[i] = b[i] case
    add a3, a3, t0        # b[i] bump pointer
  vse16.v v2, (a4)        # Store z
    add a4, a4, t0        # z[i] bump pointer
    bnez a0, vec_conditional # Any more?
    ret
    