// RVV vectorized memcpy kernel.
//
// SPDX-License-Identifier: BSD-3-Clause
// (c) the riscv-rvv-intrinsic-doc contributors.
// Vendored (logic verbatim) from riscv-non-isa/riscv-rvv-intrinsic-doc,
// examples/rvv_memcpy.c @ e6bc0ae. Full license text in that repo's COPYING.BSD.
//
// C++ adaptations only: restrict -> __restrict__, explicit void* casts, static inline.

#ifndef RVV_MEMCPY_H
#define RVV_MEMCPY_H

#include <riscv_vector.h>
#include <stddef.h>

static inline void *memcpy_vec(void *__restrict__ destination,
                               const void *__restrict__ source, size_t n) {
  unsigned char *dst = (unsigned char *)destination;
  const unsigned char *src = (const unsigned char *)source;
  for (size_t vl; n > 0; n -= vl, src += vl, dst += vl) {
    vl = __riscv_vsetvl_e8m8(n);
    vuint8m8_t vec_src = __riscv_vle8_v_u8m8(src, vl);
    __riscv_vse8_v_u8m8(dst, vec_src, vl);
  }
  return destination;
}

#endif // RVV_MEMCPY_H
