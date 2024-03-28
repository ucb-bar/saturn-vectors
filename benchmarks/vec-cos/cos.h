// Modified version of:
// "RISC-V VECTOR COS FUNCTION Version by Cristóbal Ramírez Lazo, "Barcelona
// 2019"" Find details on the original version below Author: Matteo Perotti
// <mperotti@iis.ee.ethz.ch>

// RISC-V VECTOR COS FUNCTION Version by Cristóbal Ramírez Lazo, "Barcelona
// 2019" This RISC-V Vector implementation is based on the original code
// presented by Julien Pommier

/*
   AVX implementation of sin, cos, sincos, exp and log
   Based on "sse_mathfun.h", by Julien Pommier
   http://gruntthepeon.free.fr/ssemath/
   Copyright (C) 2012 Giovanni Garberoglio
   Interdisciplinary Laboratory for Computational Science (LISC)
   Fondazione Bruno Kessler and University of Trento
   via Sommarive, 18
   I-38123 Trento (Italy)
  This software is provided 'as-is', without any express or implied
  warranty.  In no event will the authors be held liable for any damages
  arising from the use of this software.
  Permission is granted to anyone to use this software for any purpose,
  including commercial applications, and to alter it and redistribute it
  freely, subject to the following restrictions:
  1. The origin of this software must not be misrepresented; you must not
     claim that you wrote the original software. If you use this software
     in a product, an acknowledgment in the product documentation would be
     appreciated but is not required.
  2. Altered source versions must be plainly marked as such, and must not be
     misrepresented as being the original software.
  3. This notice may not be removed or altered from any source distribution.
  (this is the zlib license)
*/

#include <stdio.h>
#include <string.h>
#include <riscv_vector.h>

#define COS64_INLINE(m,md)                                              \
void cos_f64m##m##_bmark(double *angles, double *results, size_t len);  \
static inline vfloat64m##m##_t __cos_f64m##m(vfloat64m##m##_t x, size_t gvl) {  \
  int64_t _ps_inv_sign_mask = ~0x8000000000000000;                      \
  double _ps_cephes_FOPI = 1.27323954473516; /* 4 / M_PI */             \
  int64_t _pi32_1 = 1;                                                  \
  int64_t _pi32_inv1 = ~0x0000000000000001;                             \
  int64_t _pi32_2 = 2;                                                  \
  int64_t _pi32_4 = 4;                                                  \
  int64_t _Zero = 0;                                                    \
                                                                        \
  vfloat64m##m##_t xmm2;                                                \
  vfloat64m##m##_t xmm1;                                                \
  vfloat64m##m##_t xmm3;                                                \
  vfloat64m##m##_t y;                                                   \
                                                                        \
  vint64m##m##_t emm0;                                                  \
  vint64m##m##_t emm2;                                                  \
                                                                        \
  vbool##md##_t xMask;                                                  \
  /* take the absolute value */                                         \
  vint64m##m##_t xf = __riscv_vreinterpret_v_f64m##m##_i64m##m(x);	\
  vint64m##m##_t xa = __riscv_vand_vx_i64m##m(xf, _ps_inv_sign_mask, gvl); \
  x = __riscv_vreinterpret_v_i64m##m##_f64m##m(xa);			\
                                                                        \
  /* scale by 4/Pi */                                                   \
  y = __riscv_vfmul_vf_f64m##m(x, _ps_cephes_FOPI, gvl);                \
                                                                        \
  /* store the integer part of y in mm0 */                              \
  emm2 = __riscv_vfcvt_x_f_v_i64m##m(y, gvl);                           \
                                                                        \
  /* j=(j+1) & (~1) (see the cephes sources) */                         \
  emm2 = __riscv_vadd_vx_i64m##m(emm2, _pi32_1, gvl);                   \
  emm2 = __riscv_vand_vx_i64m##m(emm2, _pi32_inv1, gvl);                \
  y = __riscv_vfcvt_f_x_v_f64m##m(emm2, gvl);                           \
                                                                        \
  emm2 = __riscv_vsub_vx_i64m##m(emm2, _pi32_2, gvl);                   \
                                                                        \
  /* get the swap sign flag */                                          \
  emm0 = __riscv_vxor_vx_i64m##m(emm2, 0xffffffffffffffff, gvl);        \
  emm0 = __riscv_vand_vx_i64m##m(emm0, _pi32_4, gvl);                   \
                                                                        \
  emm0 = __riscv_vsll_vx_i64m##m(emm0, 61, gvl);                        \
                                                                        \
  /* get the polynom selection mask */                                  \
  emm2 = __riscv_vand_vx_i64m##m(emm2, _pi32_2, gvl);                   \
  xMask = __riscv_vmseq_vx_i64m##m##_b##md(emm2, _Zero, gvl);           \
  vint64m##m##_t zv = __riscv_vmv_v_x_i64m##m(_Zero, gvl);		\
  emm2 = __riscv_vmerge_vxm_i64m##m(zv, 0xffffffffffffffff, xMask, gvl); \
                                                                        \
  vfloat64m##m##_t sign_bit = __riscv_vreinterpret_v_i64m##m##_f64m##m(emm0); \
  vfloat64m##m##_t poly_mask = __riscv_vreinterpret_v_i64m##m##_f64m##m(emm2); \
                                                                        \
  /* The magic pass: "Extended precision modular arithmetic"            \
     x = ((x - y * DP1) - y * DP2) - y * DP3; */                        \
                                                                        \
  double _ps_minus_cephes_DP1 = -0.78515625;                            \
  double _ps_minus_cephes_DP2 = -2.4187564849853515625E-4;              \
  double _ps_minus_cephes_DP3 = -3.77489497744594108E-8;                \
                                                                        \
  x = __riscv_vfmacc_vf_f64m##m(x, _ps_minus_cephes_DP1, y, gvl);       \
  x = __riscv_vfmacc_vf_f64m##m(x, _ps_minus_cephes_DP2, y, gvl);       \
  x = __riscv_vfmacc_vf_f64m##m(x, _ps_minus_cephes_DP3, y, gvl);       \
                                                                        \
  /* Evaluate the first polynom  (0 <= x <= Pi/4) */                    \
  double _ps_coscof_p0 = 2.443315711809948E-005;                        \
  double _ps_coscof_p1 = -1.388731625493765E-003;                       \
  double _ps_coscof_p2 = 4.166664568298827E-002;                        \
  double _ps_0p5 = 0.5f;                                                \
                                                                        \
  vfloat64m##m##_t z;                                                   \
  vfloat64m##m##_t tmp;                                                 \
                                                                        \
  z = __riscv_vfmul_vv_f64m##m(x, x, gvl);                              \
                                                                        \
  vfloat64m##m##_t vcp1 = __riscv_vfmv_v_f_f64m##m(_ps_coscof_p1, gvl); \
  vfloat64m##m##_t vcp2 = __riscv_vfmv_v_f_f64m##m(_ps_coscof_p2, gvl); \
  y = __riscv_vfmacc_vf_f64m##m(vcp1, _ps_coscof_p0, y, gvl);		\
  y = __riscv_vfmacc_vv_f64m##m(vcp2, z, y, gvl);			\
  y = __riscv_vfmul_vv_f64m##m(y, z, gvl);                              \
  y = __riscv_vfmul_vv_f64m##m(y, z, gvl);                              \
  y = __riscv_vfnmsub_vf_f64m##m(y, _ps_0p5, z, gvl);                   \
  y = __riscv_vfadd_vf_f64m##m(y, 1.0, gvl);                            \
                                                                        \
  /* Evaluate the second polynom  (Pi/4 <= x <= 0) */                   \
  double _ps_sincof_p0 = -1.9515295891E-4;                              \
  double _ps_sincof_p1 = 8.3321608736E-3;                               \
  double _ps_sincof_p2 = -1.6666654611E-1;                              \
  vfloat64m##m##_t y2;                                                  \
                                                                        \
  vfloat64m##m##_t vsp1 = __riscv_vfmv_v_f_f64m##m(_ps_sincof_p1, gvl);	\
  vfloat64m##m##_t vsp2 = __riscv_vfmv_v_f_f64m##m(_ps_sincof_p2, gvl);	\
  y2 = __riscv_vfmacc_vf_f64m##m(vsp1, _ps_sincof_p0, z, gvl);		\
  y2 = __riscv_vfmacc_vv_f64m##m(vsp2, z, y2, gvl);			\
  y2 = __riscv_vfmul_vv_f64m##m(y2, z, gvl);                            \
  y2 = __riscv_vfmacc_vv_f64m##m(x, y2, x, gvl);                        \
                                                                        \
  /* select the correct result from the two polynoms */                 \
  xmm3 = poly_mask;                                                     \
  vint64m##m##_t t1 = __riscv_vreinterpret_v_f64m##m##_i64m##m(xmm3);   \
  vint64m##m##_t t2 = __riscv_vreinterpret_v_f64m##m##_i64m##m(y2);     \
  vint64m##m##_t t3 = __riscv_vreinterpret_v_f64m##m##_i64m##m(y);      \
  vint64m##m##_t t4 = __riscv_vxor_vx_i64m##m(t1, 0xffffffffffffffff, gvl); \
  vint64m##m##_t at1t2 = __riscv_vand_vv_i64m##m(t1, t2, gvl);		\
  vint64m##m##_t at3t4 = __riscv_vand_vv_i64m##m(t3, t4, gvl);		\
  y2 = __riscv_vreinterpret_v_i64m##m##_f64m##m(at1t2);			\
  y = __riscv_vreinterpret_v_i64m##m##_f64m##m(at3t4);			\
  y = __riscv_vfadd_vv_f64m##m(y, y2, gvl);                             \
  /* update the sign */                                                 \
  t1 = __riscv_vreinterpret_v_f64m##m##_i64m##m(y);                     \
  t2 = __riscv_vreinterpret_v_f64m##m##_i64m##m(sign_bit);              \
  vint64m##m##_t xt1t2 = __riscv_vxor_vv_i64m##m(t1, t2, gvl);		\
  y = __riscv_vreinterpret_v_i64m##m##_f64m##m(xt1t2);			\
                                                                        \
  return y;                                                             \
}                                                                       \


#define COS32_INLINE(m,md)                                              \
void cos_f32m##m##_bmark(float *angles, float *results, size_t len);  \
static inline vfloat32m##m##_t __cos_f32m##m(vfloat32m##m##_t x, size_t gvl) {  \
  int32_t _ps_inv_sign_mask = ~0x80000000;				\
  float _ps_cephes_FOPI = 1.27323954473516; /* 4 / M_PI */             \
  int32_t _pi32_1 = 1;                                                  \
  int32_t _pi32_inv1 = ~0x00000001;					\
  int32_t _pi32_2 = 2;                                                  \
  int32_t _pi32_4 = 4;                                                  \
  int32_t _Zero = 0;                                                    \
                                                                        \
  vfloat32m##m##_t xmm2;                                                \
  vfloat32m##m##_t xmm1;                                                \
  vfloat32m##m##_t xmm3;                                                \
  vfloat32m##m##_t y;                                                   \
                                                                        \
  vint32m##m##_t emm0;                                                  \
  vint32m##m##_t emm2;                                                  \
                                                                        \
  vbool##md##_t xMask;                                                  \
  /* take the absolute value */                                         \
  vint32m##m##_t xf = __riscv_vreinterpret_v_f32m##m##_i32m##m(x);	\
  vint32m##m##_t xa = __riscv_vand_vx_i32m##m(xf, _ps_inv_sign_mask, gvl); \
  x = __riscv_vreinterpret_v_i32m##m##_f32m##m(xa);			\
                                                                        \
  /* scale by 4/Pi */                                                   \
  y = __riscv_vfmul_vf_f32m##m(x, _ps_cephes_FOPI, gvl);                \
                                                                        \
  /* store the integer part of y in mm0 */                              \
  emm2 = __riscv_vfcvt_x_f_v_i32m##m(y, gvl);                           \
                                                                        \
  /* j=(j+1) & (~1) (see the cephes sources) */                         \
  emm2 = __riscv_vadd_vx_i32m##m(emm2, _pi32_1, gvl);                   \
  emm2 = __riscv_vand_vx_i32m##m(emm2, _pi32_inv1, gvl);                \
  y = __riscv_vfcvt_f_x_v_f32m##m(emm2, gvl);                           \
                                                                        \
  emm2 = __riscv_vsub_vx_i32m##m(emm2, _pi32_2, gvl);                   \
                                                                        \
  /* get the swap sign flag */                                          \
  emm0 = __riscv_vxor_vx_i32m##m(emm2, 0xffffffff, gvl);		\
  emm0 = __riscv_vand_vx_i32m##m(emm0, _pi32_4, gvl);                   \
                                                                        \
  emm0 = __riscv_vsll_vx_i32m##m(emm0, 61, gvl);                        \
                                                                        \
  /* get the polynom selection mask */                                  \
  emm2 = __riscv_vand_vx_i32m##m(emm2, _pi32_2, gvl);                   \
  xMask = __riscv_vmseq_vx_i32m##m##_b##md(emm2, _Zero, gvl);           \
  vint32m##m##_t zv = __riscv_vmv_v_x_i32m##m(_Zero, gvl);		\
  emm2 = __riscv_vmerge_vxm_i32m##m(zv, 0xffffffff, xMask, gvl);	\
                                                                        \
  vfloat32m##m##_t sign_bit = __riscv_vreinterpret_v_i32m##m##_f32m##m(emm0); \
  vfloat32m##m##_t poly_mask = __riscv_vreinterpret_v_i32m##m##_f32m##m(emm2); \
                                                                        \
  /* The magic pass: "Extended precision modular arithmetic"            \
     x = ((x - y * DP1) - y * DP2) - y * DP3; */                        \
                                                                        \
  float _ps_minus_cephes_DP1 = -0.78515625;                            \
  float _ps_minus_cephes_DP2 = -2.4187532849853515625E-4;              \
  float _ps_minus_cephes_DP3 = -3.77489497744594108E-8;                \
                                                                        \
  x = __riscv_vfmacc_vf_f32m##m(x, _ps_minus_cephes_DP1, y, gvl);       \
  x = __riscv_vfmacc_vf_f32m##m(x, _ps_minus_cephes_DP2, y, gvl);       \
  x = __riscv_vfmacc_vf_f32m##m(x, _ps_minus_cephes_DP3, y, gvl);       \
                                                                        \
  /* Evaluate the first polynom  (0 <= x <= Pi/4) */                    \
  float _ps_coscof_p0 = 2.443315711809948E-005;                        \
  float _ps_coscof_p1 = -1.388731625493765E-003;                       \
  float _ps_coscof_p2 = 4.166632568298827E-002;                        \
  float _ps_0p5 = 0.5f;                                                \
                                                                        \
  vfloat32m##m##_t z;                                                   \
  vfloat32m##m##_t tmp;                                                 \
                                                                        \
  z = __riscv_vfmul_vv_f32m##m(x, x, gvl);                              \
                                                                        \
  vfloat32m##m##_t vcp1 = __riscv_vfmv_v_f_f32m##m(_ps_coscof_p1, gvl); \
  vfloat32m##m##_t vcp2 = __riscv_vfmv_v_f_f32m##m(_ps_coscof_p2, gvl); \
  y = __riscv_vfmacc_vf_f32m##m(vcp1, _ps_coscof_p0, y, gvl);		\
  y = __riscv_vfmacc_vv_f32m##m(vcp2, z, y, gvl);			\
  y = __riscv_vfmul_vv_f32m##m(y, z, gvl);                              \
  y = __riscv_vfmul_vv_f32m##m(y, z, gvl);                              \
  y = __riscv_vfnmsub_vf_f32m##m(y, _ps_0p5, z, gvl);                   \
  y = __riscv_vfadd_vf_f32m##m(y, 1.0, gvl);                            \
                                                                        \
  /* Evaluate the second polynom  (Pi/4 <= x <= 0) */                   \
  float _ps_sincof_p0 = -1.9515295891E-4;                              \
  float _ps_sincof_p1 = 8.3321608736E-3;                               \
  float _ps_sincof_p2 = -1.6666654611E-1;                              \
  vfloat32m##m##_t y2;                                                  \
                                                                        \
  vfloat32m##m##_t vsp1 = __riscv_vfmv_v_f_f32m##m(_ps_sincof_p1, gvl);	\
  vfloat32m##m##_t vsp2 = __riscv_vfmv_v_f_f32m##m(_ps_sincof_p2, gvl);	\
  y2 = __riscv_vfmacc_vf_f32m##m(vsp1, _ps_sincof_p0, z, gvl);		\
  y2 = __riscv_vfmacc_vv_f32m##m(vsp2, z, y2, gvl);			\
  y2 = __riscv_vfmul_vv_f32m##m(y2, z, gvl);                            \
  y2 = __riscv_vfmacc_vv_f32m##m(x, y2, x, gvl);                        \
                                                                        \
  /* select the correct result from the two polynoms */                 \
  xmm3 = poly_mask;                                                     \
  vint32m##m##_t t1 = __riscv_vreinterpret_v_f32m##m##_i32m##m(xmm3);   \
  vint32m##m##_t t2 = __riscv_vreinterpret_v_f32m##m##_i32m##m(y2);     \
  vint32m##m##_t t3 = __riscv_vreinterpret_v_f32m##m##_i32m##m(y);      \
  vint32m##m##_t t4 = __riscv_vxor_vx_i32m##m(t1, 0xffffffff, gvl);	\
  vint32m##m##_t at1t2 = __riscv_vand_vv_i32m##m(t1, t2, gvl);		\
  vint32m##m##_t at3t4 = __riscv_vand_vv_i32m##m(t3, t4, gvl);		\
  y2 = __riscv_vreinterpret_v_i32m##m##_f32m##m(at1t2);			\
  y = __riscv_vreinterpret_v_i32m##m##_f32m##m(at3t4);			\
  y = __riscv_vfadd_vv_f32m##m(y, y2, gvl);                             \
  /* update the sign */                                                 \
  t1 = __riscv_vreinterpret_v_f32m##m##_i32m##m(y);                     \
  t2 = __riscv_vreinterpret_v_f32m##m##_i32m##m(sign_bit);              \
  vint32m##m##_t xt1t2 = __riscv_vxor_vv_i32m##m(t1, t2, gvl);		\
  y = __riscv_vreinterpret_v_i32m##m##_f32m##m(xt1t2);			\
                                                                        \
  return y;                                                             \
}                                                                       \


COS64_INLINE(1,64)
COS64_INLINE(2,32)
COS64_INLINE(4,16)
COS32_INLINE(1,32)
COS32_INLINE(2,16)
COS32_INLINE(4,8)
