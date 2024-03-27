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

#include "ara/rivec/vector_defines.h"

void cos_1xf64_bmark(double *angles, double *results, size_t len);
void cos_2xf32_bmark(float *angles, float *results, size_t len);

static inline _MMR_f64 __cos_1xf64(_MMR_f64 x, size_t gvl) {

  int64_t _ps_inv_sign_mask = ~0x8000000000000000;
  double _ps_cephes_FOPI = 1.27323954473516; // 4 / M_PI
  int64_t _pi32_1 = 1;
  int64_t _pi32_inv1 = ~0x0000000000000001;
  int64_t _pi32_2 = 2;
  int64_t _pi32_4 = 4;
  int64_t _Zero = 0;

  _MMR_f64 xmm2;
  _MMR_f64 xmm1;
  _MMR_f64 xmm3;
  _MMR_f64 y;

  _MMR_i64 emm0;
  _MMR_i64 emm2;

  _MMR_MASK_i64 xMask;
  /* take the absolute value */
  x = _MM_CAST_f64_i64(__riscv_vand_vx_i64m1(_MM_CAST_i64_f64(x), _ps_inv_sign_mask, gvl));

  /* scale by 4/Pi */
  y = __riscv_vfmul_vf_f64m1(x, _ps_cephes_FOPI, gvl);

  /* store the integer part of y in mm0 */
  emm2 = _MM_VFCVT_X_F_i64(y, gvl);

  /* j=(j+1) & (~1) (see the cephes sources) */
  emm2 = __riscv_vadd_vx_i64m1(emm2, _pi32_1, gvl);
  emm2 = __riscv_vand_vx_i64m1(emm2, _pi32_inv1, gvl);
  y = _MM_VFCVT_F_X_f64(emm2, gvl);

  emm2 = __riscv_vsub_vx_i64m1(emm2, _pi32_2, gvl);

  /* get the swap sign flag */
  emm0 = __riscv_vxor_vx_i64m1(emm2, 0xffffffffffffffff, gvl);
  emm0 = __riscv_vand_vx_i64m1(emm0, _pi32_4, gvl);

  emm0 = __riscv_vsll_vx_i64m1(emm0, 61, gvl);

  /* get the polynom selection mask */
  emm2 = __riscv_vand_vx_i64m1(emm2, _pi32_2, gvl);
  xMask = __riscv_vmseq_vx_i64m1_b64(emm2, _Zero, gvl);
  emm2 = __riscv_vmerge_vxm_i64m1(_MM_SET_i64(_Zero, gvl), 0xffffffffffffffff, xMask, gvl);

  _MMR_f64 sign_bit = _MM_CAST_f64_i64(emm0);
  _MMR_f64 poly_mask = _MM_CAST_f64_i64(emm2);

  /* The magic pass: "Extended precision modular arithmetic"
     x = ((x - y * DP1) - y * DP2) - y * DP3; */

  double _ps_minus_cephes_DP1 = -0.78515625;
  double _ps_minus_cephes_DP2 = -2.4187564849853515625E-4;
  double _ps_minus_cephes_DP3 = -3.77489497744594108E-8;

  x = __riscv_vfmacc_vf_f64m1(x, _ps_minus_cephes_DP1, y, gvl);
  x = __riscv_vfmacc_vf_f64m1(x, _ps_minus_cephes_DP2, y, gvl);
  x = __riscv_vfmacc_vf_f64m1(x, _ps_minus_cephes_DP3, y, gvl);

  /* Evaluate the first polynom  (0 <= x <= Pi/4) */
  double _ps_coscof_p0 = 2.443315711809948E-005;
  double _ps_coscof_p1 = -1.388731625493765E-003;
  double _ps_coscof_p2 = 4.166664568298827E-002;
  double _ps_0p5 = 0.5f;

  _MMR_f64 z;
  _MMR_f64 tmp;

  z = _MM_MUL_f64(x, x, gvl);

  y = __riscv_vfmacc_vf_f64m1(_MM_SET_f64(_ps_coscof_p1, gvl), _ps_coscof_p0, y, gvl);
  y = __riscv_vfmacc_vv_f64m1(_MM_SET_f64(_ps_coscof_p2, gvl), z, y, gvl);
  y = _MM_MUL_f64(y, z, gvl);
  y = _MM_MUL_f64(y, z, gvl);
  y = __riscv_vfnmsub_vf_f64m1(y, _ps_0p5, z, gvl);
  y = __riscv_vfadd_vf_f64m1(y, 1.0, gvl);

  /* Evaluate the second polynom  (Pi/4 <= x <= 0) */
  double _ps_sincof_p0 = -1.9515295891E-4;
  double _ps_sincof_p1 = 8.3321608736E-3;
  double _ps_sincof_p2 = -1.6666654611E-1;
  _MMR_f64 y2;

  y2 = __riscv_vfmacc_vf_f64m1(_MM_SET_f64(_ps_sincof_p1, gvl), _ps_sincof_p0, z, gvl);
  y2 = __riscv_vfmacc_vv_f64m1(_MM_SET_f64(_ps_sincof_p2, gvl), z, y2, gvl);
  y2 = _MM_MUL_f64(y2, z, gvl);
  y2 = __riscv_vfmacc_vv_f64m1(x, y2, x, gvl);

  /* select the correct result from the two polynoms */
  xmm3 = poly_mask;
  y2 = _MM_CAST_f64_i64(
      _MM_AND_i64(_MM_CAST_i64_f64(xmm3), _MM_CAST_i64_f64(y2), gvl));
  y = _MM_CAST_f64_i64(
      _MM_AND_i64(_MM_XOR_i64(_MM_CAST_i64_f64(xmm3),
                              _MM_SET_i64(0xffffffffffffffff, gvl), gvl),
                  _MM_CAST_i64_f64(y), gvl));
  y = _MM_ADD_f64(y, y2, gvl);
  /* update the sign */
  y = _MM_CAST_f64_i64(
      _MM_XOR_i64(_MM_CAST_i64_f64(y), _MM_CAST_i64_f64(sign_bit), gvl));

  return y;
}

static inline _MMR_f32 __cos_2xf32(_MMR_f32 x, size_t gvl) {

  int32_t _ps_inv_sign_mask = ~0x80000000;
  float _ps_cephes_FOPI = 1.27323954473516; // 4 / M_PI
  int32_t _pi32_1 = 1;
  int32_t _pi32_inv1 = ~0x00000001;
  int32_t _pi32_2 = 2;
  int32_t _pi32_4 = 4;
  int32_t _Zero = 0;

  _MMR_f32 xmm2;
  _MMR_f32 xmm1;
  _MMR_f32 xmm3;
  _MMR_f32 y;

  _MMR_i32 emm0;
  _MMR_i32 emm2;

  _MMR_MASK_i32 xMask;
  /* take the absolute value */
  x = _MM_CAST_f32_i32(__riscv_vand_vx_i32m1(_MM_CAST_i32_f32(x), _ps_inv_sign_mask, gvl));

  /* scale by 4/Pi */
  y = __riscv_vfmul_vf_f32m1(x, _ps_cephes_FOPI, gvl);

  /* store the integer part of y in mm0 */
  emm2 = _MM_VFCVT_X_F_i32(y, gvl);

  /* j=(j+1) & (~1) (see the cephes sources) */
  emm2 = __riscv_vadd_vx_i32m1(emm2, _pi32_1, gvl);
  emm2 = __riscv_vand_vx_i32m1(emm2, _pi32_inv1, gvl);
  y = _MM_VFCVT_F_X_f32(emm2, gvl);

  emm2 = __riscv_vsub_vx_i32m1(emm2, _pi32_2, gvl);

  /* get the swap sign flag */
  emm0 = __riscv_vxor_vx_i32m1(emm2, 0xffffffff, gvl);
  emm0 = __riscv_vand_vx_i32m1(emm0, _pi32_4, gvl);

  emm0 = __riscv_vsll_vx_i32m1(emm0, 61, gvl);

  /* get the polynom selection mask */
  emm2 = __riscv_vand_vx_i32m1(emm2, _pi32_2, gvl);
  xMask = __riscv_vmseq_vx_i32m1_b32(emm2, _Zero, gvl);
  emm2 = __riscv_vmerge_vxm_i32m1(_MM_SET_i32(_Zero, gvl), 0xffffffff, xMask, gvl);

  _MMR_f32 sign_bit = _MM_CAST_f32_i32(emm0);
  _MMR_f32 poly_mask = _MM_CAST_f32_i32(emm2);

  /* The magic pass: "Extended precision modular arithmetic"
     x = ((x - y * DP1) - y * DP2) - y * DP3; */

  float _ps_minus_cephes_DP1 = -0.78515625;
  float _ps_minus_cephes_DP2 = -2.4187532849853515625E-4;
  float _ps_minus_cephes_DP3 = -3.77489497744594108E-8;

  x = __riscv_vfmacc_vf_f32m1(x, _ps_minus_cephes_DP1, y, gvl);
  x = __riscv_vfmacc_vf_f32m1(x, _ps_minus_cephes_DP2, y, gvl);
  x = __riscv_vfmacc_vf_f32m1(x, _ps_minus_cephes_DP3, y, gvl);

  /* Evaluate the first polynom  (0 <= x <= Pi/4) */
  float _ps_coscof_p0 = 2.443315711809948E-005;
  float _ps_coscof_p1 = -1.388731625493765E-003;
  float _ps_coscof_p2 = 4.166632568298827E-002;
  float _ps_0p5 = 0.5f;

  _MMR_f32 z;
  _MMR_f32 tmp;

  z = _MM_MUL_f32(x, x, gvl);

  y = __riscv_vfmacc_vf_f32m1(_MM_SET_f32(_ps_coscof_p1, gvl), _ps_coscof_p0, y, gvl);
  y = __riscv_vfmacc_vv_f32m1(_MM_SET_f32(_ps_coscof_p2, gvl), z, y, gvl);
  y = _MM_MUL_f32(y, z, gvl);
  y = _MM_MUL_f32(y, z, gvl);
  y = __riscv_vfnmsub_vf_f32m1(y, _ps_0p5, z, gvl);
  y = __riscv_vfadd_vf_f32m1(y, 1.0, gvl);

  /* Evaluate the second polynom  (Pi/4 <= x <= 0) */
  float _ps_sincof_p0 = -1.9515295891E-4;
  float _ps_sincof_p1 = 8.3321608736E-3;
  float _ps_sincof_p2 = -1.6666654611E-1;
  _MMR_f32 y2;

  y2 = __riscv_vfmacc_vf_f32m1(_MM_SET_f32(_ps_sincof_p1, gvl), _ps_sincof_p0, z, gvl);
  y2 = __riscv_vfmacc_vv_f32m1(_MM_SET_f32(_ps_sincof_p2, gvl), z, y2, gvl);
  y2 = _MM_MUL_f32(y2, z, gvl);
  y2 = __riscv_vfmacc_vv_f32m1(x, y2, x, gvl);

  /* select the correct result from the two polynoms */
  xmm3 = poly_mask;
  y2 = _MM_CAST_f32_i32(
      _MM_AND_i32(_MM_CAST_i32_f32(xmm3), _MM_CAST_i32_f32(y2), gvl));
  y = _MM_CAST_f32_i32(
      _MM_AND_i32(_MM_XOR_i32(_MM_CAST_i32_f32(xmm3),
                              _MM_SET_i32(0xffffffff, gvl), gvl),
                  _MM_CAST_i32_f32(y), gvl));
  y = _MM_ADD_f32(y, y2, gvl);
  /* update the sign */
  y = _MM_CAST_f32_i32(
      _MM_XOR_i32(_MM_CAST_i32_f32(y), _MM_CAST_i32_f32(sign_bit), gvl));

  return y;
}
