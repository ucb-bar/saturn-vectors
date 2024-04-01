// Copyright 2022 ETH Zurich and University of Bologna.
//
// SPDX-License-Identifier: Apache-2.0
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//    http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// Author: Matteo Perotti

#include <stdint.h>
#include <string.h>

#include "riscv_vector.h"


#define EXP_IMPL(t,w,l,b,c1,c2)						\
void exp_f##w##m##l##_bmark(t *exponents, t *results, size_t len);	        \
inline vfloat##w##m##l##_t __exp_f##w##m##l(vfloat##w##m##l##_t x, size_t gvl) {	\
  t exp_hi = 88.3762626##w##7949;					\
  t exp_lo = -88.3762626##w##7949;					\
									\
  t cephes_LOG2EF = 1.44269504088896341;				\
  t cephes_exp_C1 = 0.693359375;					\
  t cephes_exp_C2 = -2.12194440e-4;					\
  									\
  t cephes_exp_p0 = 1.9875691500E-4;					\
  t cephes_exp_p1 = 1.3981999507E-3;					\
  t cephes_exp_p2 = 8.3334519073E-3;					\
  t cephes_exp_p3 = 4.1665795894E-2;					\
  t cephes_exp_p4 = 1.6666665459E-1;					\
  t cephes_exp_p5 = 5.0000001201E-1;					\
  vfloat##w##m##l##_t tmp;						\
  vfloat##w##m##l##_t tmp2;						\
  vfloat##w##m##l##_t tmp4;						\
  vfloat##w##m##l##_t fx;						\
   									\
  vfloat##w##m##l##_t one = __riscv_vfmv_v_f_f##w##m##l(1.0, gvl);	\
  vfloat##w##m##l##_t zero = __riscv_vfmv_v_f_f##w##m##l(0.0, gvl);	\
  vfloat##w##m##l##_t z;						\
  vfloat##w##m##l##_t y;						\
  									\
  vbool##b##_t mask;							\
  vint##w##m##l##_t imm0;						\
  vint##w##m##l##_t tmp3;						\
  									\
  x = __riscv_vfmin_vf_f##w##m##l(x, exp_hi, gvl);			\
  x = __riscv_vfmax_vf_f##w##m##l(x, exp_lo, gvl);			\
  									\
  fx = __riscv_vfmv_v_f_f##w##m##l(0.5, gvl);				\
  fx = __riscv_vfmacc_vf_f##w##m##l(fx, cephes_LOG2EF, x, gvl);	\
									\
  tmp3 = __riscv_vfcvt_x_f_v_i##w##m##l(fx, gvl);			\
  tmp = __riscv_vfcvt_f_x_v_f##w##m##l(tmp3, gvl);			\
									\
  mask = __riscv_vmflt_vv_f##w##m##l##_b##b(fx, tmp, gvl);		\
  tmp2 = __riscv_vmerge_vvm_f##w##m##l(zero, one, mask, gvl);		\
  fx = __riscv_vfsub_vv_f##w##m##l(tmp, tmp2, gvl);			\
  tmp = __riscv_vfmul_vf_f##w##m##l(fx, cephes_exp_C1, gvl);		\
  z = __riscv_vfmul_vf_f##w##m##l(fx, cephes_exp_C2, gvl);		\
  x = __riscv_vfsub_vv_f##w##m##l(x, tmp, gvl);				\
  x = __riscv_vfsub_vv_f##w##m##l(x, z, gvl);				\
  									\
  z = __riscv_vfmul_vv_f##w##m##l(x, x, gvl);				\
  									\
  y = __riscv_vfmv_v_f_f##w##m##l(cephes_exp_p0, gvl);			\
  y = __riscv_vfmadd_vf_f##w##m##l(y, cephes_exp_p1, x, gvl);		\
  y = __riscv_vfmadd_vf_f##w##m##l(y, cephes_exp_p2, x, gvl);		\
  y = __riscv_vfmadd_vf_f##w##m##l(y, cephes_exp_p3, x, gvl);		\
  y = __riscv_vfmadd_vf_f##w##m##l(y, cephes_exp_p4, x, gvl);		\
  y = __riscv_vfmadd_vf_f##w##m##l(y, cephes_exp_p5, x, gvl);		\
  y = __riscv_vfmadd_vv_f##w##m##l(y, z, x, gvl);			\
  y = __riscv_vfadd_vv_f##w##m##l(y, one, gvl);				\
									\
  imm0 = __riscv_vfcvt_x_f_v_i##w##m##l(fx, gvl);			\
  imm0 = __riscv_vadd_vv_i##w##m##l(imm0, __riscv_vmv_v_x_i##w##m##l(c1, gvl), gvl); \
  imm0 = __riscv_vsll_vv_i##w##m##l(imm0, __riscv_vmv_v_x_u##w##m##l(c2, gvl), gvl); \
  									\
  tmp4 = __riscv_vreinterpret_v_i##w##m##l##_f##w##m##l(imm0);		\
  y = __riscv_vfmul_vv_f##w##m##l(y, tmp4, gvl);			\
  return y;								\
}

EXP_IMPL(double,64,1,64,1023,52);
EXP_IMPL(double,64,2,32,1023,52);
EXP_IMPL(double,64,4,16,1023,52);
EXP_IMPL(float,32,1,32,0x7f,23);
EXP_IMPL(float,32,2,16,0x7f,23);
EXP_IMPL(float,32,4,8,0x7f,23);

