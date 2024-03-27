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
// Author: Matteo Perotti <mperotti@iis.ee.ethz.ch>

#include "cos.h"

#define COS_IMPL(m) void cos_f64m##m##_bmark(double *angles, double *results, size_t len) { \
                                                                        \
  size_t avl = len;                                                     \
  vfloat64m##m##_t cos_vec, res_vec;                                    \
                                                                        \
  for (size_t vl = __riscv_vsetvl_e64m##m(avl); avl > 0; avl -= vl) {   \
    vl = __riscv_vsetvl_e64m##m(avl);                                   \
    cos_vec = __riscv_vle64_v_f64m##m(angles, vl);                      \
    res_vec = __cos_f64m##m(cos_vec, vl);                               \
    __riscv_vse64_v_f64m##m(results, res_vec, vl);                      \
    angles += vl;                                                       \
    results += vl;                                                      \
  }                                                                     \
}

COS_IMPL(1)
COS_IMPL(2)
COS_IMPL(4)

void cos_f32_bmark(float *angles, float *results, size_t len) {

  size_t avl = len;
  vfloat32m1_t cos_vec, res_vec;

  for (size_t vl = __riscv_vsetvl_e32m1(avl); avl > 0; avl -= vl) {
    // Strip-mine
    vl = __riscv_vsetvl_e32m1(avl);
    // Load vector
    cos_vec = __riscv_vle32_v_f32m1(angles, vl);
    // Compute
    res_vec = __cos_f32(cos_vec, vl);
    // Store
    __riscv_vse32_v_f32m1(results, res_vec, vl);
    // Bump pointers
    angles += vl;
    results += vl;
  }
}
