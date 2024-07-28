/*
OHIO STATE UNIVERSITY SOFTWARE DISTRIBUTION LICENSE

PolyBench/C, a collection of benchmarks containing static control
parts (the "Software")
Copyright (c) 2010-2016, Ohio State University. All rights reserved.

The Software is available for download and use subject to the terms
and conditions of this License.  Access or use of the Software
constitutes acceptance and agreement to the terms and conditions of
this License.  Redistribution and use of the Software in source and
binary forms, with or without modification, are permitted provided
that the following conditions are met:

1. Redistributions of source code must retain the above copyright
notice, this list of conditions and the capitalized paragraph below.

2. Redistributions in binary form must reproduce the above copyright
notice, this list of conditions and the capitalized paragraph below in
the documentation and/or other materials provided with the
distribution.

3. The name of Ohio State University, or its faculty, staff or
students may not be used to endorse or promote products derived from
the Software without specific prior written permission.

This software was produced with support from the U.S. Defense Advanced
Research Projects Agency (DARPA), the U.S. Department of Energy (DoE)
and the U.S. National Science Foundation. Nothing in this work should
be construed as reflecting the official policy or position of the
Defense Department, the United States government or Ohio State
University.

THIS SOFTWARE HAS BEEN APPROVED FOR PUBLIC RELEASE, UNLIMITED
DISTRIBUTION.  THE SOFTWARE IS PROVIDED ?AS IS? AND WITHOUT ANY
EXPRESS, IMPLIED OR STATUTORY WARRANTIES, INCLUDING, BUT NOT LIMITED
TO, WARRANTIES OF ACCURACY, COMPLETENESS, NONINFRINGEMENT,
MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
ACCESS OR USE OF THE SOFTWARE IS ENTIRELY AT THE USER?S RISK.  IN NO
EVENT SHALL OHIO STATE UNIVERSITY OR ITS FACULTY, STAFF OR STUDENTS BE
LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.  THE SOFTWARE USER SHALL
INDEMNIFY, DEFEND AND HOLD HARMLESS OHIO STATE UNIVERSITY AND ITS
FACULTY, STAFF AND STUDENTS FROM ANY AND ALL CLAIMS, ACTIONS, DAMAGES,
LOSSES, LIABILITIES, COSTS AND EXPENSES, INCLUDING ATTORNEYS? FEES AND
COURT COSTS, DIRECTLY OR INDIRECTLY ARISING OUT OF OR IN CONNECTION
WITH ACCESS OR USE OF THE SOFTWARE.
*/

/**
 * This version is stamped on May 10, 2016
 *
 * Contact:
 *   Louis-Noel Pouchet <pouchet.ohio-state.edu>
 *   Tomofumi Yuki <tomofumi.yuki.fr>
 *
 * Web address: http://polybench.sourceforge.net
 */
/* jacobi-2d.c: this file is part of PolyBench/C */

/*************************************************************************
 * RISC-V Vectorized Version
 * Author: Cristóbal Ramírez Lazo
 * email: cristobal.ramirez@bsc.es
 * Barcelona Supercomputing Center (2020)
 *************************************************************************/

// Porting to Ara SW environment and Optimization
// Author: Matteo Perotti, ETH Zurich, <mperotti@iis.ee.ethz.ch>

#include "jacobi2d.h"
#define DOUBLE_BUFFERING

void j2d_s(uint64_t r, uint64_t c, DATA_TYPE *A, DATA_TYPE *B,
           uint64_t tsteps) {
  for (uint32_t t = 0; t < tsteps; t++) {
    for (uint32_t i = 1; i < r - 1; i++)
      for (uint32_t j = 1; j < c - 1; j++)
        B[i * c + j] =
            (0.2) * (A[i * c + j] + A[i * c + j - 1] + A[i * c + j + 1] +
                     A[(i + 1) * c + j] + A[(i - 1) * c + j]);
#ifdef DOUBLE_BUFFERING
    for (uint32_t i = 1; i < r - 1; i++)
      for (uint32_t j = 1; j < c - 1; j++)
        A[i * c + j] =
            (0.2) * (B[i * c + j] + B[i * c + j - 1] + B[i * c + j + 1] +
                     B[(i + 1) * c + j] + B[(i - 1) * c + j]);
#endif
  }
}

void j2d_v(uint64_t r, uint64_t c, DATA_TYPE *A, DATA_TYPE *B,
           uint64_t tsteps) {
  for (uint32_t t = 0; t < tsteps; t++) {
    j2d_kernel_v(r, c, B, A);
  }
}

void j2d_kernel_v(uint64_t r, uint64_t c, DATA_TYPE *A, DATA_TYPE *B) {
  vfloat64m4_t xU;
  vfloat64m4_t xUtmp;
  vfloat64m4_t xUleft;
  vfloat64m4_t xUright;
  vfloat64m4_t xUtop;
  vfloat64m4_t xUbottom;

  DATA_TYPE izq, der;
  uint32_t size_x = c - 2;
  uint32_t size_y = r - 2;

  size_t gvl = __riscv_vsetvl_e64m4(size_x);

  for (uint32_t j = 1; j <= size_x; j = j + gvl) {
    gvl = __riscv_vsetvl_e64m4(size_x - j + 1);
    xU = __riscv_vle64_v_f64m4(&A[1 * c + j], gvl);
    xUtop = __riscv_vle64_v_f64m4(&A[0 * c + j], gvl);
    xUbottom = __riscv_vle64_v_f64m4(&A[2 * c + j], gvl);

    for (uint32_t i = 1; i <= size_y; i++) {
      if (i != 1) {
        xUtop = xU;
        xU = xUbottom;
        xUbottom = __riscv_vle64_v_f64m4(&A[(i + 1) * c + j], gvl);
      }
      izq = A[i * c + j - 1];
      der = A[i * c + j + gvl];
      xUleft = __riscv_vfslide1up_vf_f64m4(xU, izq, gvl);
      xUright = __riscv_vfslide1down_vf_f64m4(xU, der, gvl);
      xUtmp = __riscv_vfadd_vv_f64m4(xUleft, xUright, gvl);
      xUtmp = __riscv_vfadd_vv_f64m4(xUtmp, xUtop, gvl);
      xUtmp = __riscv_vfadd_vv_f64m4(xUtmp, xUbottom, gvl);
      xUtmp = __riscv_vfadd_vv_f64m4(xUtmp, xU, gvl);
      xUtmp = __riscv_vfmul_vf_f64m4(xUtmp, (float)0.2, gvl);
      __riscv_vse64_v_f64m4(&B[i * c + j], xUtmp, gvl);
    }
  }
}
