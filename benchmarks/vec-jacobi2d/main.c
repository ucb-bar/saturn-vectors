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

// Porting to Ara SW environment
// Author: Matteo Perotti, ETH Zurich, <mperotti@iis.ee.ethz.ch>

#include <stdio.h>
#include <string.h>

#include "jacobi2d.h"
#include "util.h"
#include "ara/util.h"


// The padded matrices should be aligned in SW not on the padding,
// but on the actual data.
// R and C contain the padding as well.
extern uint64_t R;
extern uint64_t C;

extern uint64_t TSTEPS;

extern DATA_TYPE A_s[] __attribute__((aligned(32), section(".l2")));
extern DATA_TYPE B_s[] __attribute__((aligned(32), section(".l2")));
extern DATA_TYPE A_v[] __attribute__((aligned(32), section(".l2")));
extern DATA_TYPE B_v[] __attribute__((aligned(32), section(".l2")));

int main() {
  printf("JACOBI2D\n");

  int error = 0;
  unsigned long cycles1, cycles2, instr2, instr1;

#if PREALLOCATE
  j2d_v(R, C, A_v, B_v, TSTEPS);
#endif

  // Measure vector kernel execution
  printf("Processing the vector benchmark\n");
  instr1 = read_csr(minstret);
  cycles1 = read_csr(mcycle);
  j2d_v(R, C, A_v, B_v, TSTEPS);
  asm volatile("fence");
  instr2 = read_csr(minstret);
  cycles2 = read_csr(mcycle);
  int64_t runtime = cycles2 - cycles1;
  // 2* since we have 2 jacobi kernels, one on A_fixed_v, one on B_fixed_v
  // TSTEPS*5*N*N is the number of DPFLOP to compute
  float performance = (2.0 * TSTEPS * 5.0 * (R - 1) * (C - 1) / runtime);
  printf("operations = %ld\n", (size_t)(TSTEPS * 5.0 * (R - 1) * (C - 1)));
  printf("Vector jacobi2d (R=%ld C=%ld) cycle count: %d\n", R, C, runtime);
  printf("The performance is %ld DPFLOP/1000 cycles\n",
         (uint64_t)(1000.0 * performance));

  return error;
}
