// Modified version of:
// "RISC-V VECTOR EXP FUNCTION Version by Cristóbal Ramírez Lazo, "Barcelona
// 2019"" Find details on the original version below Author: Matteo Perotti
// <mperotti@iis.ee.ethz.ch>

//
// RISC-V VECTOR EXP FUNCTION Version by Cristóbal Ramírez Lazo, "Barcelona
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

#include "exp.h"

#define EXP_BMARK(t,w,l)							\
void exp_f##w##m##l##_bmark(t *exponents, t *results, size_t len) {		\
 size_t avl = len;							\
 vfloat##w##m##l##_t exp_vec, res_vec;					\
									\
 for (size_t vl = __riscv_vsetvl_e##w##m##l(avl); avl > 0; avl -= vl) {	\
 vl = __riscv_vsetvl_e##w##m##l(avl);					\
 exp_vec = __riscv_vle##w##_v_f##w##m##l(exponents, vl);		\
 res_vec = __exp_f##w##m##l(exp_vec, vl);				\
 __riscv_vse##w##_v_f##w##m##l(results, res_vec, vl);			\
 exponents += vl;							\
 results += vl;								\
 }									\
}

EXP_BMARK(double,64,1)
EXP_BMARK(double,64,2)
EXP_BMARK(double,64,4)
EXP_BMARK(float,32,1)
EXP_BMARK(float,32,2)
EXP_BMARK(float,32,4)
