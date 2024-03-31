// Modified version of pathfinder from RODINIA and then RiVEC, adapted to Ara
// environment. Author: Matteo Perotti <mperotti@iis.ee.ethz.ch> Check LICENSE_0
// and LICENCE_1 for additional information

/*************************************************************************
 * RISC-V Vectorized Version
 * Author: Cristóbal Ramírez Lazo
 * email: cristobal.ramirez@bsc.es
 * Barcelona Supercomputing Center (2020)
 *************************************************************************/

#include "pathfinder.h"

#define MIN(a, b)((a < b) ? a : b)

int *run(int *wall, int *result_s, int *src, uint32_t cols, uint32_t rows,
         uint32_t num_runs) {
  int min;
  int *temp;
  int *dst;

  for (uint32_t j = 0; j < num_runs; j++) {
    for (uint32_t x = 0; x < cols; x++) {
      result_s[x] = wall[x];
    }

    dst = result_s;

    for (uint32_t t = 0; t < rows - 1; t++) {
      temp = src;
      src = dst;
      dst = temp;
      for (uint32_t n = 0; n < cols; n++) {
        min = src[n];
        if (n > 0)
          min = MIN(min, src[n - 1]);
        if (n < cols - 1)
          min = MIN(min, src[n + 1]);
        dst[n] = wall[(t + 1) * cols + n] + min;
      }
    }
    // Reset the pointer not to lose it
    src = temp;
  }
  return dst;
}

#define IMPL(l)                                                         \
void run_vectorm##l(int *wall, int *result_v, uint32_t cols, uint32_t rows, \
                    uint32_t num_runs) {                                \
                                                                        \
  size_t gvl;                                                           \
                                                                        \
  vint32m##l##_t temp;                                                  \
  vint32m##l##_t xSrc_slideup;                                          \
  vint32m##l##_t xSrc_slidedown;                                        \
  vint32m##l##_t xSrc;                                                  \
  vint32m##l##_t xNextrow;                                              \
                                                                        \
  int aux, aux2;                                                        \
  int *dst;                                                             \
                                                                        \
  for (uint32_t j = 0; j < num_runs; j++) {                             \
    for (uint32_t n = 0; n < cols; n += gvl) {                          \
      gvl = __riscv_vsetvl_e32m##l(cols);                               \
      temp = __riscv_vle32_v_i32m##l(&wall[n], gvl);                    \
      __riscv_vse32_v_i32m##l(&result_v[n], temp, gvl);                 \
    }                                                                   \
    dst = result_v;                                                     \
                                                                        \
    gvl = __riscv_vsetvl_e32m##l(cols);                                 \
                                                                        \
    for (uint32_t t = 0; t < rows - 1; t++) {                           \
      aux = dst[0];                                                     \
      for (uint32_t n = 0; n < cols; n = n + gvl) {                     \
        gvl = __riscv_vsetvl_e32m##l(cols - n);                         \
        xNextrow = __riscv_vle32_v_i32m##l(&dst[n], gvl);               \
                                                                        \
        xSrc = xNextrow;                                                \
        aux2 = (n + gvl >= cols) ? dst[n + gvl - 1] : dst[n + gvl];     \
        xSrc_slideup = __riscv_vslide1up_vx_i32m##l(xSrc, aux, gvl);    \
        xSrc_slidedown = __riscv_vslide1down_vx_i32m##l(xSrc, aux2, gvl); \
                                                                        \
        xSrc = __riscv_vmin_vv_i32m##l(xSrc, xSrc_slideup, gvl);        \
        xSrc = __riscv_vmin_vv_i32m##l(xSrc, xSrc_slidedown, gvl);      \
                                                                        \
        xNextrow = __riscv_vle32_v_i32m##l(&wall[(t + 1) * cols + n], gvl); \
        xNextrow = __riscv_vadd_vv_i32m##l(xNextrow, xSrc, gvl);        \
                                                                        \
        aux = dst[n + gvl - 1];                                         \
        __riscv_vse32_v_i32m##l(&dst[n], xNextrow, gvl);                \
      }                                                                 \
    }                                                                   \
  }                                                                     \
}

IMPL(1)
IMPL(2)
IMPL(4)
IMPL(8)
