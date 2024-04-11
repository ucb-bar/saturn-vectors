// Modified version of pathfinder from RODINIA and then RiVEC, adapted to Ara
// environment. Author: Matteo Perotti <mperotti@iis.ee.ethz.ch> Check LICENSE_0
// and LICENCE_1 for additional information

/*************************************************************************
 * RISC-V Vectorized Version
 * Author: Cristóbal Ramírez Lazo
 * email: cristobal.ramirez@bsc.es
 * Barcelona Supercomputing Center (2020)
 *************************************************************************/

#include <stdint.h>
#include <string.h>

#include "pathfinder.h"
#include "util.h"
#include <stdio.h>

//#define CHECK

extern int32_t num_runs;
extern int32_t rows;
extern int32_t cols;
extern int src[] __attribute__((aligned(32), section(".l2")));
extern int wall[] __attribute__((aligned(32), section(".l2")));
extern int result_v[] __attribute__((aligned(32), section(".l2")));
extern int result_s[] __attribute__((aligned(32), section(".l2")));

int verify_result(int *result_s, int *result_v, uint32_t cols) {
#ifdef CHECK
  // Check vector with scalar result
  for (uint32_t i = 0; i < cols; i++) {
    if (result_v[i] != result_s[i]) {
      printf("Error. result_v[%d]=%d != result_s[%d]=%d \n", i, result_v[i], i,
             result_s[i]);
      return 1;
    }
  }

  printf("Test result: PASS. No errors found.\n");
#else
  volatile uint32_t x;
  for (uint32_t i = 0; i < cols; i++) {
    x = result_v[i];
  }
#endif
  return 0;
}

int main() {
  printf("PATHFINDER\n");

  int error;
  int *s_ptr;
  unsigned long cycles1, cycles2, instr2, instr1;

  printf("Number of runs: %d\n", num_runs);
  printf("rows=%ld cols=%ld\n", rows, cols);

#ifdef CHECK
  instr1 = read_csr(minstret);
  cycles1 = read_csr(mcycle);
  s_ptr = run(wall, result_s, src, cols, rows, num_runs);
  asm volatile("fence");
  instr2 = read_csr(minstret);
  cycles2 = read_csr(mcycle);
  printf("Scalar code cycles: %d\n", cycles2 - cycles1);
#endif

#define TEST(l)                                                         \
  instr1 = read_csr(minstret);                                          \
  cycles1 = read_csr(mcycle);                                           \
  run_vectorm##l(wall, result_v, cols, rows, num_runs);                 \
  asm volatile("fence");                                                \
  instr2 = read_csr(minstret);                                          \
  cycles2 = read_csr(mcycle);                                           \
  printf("Vector code LMUL=%d, cycles: %d\n", l, cycles2 - cycles1);    \
  error = verify_result(s_ptr, result_v, cols);                         \
  if (error) return error;                                              \


  TEST(1);
  TEST(2);
  TEST(4);
  TEST(8);

  return error;
}
