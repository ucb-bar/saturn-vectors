// See LICENSE for license details.

//**************************************************************************
// SGEMM benchmark
//--------------------------------------------------------------------------
//
// This benchmark tests a vectorized sgemm implementation.

#include <string.h>
#include <stdio.h>
#include "util.h"

//--------------------------------------------------------------------------
// Input/Reference Data

#include "dataset1.h"

//--------------------------------------------------------------------------
// Main

void *vec_sgemm_nn (size_t, size_t, size_t, const float*, size_t, const float*, size_t, float*, size_t);

int main( int argc, char* argv[] )
{
  float results_data[M_DIM*N_DIM] = {0};
  printf("sgemm M,N,K = %ld,%ld,%ld\n", M_DIM, N_DIM, K_DIM);

#if PREALLOCATE
  // If needed we preallocate everything in the caches
  vec_sgemm_nn(N_DIM, M_DIM, K_DIM, a_matrix, K_DIM, b_matrix, N_DIM, results_data, N_DIM);
#endif

  // Do the size sweeps
  if (M_DIM >= 64 && N_DIM >= 64 && K_DIM >= 64) {
    for (size_t t = 8; t <= 64; t += 7) {
      size_t start, end;
      start = read_csr(mcycle);
      vec_sgemm_nn(t, t, t, a_matrix, t, b_matrix, t, results_data, t);
      asm volatile ("fence");
      end = read_csr(mcycle);
      printf("size %ld cycles = %ld\n", t, end - start);
    }
  }

  // Do the sgemm
  memset(results_data, 0, sizeof(results_data));
  setStats(1);
  vec_sgemm_nn(N_DIM, M_DIM, K_DIM, a_matrix, K_DIM, b_matrix, N_DIM, results_data, N_DIM);
  setStats(0);

  // Check the results
  return verifyFloat(M_DIM*N_DIM, results_data, verify_data);
}
