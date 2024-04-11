// See LICENSE for license details.

//**************************************************************************
// SGEMV benchmark
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

void *vec_sgemv (size_t, size_t, const float*, const float*, float*);

int main( int argc, char* argv[] )
{
  float results_data[N_DIM] = {0};

  printf("sgemv M,N = %ld,%ld\n", M_DIM, N_DIM);
#if PREALLOCATE
  // If needed we preallocate everything in the caches
  vec_sgemv(M_DIM, N_DIM, input_data_x, input_data_A, results_data);
  memset(results_data, 0, sizeof(results_data));
#endif

  // Do the sgemv
  setStats(1);
  vec_sgemv(M_DIM, N_DIM, input_data_x, input_data_A, results_data);
  setStats(0);

  // Check the results
  return verifyFloat( N_DIM, results_data, verify_data );
}
