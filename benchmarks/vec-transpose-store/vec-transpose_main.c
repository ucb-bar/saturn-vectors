// See LICENSE for license details.

//**************************************************************************
// Transpose benchmark
//--------------------------------------------------------------------------
//
// This benchmark tests a vectorized matrix transpose implementation.

#include <string.h>
#include "util.h"

//--------------------------------------------------------------------------
// Input/Reference Data

#include "dataset1.h"

//--------------------------------------------------------------------------
// Main

void *vec_transpose (size_t, size_t, const float*, float*);

int main( int argc, char* argv[] )
{
  float results_data[ARRAY_SIZE] = {0};

#if PREALLOCATE
  // If needed we preallocate everything in the caches
  vec_transpose(DIM_N, DIM_M, input_matrix, results_data);
  memset(results_data, 0, sizeof(results_data));
#endif

  setStats(1);
  vec_transpose(DIM_N, DIM_M, input_matrix, results_data);
  setStats(0);

  // Check the results
  return verifyFloat( ARRAY_SIZE, results_data, verify_data );
}
