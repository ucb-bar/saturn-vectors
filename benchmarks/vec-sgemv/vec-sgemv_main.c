// See LICENSE for license details.

//**************************************************************************
// SGEMV benchmark
//--------------------------------------------------------------------------
//
// This benchmark tests a vectorized sgemm implementation.

#include <string.h>
#include "util.h"

//--------------------------------------------------------------------------
// Input/Reference Data

#include "dataset1.h"

//--------------------------------------------------------------------------
// Main

void *vec_sgemv (size_t, size_t, const float*, const float*, float*);

int main( int argc, char* argv[] )
{
  float results_data[ROW_SIZE] = {0};

#if PREALLOCATE
  // If needed we preallocate everything in the caches
  vec_sgemv(ROW_SIZE, COL_SIZE, input_data_x, input_data_A, results_data);
  memset(results_data, 0, sizeof(results_data));
#endif

  // Do the sgemv
  setStats(1);
  vec_sgemv(ROW_SIZE, COL_SIZE, input_data_x, input_data_A, results_data);
  setStats(0);

  // Check the results
  return verifyFloat( ROW_SIZE, results_data, verify_data );
}
