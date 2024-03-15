// See LICENSE for license details.

//**************************************************************************
// Mixed_width mask benchmark
//--------------------------------------------------------------------------
//
// This benchmark tests a vectorized mixed_width_mask and compute implementation.
// The input data (and reference data) should be generated using
// the mixed_with_mask_gendata.pl perl script and dumped to a file named
// dataset1.h.

#include <string.h>
#include "util.h"

//--------------------------------------------------------------------------
// Input/Reference Data

#include "dataset1.h"

//--------------------------------------------------------------------------
// Main

void vec_mixed_width_mask(size_t n, int8_t x[], int y[], int z[]);


int main( int argc, char* argv[] )
{
  int results_data[DATA_SIZE];

#if PREALLOCATE
  // If needed we preallocate everything in the caches
  vec_mixed_width_mask(DATA_SIZE, input1_data, results_data, input2_data);
#endif

  // Do the compute
  setStats(1);
  vec_mixed_width_mask(DATA_SIZE, input1_data, results_data, input2_data);
  setStats(0);

  // Check the results
  return verify( DATA_SIZE, results_data, verify_data );
}

