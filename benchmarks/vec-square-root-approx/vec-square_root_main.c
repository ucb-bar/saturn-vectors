// See LICENSE for license details.

//**************************************************************************
//  square root approximation benchmark
//--------------------------------------------------------------------------
//
// This benchmark tests a vectorized conditional implementation.
// The input data (and reference data) should be generated using
// the root_approx_gendata.pl perl script and dumped to a file named
// dataset1.h.

#include <string.h>
#include "util.h"

//--------------------------------------------------------------------------
// Input/Reference Data

#include "dataset1.h"
#include <stdio.h>

//--------------------------------------------------------------------------
// Main

void vec_root_approx(size_t n, float x[]);

int main( int argc, char* argv[] )
{

#if PREALLOCATE
  // If needed we preallocate everything in the caches
  vec_root_approx(DATA_SIZE, input1_data);
#endif

  // Do the root
  setStats(1);
  vec_root_approx(DATA_SIZE, input1_data);
  setStats(0);
}
