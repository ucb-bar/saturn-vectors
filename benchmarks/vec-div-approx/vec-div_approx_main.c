// See LICENSE for license details.

//**************************************************************************
// division approximation benchmark
//--------------------------------------------------------------------------
//
// This benchmark tests a vectorized conditional implementation.
// The input data (and reference data) should be generated using
// the div_approx_gendata.pl perl script and dumped to a file named
// dataset1.h.

#include <string.h>
#include "util.h"

//--------------------------------------------------------------------------
// Input/Reference Data

#include "dataset1.h"
#include <stdio.h>

//--------------------------------------------------------------------------
// Main

void vec_div_approx(size_t n, float x[], float y[]);

int main( int argc, char* argv[] )
{

#if PREALLOCATE
  // If needed we preallocate everything in the caches
  vec_div_approx(DATA_SIZE, input1_data, input2_data);
#endif

  // Do the division
  setStats(1);
  vec_div_approx(DATA_SIZE, input1_data, input2_data);
  setStats(0);

  // int i;
  // // Unrolled for faster verification
  // for (i = 0; i < 17/2*2; i+=2)
  // {
  //   float t0 = input1_data[i], t1 = input1_data[i+1];
  //   printf("test_val: %.2f\n", t0);
  //   printf("test_val: %.2f\n", t1);
  // }
  // if (17 % 2 != 0) printf("test_val: %.2f\n\n", input1_data[17-1]);
  return 0;

}
