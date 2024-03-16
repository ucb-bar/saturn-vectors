// See LICENSE for license details.

//**************************************************************************
// Conditional benchmark
//--------------------------------------------------------------------------
//
// This benchmark tests a vectorized conditional implementation.
// The input data (and reference data) should be generated using
// the conditional_gendata.pl perl script and dumped to a file named
// dataset1.h.

#include <string.h>
#include "util.h"

//--------------------------------------------------------------------------
// Input/Reference Data

#include "dataset1.h"
#include <stdio.h>

//--------------------------------------------------------------------------
// Main

static int verify_short(int n, const volatile int16_t* test, const int16_t* verify)
{
  int i;
  // Unrolled for faster verification
  for (i = 0; i < n/2*2; i+=2)
  {
    int t0 = test[i], t1 = test[i+1];
    int v0 = verify[i], v1 = verify[i+1];
    if (t0 != v0) return i+1;
    if (t1 != v1) return i+2;
  }
  if (n % 2 != 0 && test[n-1] != verify[n-1])
    return n;
  return 0;
}

void vec_conditional(size_t n, int8_t x[], int16_t a[], int16_t b[], int16_t z[]);

int main( int argc, char* argv[] )
{
  int16_t results_data[DATA_SIZE];

#if PREALLOCATE
  // If needed we preallocate everything in the caches
  vec_conditional(DATA_SIZE, input1_data, input2_data, input3_data, results_data);
#endif

  // Do the conditional
  setStats(1);
  vec_conditional(DATA_SIZE, input1_data, input2_data, input3_data, results_data);
  setStats(0);

  return verify_short(DATA_SIZE, results_data, verify_data );
}
