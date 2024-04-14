// See LICENSE for license details.

//**************************************************************************
// Separable Convolution Benchmark
//--------------------------------------------------------------------------
//
// This benchmark tests a vectorized 2D separable convolution implementation.

#include <string.h>
#include <stdio.h>
#include "util.h"

//--------------------------------------------------------------------------
// Input/Reference Data

#include "dataset1.h"

//--------------------------------------------------------------------------
// Main

void *vec_sep_conv (size_t, size_t, size_t, size_t, const float*, const float*, const float*, float*);

int main( int argc, char* argv[] )
{
  float results_data[O_SIZE] = {0};
  printf("2dsepconv (OH,OW,KH,KW,IH,IW) = (%ld, %ld, %ld, %ld, %ld, %ld)\n", OH, OW, KH, KW, IH, IW);
  printf("operations = %ld\n", (IW-KW+1)*(IH-KH+1)*(KW+KH));
#if PREALLOCATE
  // If needed we preallocate everything in the caches
  vec_sep_conv(OH, OW, IW, OW, input_k1, input_k2, input_image, results_data);
  memset(results_data, 0, sizeof(results_data));
#endif

  // Do the convolution
  setStats(1);
  vec_sep_conv(OH, OW, IW, OW, input_k1, input_k2, input_image, results_data);
  setStats(0);

  // Check the results
  return verifyFloat( O_SIZE, results_data, verify_data );
}
