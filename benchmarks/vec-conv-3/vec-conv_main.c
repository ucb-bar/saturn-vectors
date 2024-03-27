// See LICENSE for license details.

//**************************************************************************
// 3x3 2D Convolution Benchmark
//--------------------------------------------------------------------------
//
// This benchmark tests a vectorized 2D 3x3 convolution implementation.

#include <string.h>
#include "util.h"

//--------------------------------------------------------------------------
// Input/Reference Data

#include "dataset1.h"

//--------------------------------------------------------------------------
// Main

void *vec_conv (size_t, size_t, size_t, size_t, const float*, const float*, float*);

int main( int argc, char* argv[] )
{
  float results_data[O_SIZE] = {0};

#if PREALLOCATE
  // If needed we preallocate everything in the caches
  vec_conv(OH, OW, IW, OW, input_k, input_image, results_data);
  memset(results_data, 0, sizeof(results_data));
#endif

  // Do the convolution
  setStats(1);
  vec_conv(OH, OW, IW, OW, input_k, input_image, results_data);
  setStats(0);

  // Check the results
  return verifyFloat( O_SIZE, results_data, verify_data );
}
