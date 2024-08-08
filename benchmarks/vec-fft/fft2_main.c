// See LICENSE for license details.

//**************************************************************************
// fft2 benchmark
//--------------------------------------------------------------------------
//

#include "util.h"
#include "fft2.h"

//--------------------------------------------------------------------------
// Input/Reference Data

#include "dataset1.h"

//--------------------------------------------------------------------------
// Main

int main( int argc, char* argv[] )
{
#if PREALLOCATE
  for (size_t i = 0; i < DATA_SIZE-1; i++) {
    volatile float tmp;
    tmp = input_Xr[i];
    tmp = input_Xi[i];
    tmp = input_Wr[i];
    tmp = input_Wi[i];
  }
#endif

  // Do the FFT
  setStats(1);
  fft2(input_Xr, input_Xi, input_Wr, input_Wi, DATA_SIZE, LOG2_DATA_SIZE);
  setStats(0);

#define VERIFY
#ifdef VERIFY
#define FFT_MAX_ERROR (10e-8f)
  // Check the results
  {
    size_t i;
    for (i = 0; i < DATA_SIZE; i++) {
      float rdiff, idiff, err;
      rdiff = input_Xr[i] - verify_Xr[i];
      idiff = input_Xi[i] - verify_Xi[i];

      err = (rdiff * rdiff) + (idiff * idiff);
      if (err > FFT_MAX_ERROR) {
        return (i + 1);
      }
    }
  }
#endif
  return 0;
}
