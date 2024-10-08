/*
    Original implementation taken from
    https://github.com/longcw/RoIAlign.pytorch No license found on the website.
    A question about the license was made here
    https://github.com/longcw/RoIAlign.pytorch/issues/48 Following the answer to
    this question, a correct header will be added also here
    Adaptation by: Matteo Perotti, ETH Zurich, <mperotti@iis.ee.ethz.ch>
*/

#include <stdint.h>
#include <string.h>

#include "roi_align.h"
#include "util.h"
#include "ara/util.h"
#include <stdio.h>

#define EXTRAPOLATION_VALUE 0

extern uint64_t BATCH_SIZE;
extern uint64_t DEPTH;
extern uint64_t IMAGE_HEIGHT;
extern uint64_t IMAGE_WIDTH;
extern uint64_t N_BOXES;
extern uint64_t CROP_HEIGHT;
extern uint64_t CROP_WIDTH;

extern float image_data[];
extern float boxes_data[];
extern int box_index_data[];
extern float crops_data[];
extern float crops_data_vec[];

// Compare the vector and scalar implementation.
// Return 0 if no error is found
// Return -1 if we have an error on the first element
// A positive return value indicates the index of the faulty element
int verify_result(float *s_crops_data, float *v_crops_data, size_t size,
                  float delta) {
  int ret;

  for (unsigned long int i = 0; i < size; ++i) {
    if (!similarity_check_32b(s_crops_data[i], v_crops_data[i], delta)) {
      ret = (!i) ? -1 : i;
      return ret;
    }
  }

  return 0;
}

int main() {
  printf("RoI Align\n");

  int64_t err;
  unsigned long cycles1, cycles2, instr2, instr1;
  uint64_t runtime_s, runtime_v;
  uint64_t result_size = N_BOXES * DEPTH * CROP_HEIGHT * CROP_WIDTH;


  // Parameters
  printf("BATCH_SIZE = %ld\nDEPTH = %ld\nIMAGE_HEIGHT = %ld\nIMAGE_WIDTH = "
         "%ld\nN_BOXES = %ld\nCROP_HEIGHT = %ld\nCROP_WIDTH = "
         "%ld\nEXTRAPOLATION_VALUE = %ld\n",
         BATCH_SIZE, DEPTH, IMAGE_HEIGHT, IMAGE_WIDTH, N_BOXES, CROP_HEIGHT,
         CROP_WIDTH, EXTRAPOLATION_VALUE);

  // Scalar benchmark
  printf("Starting scalar benchmark...\n");
  instr1 = read_csr(minstret);
  cycles1 = read_csr(mcycle);
  CropAndResizePerBox(image_data, BATCH_SIZE, DEPTH, IMAGE_HEIGHT, IMAGE_WIDTH,
                      boxes_data, box_index_data, 0, N_BOXES, crops_data,
                      CROP_HEIGHT, CROP_WIDTH, EXTRAPOLATION_VALUE);
  asm volatile("fence");
  instr2 = read_csr(minstret);
  cycles2 = read_csr(mcycle);
  runtime_s = cycles2 - cycles1;
  printf("Scalar benchmark complete.\n");
  printf("Cycles: %d\n", runtime_s);

  // Vector benchmark
  printf("Starting vector benchmark...\n");
  instr1 = read_csr(minstret);
  cycles1 = read_csr(mcycle);
  CropAndResizePerBox_BCHW_vec(image_data, BATCH_SIZE, DEPTH, IMAGE_HEIGHT,
                               IMAGE_WIDTH, boxes_data, box_index_data, 0,
                               N_BOXES, crops_data_vec, CROP_HEIGHT, CROP_WIDTH,
                               EXTRAPOLATION_VALUE);
  asm volatile("fence");
  instr2 = read_csr(minstret);
  cycles2 = read_csr(mcycle);
  runtime_v = cycles2 - cycles1;
  printf("Vector benchmark complete.\n");
  printf("Cycles: %d\n", runtime_v);
  printf("Operations: %ld\n", N_BOXES * CROP_WIDTH * CROP_HEIGHT * DEPTH * 6);
  printf("Loads: %ld\n", N_BOXES * CROP_WIDTH * CROP_HEIGHT * DEPTH * 4);
  printf("Stores: %ld\n", N_BOXES * CROP_WIDTH * CROP_HEIGHT * DEPTH);


  // Check for errors
  err = verify_result(crops_data, crops_data_vec, result_size, DELTA);

  if (err != 0) {
    // Fix return code to match the index of the faulty element
    err = (err == -1) ? 0 : err;
    printf("Failed. Index %d: %x != %x\n", err, *((uint32_t *)&crops_data[err]),
           *((uint32_t *)&crops_data_vec[err]));
    return err;
  } else {
    printf("Passed.\n");
  }

  return 0;
}
