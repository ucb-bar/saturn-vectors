// See LICENSE for license details.

#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#include "utasks.h"
#include "vec-tasks.h"
#include "util.h"
#include "dataset1.h"

// EDIT THIS
#define NUM_CORES (4)
#define NUM_RUNNERS (NUM_CORES-1)

mutex_t runner_lock;
runner_t* runners[NUM_RUNNERS] = {0};

uint32_t output_data[DATA_SIZE];

volatile std::atomic<size_t> initialized_runners = 0;

extern "C" void __main(void) {
  size_t mhartid = read_csr(mhartid);
  if (mhartid >= NUM_CORES) while (1);

  if (mhartid > 0) { // runner
    size_t runner_id = mhartid - 1;
    runner_lock.lock();
    if (mhartid == 1) {
      runners[runner_id] = new runner_t(runner_id, new heap_allocator_t);
    } else {
      size_t base = 0x70000000 + 0x10000 * mhartid;
      runners[runner_id] = new runner_t(runner_id, new region_allocator_t((void*)base, 0x10000));
    }

    initialized_runners++;
    runner_lock.unlock();

    runners[runner_id]->run(); // spins forever
    __builtin_unreachable();
  } else { // create and distribute tasks

    while (initialized_runners.load() < NUM_RUNNERS) { }; // wait for runners to come up

    // construct the tasks
    stream_task_t<uint32_t>* stream_task = new stream_task_t<uint32_t>(512);
    uint32_scale_task_t* scale_task = new uint32_scale_task_t(2, 2048, 512);
    uint32_add_task_t* add_task = new uint32_add_task_t(3, 2048, 512);

    // PRINTF("%p %p %p %ld\n", &printf_lock, runners[0], runners[1], sizeof(runner_t));
    // PRINTF("%p %p %ld %ld\n", stream_task, scale_task, sizeof(stream_task_t<uint32_t>), sizeof(uint32_scale_task_t));

    // construct the task graph
    stream_task->chain(scale_task);
    scale_task->chain(add_task);
    add_task->terminate(output_data);

    // assign tasks to runners
    runners[0]->add_task(stream_task);
    runners[1]->add_task(scale_task);
    runners[2]->add_task(add_task);

    // warm up the system, push the first set of tasks through
    stream_task->push_work(input_data, DATA_SIZE);
    while (add_task->has_work() || scale_task->has_work() || stream_task->has_work()) { };

    for (size_t i = 0; i < DATA_SIZE; i++) {
      uint32_t ref = verify_data[i];
      uint32_t out = output_data[i];
      if (out != ref) {
	PRINTF("early Mismatch %p %x != %x\n", &output_data[i], out, ref);
	exit(1);
      }
    }

    add_task->terminate(output_data);

    // measure the system, push the data through again
    PRINTF("Warmed up, starting measurement\n");
    size_t start = read_csr(mcycle);
    stream_task->push_work(input_data, DATA_SIZE);
    stream_task->set_may_finish();
    add_task->wait_for_finished();
    size_t end = read_csr(mcycle);

    PRINTF("task took %ld cycles\n", end - start);

    for (size_t i = 0; i < DATA_SIZE; i++) {
      uint32_t ref = verify_data[i];
      uint32_t out = output_data[i];
      if (out != ref) {
	PRINTF("Mismatch %ld %p %x != %x\n", i, &output_data[i], out, ref);
	exit(1);
      }
    }

    for (size_t i = 0; i < NUM_RUNNERS; i++) while (!runners[i]->idle()) { } // Wait for idle
  }
}

int main(void) {
  __main();
  return 0;
}
