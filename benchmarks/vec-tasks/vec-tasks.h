#ifndef VEC_TASKS_H
#define VEC_TASKS_H

#include <riscv_vector.h>
#include "utasks.h"
#include "util.h"

// Head of a chain. Streams some size of elements T from buffer at base into the next task
template <typename T>
class stream_task_t : public source_task_t<T> {
public:
  stream_task_t(size_t max_chunk) : source_task_t<T>(max_chunk) { }
  bool has_work() { return work_queue.size() > 0; }
  void push_work(T* base, size_t n) { work_queue.push_back(std::pair(base, n)); }
  void kernel(T* out, size_t& n) {
    if (work_queue.size() == 0) {
      n = 0;
      return;
    }
    T* base = work_queue.front().first;
    size_t remaining = work_queue.front().second;

    n = (n > remaining) ? remaining : n;

    // do the memcpy with bytes
    size_t avl = n * sizeof(T);
    uint8_t* out_bytes = (uint8_t*) out;
    uint8_t* base_bytes = (uint8_t*) base;

    while (avl > 0) {
      size_t consumed1 = __riscv_vsetvl_e8m8(avl);
      vuint8m8_t load1 = __riscv_vle8_v_u8m8(base_bytes, consumed1);
      avl -= consumed1;
      base_bytes += consumed1;
      size_t consumed2 = __riscv_vsetvl_e8m8(avl);
      vuint8m8_t load2 = __riscv_vle8_v_u8m8(base_bytes, consumed2);
      avl -= consumed2;
      base_bytes += consumed2;
      size_t consumed3 = __riscv_vsetvl_e8m8(avl);
      vuint8m8_t load3 = __riscv_vle8_v_u8m8(base_bytes, consumed3);
      avl -= consumed3;
      base_bytes += consumed3;
      size_t consumed4 = __riscv_vsetvl_e8m8(avl);
      vuint8m8_t load4 = __riscv_vle8_v_u8m8(base_bytes, consumed4);
      avl -= consumed4;
      base_bytes += consumed4;

      __riscv_vse8_v_u8m8(out_bytes, load1, consumed1);
      out_bytes += consumed1;
      __riscv_vse8_v_u8m8(out_bytes, load2, consumed2);
      out_bytes += consumed2;
      __riscv_vse8_v_u8m8(out_bytes, load3, consumed3);
      out_bytes += consumed3;
      __riscv_vse8_v_u8m8(out_bytes, load4, consumed4);
      out_bytes += consumed4;
    }
    work_queue.front().first += n;
    work_queue.front().second -= n;
    if (work_queue.front().second == 0) {
      work_queue.pop_front();
    }
  }
private:
  std::list<std::pair<T*,size_t>> work_queue;
};

class uint32_scale_task_t : public pipe_task_t<uint32_t, uint32_t> {
public:
  uint32_scale_task_t(uint32_t scale, size_t buffer_size, size_t max_chunk) : scale(scale), pipe_task_t<uint32_t, uint32_t>(buffer_size, max_chunk) { }
  void kernel(uint32_t* in, uint32_t* out, size_t& n) {
    size_t avl = n;
    while (avl > 0) {
      size_t consumed1 = __riscv_vsetvl_e32m8(avl);
      vuint32m8_t load1 = __riscv_vle32_v_u32m8(in, consumed1);
      load1 = __riscv_vmul_vx_u32m8(load1, scale, consumed1);
      avl -= consumed1;
      in += consumed1;
      size_t consumed2 = __riscv_vsetvl_e32m8(avl);
      vuint32m8_t load2 = __riscv_vle32_v_u32m8(in, consumed2);
      load2 = __riscv_vmul_vx_u32m8(load2, scale, consumed1);
      avl -= consumed2;
      in += consumed2;
      size_t consumed3 = __riscv_vsetvl_e32m8(avl);
      vuint32m8_t load3 = __riscv_vle32_v_u32m8(in, consumed3);
      load3 = __riscv_vmul_vx_u32m8(load3, scale, consumed1);
      avl -= consumed3;
      in += consumed3;
      size_t consumed4 = __riscv_vsetvl_e32m8(avl);
      vuint32m8_t load4 = __riscv_vle32_v_u32m8(in, consumed4);
      load4 = __riscv_vmul_vx_u32m8(load4, scale, consumed1);
      avl -= consumed4;
      in += consumed4;

      __riscv_vse32_v_u32m8(out, load1, consumed1);
      out += consumed1;
      __riscv_vse32_v_u32m8(out, load2, consumed2);
      out += consumed2;
      __riscv_vse32_v_u32m8(out, load3, consumed3);
      out += consumed3;
      __riscv_vse32_v_u32m8(out, load4, consumed4);
      out += consumed4;
    }
  }
private:
  uint32_t scale;
};

class uint32_add_task_t : public pipe_task_t<uint32_t, uint32_t> {
public:
  uint32_add_task_t(uint32_t add, size_t buffer_size, size_t max_chunk) : add(add), pipe_task_t<uint32_t, uint32_t>(buffer_size, max_chunk) { }
  void kernel(uint32_t* in, uint32_t* out, size_t& n) {
    size_t avl = n;
    while (avl > 0) {
      size_t consumed1 = __riscv_vsetvl_e32m8(avl);
      vuint32m8_t load1 = __riscv_vle32_v_u32m8(in, consumed1);
      load1 = __riscv_vadd_vx_u32m8(load1, add, consumed1);
      avl -= consumed1;
      in += consumed1;
      size_t consumed2 = __riscv_vsetvl_e32m8(avl);
      vuint32m8_t load2 = __riscv_vle32_v_u32m8(in, consumed2);
      load2 = __riscv_vadd_vx_u32m8(load2, add, consumed1);
      avl -= consumed2;
      in += consumed2;
      size_t consumed3 = __riscv_vsetvl_e32m8(avl);
      vuint32m8_t load3 = __riscv_vle32_v_u32m8(in, consumed3);
      load3 = __riscv_vadd_vx_u32m8(load3, add, consumed1);
      avl -= consumed3;
      in += consumed3;
      size_t consumed4 = __riscv_vsetvl_e32m8(avl);
      vuint32m8_t load4 = __riscv_vle32_v_u32m8(in, consumed4);
      load4 = __riscv_vadd_vx_u32m8(load4, add, consumed1);
      avl -= consumed4;
      in += consumed4;

      __riscv_vse32_v_u32m8(out, load1, consumed1);
      out += consumed1;
      __riscv_vse32_v_u32m8(out, load2, consumed2);
      out += consumed2;
      __riscv_vse32_v_u32m8(out, load3, consumed3);
      out += consumed3;
      __riscv_vse32_v_u32m8(out, load4, consumed4);
      out += consumed4;
    }
  }
private:
  uint32_t add;
};


#endif
