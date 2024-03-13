#ifndef UTASKS_H
#define UTASKS_H

#include <stdint.h>
#include <vector>
#include <list>
#include <cstddef>
#include <cassert>
#include <stdio.h>
#include <atomic>

class runner_t;

class mutex_t { // can't use std::mutex if we don't have pthreads
public:
  mutex_t() : next_ticket(0), now_serving(0) { }
  void lock() {
    size_t my_ticket = next_ticket.fetch_add(1, std::memory_order_relaxed);
    while (my_ticket != now_serving.load(std::memory_order_acquire)) { }
  }
  void unlock() { now_serving.fetch_add(1, std::memory_order_release); }
private:
  std::atomic<size_t> next_ticket;
  std::atomic<size_t> now_serving;
};

mutex_t printf_lock;

#define PRINTF(...) ({							\
      printf_lock.lock();						\
      printf(__VA_ARGS__);						\
      printf_lock.unlock();						\
})

class allocator_t {
public:
  virtual void* allocate(size_t n) = 0;
  virtual void deallocate(void* buf) = 0;
};


mutex_t heap_allocator_lock;
class heap_allocator_t : public allocator_t {
public:
  void* allocate(size_t n) {
    heap_allocator_lock.lock();
    void* buf = malloc(n);
    heap_allocator_lock.unlock();
    return buf;
  }
  void deallocate(void* buf) {
    heap_allocator_lock.lock();
    free(buf);
    heap_allocator_lock.unlock();
  }
};

class region_allocator_t : public allocator_t {
public:
  region_allocator_t(void* base, size_t size) : base(base), size(size), tail(0) { }
  void* allocate(size_t n) {
    lock.lock();
    if (tail + n > size) {
      lock.unlock();
      PRINTF("illegal allocation size %ld\n", size);
      exit(1);
    }
    void* r = (uint8_t*)base + tail;
    tail += n;
    lock.unlock();
    return r;
  }
  void deallocate(void* buf) { }
private:
  mutex_t lock;
  void* base;
  size_t size;
  size_t tail;
};

class alignas(64) task_t {
  friend class runner_t;
public:
  task_t() : may_finish(false) { }
  void set_may_finish() { may_finish = true; }
  bool __attribute__ ((noinline)) is_finished() { return may_finish && !has_work(); }
  void wait_for_finished() { while (!this->is_finished()) { }; }
  virtual void assign_runner(runner_t* runner) { };

private:
  virtual void run() = 0;
  virtual bool has_work() = 0;
protected:
  bool may_finish;
};

template <typename T>
class alignas(64) circular_buffer_t {
public:
  circular_buffer_t(size_t capacity, void* buffer) : buffer((T*)buffer), capacity(capacity), head_wide(0), tail_wide(0), size(0)  {
    //PRINTF("this head tail %p %p %p\n", this, &this->head, &this->tail);
    if (capacity < 4 || (capacity & (capacity-1)) != 0) {
      PRINTF("Illegal capacity %ld\n", capacity);
      exit(1);
    }
    mask = capacity - 1;
  }
  ~circular_buffer_t() {
    delete[] buffer;
  }

  size_t get_head() { return head_wide & mask; }
  size_t get_tail() { return tail_wide & mask; }

  T* push_allocate(size_t &n) {
    size_t s = size;
    size_t tail = get_tail();
    T* r = buffer + tail;
    size_t head = get_head();
    n = (n > capacity - s) ? capacity - s : n;
    size_t limit = ((head > tail) ? head : capacity) - tail;
    n = (n > limit) ? limit : n;
    return r;
  }
  void push_complete(size_t n) {
    asm volatile("amoadd.d zero, %0, (%1)\n" : : "r"(n), "r"(&tail_wide));
    asm volatile("amoadd.d zero, %0, (%1)\n" : : "r"(n), "r"(&size));
  }
  T* pop_allocate(size_t &n) {
    size_t s = size;
    size_t head = get_head();
    T* r = buffer + head;
    size_t tail = get_tail();
    n = (n > s) ? s : n;
    size_t limit = ((tail > head) ? tail : capacity) - head;
    n = (n > limit) ? limit : n;
    return r;
  }
  void pop_complete(size_t n) {
    asm volatile("amoadd.d zero, %0, (%1)\n" : : "r"(n), "r"(&head_wide));
    asm volatile("amoadd.d zero, %0, (%1)\n" : : "r"((~n)+1), "r"(&size));
  }

  bool busy() {
    size_t s = size;
    return s > 0;
  }
private:
  T* buffer;
  size_t mask;
  size_t capacity;
  size_t head_wide;
  size_t tail_wide;
public:
  size_t size;
};

template <typename T> class sink_t;
template <typename T> class source_t;

class runner_t {
public:
  runner_t(size_t id, allocator_t* allocator) : id(id), task_count(0), allocator(allocator) { };
  void run() {
    PRINTF("Starting runner %ld\n", id);
    while (1) {
      task_t* scheduled_task = schedule_task();
      if (scheduled_task) {
	scheduled_task->run();
      }
    }
  }

  void add_task(task_t* task) {
    task_lock.lock();
    tasks.push_back(task);
    task->assign_runner(this);
    task_count++;
    task_lock.unlock();
  }

  bool idle() { return task_count.load() == 0; }

  allocator_t* get_allocator() { return allocator; }

private:
  task_t* schedule_task() {
    task_lock.lock();
    for (auto& t : tasks) {
      if (t->has_work()) {
	task_lock.unlock();
	return t;
      }
    }
    std::list<task_t*>::iterator it = tasks.begin();
    while (it != tasks.end()) {
      if ((*it)->is_finished()) {
	it = tasks.erase(it);
	task_count--;
      }
      it++;
    }
    task_lock.unlock();
    return nullptr;
  }

  size_t id;
  mutex_t task_lock;
  std::atomic<size_t> task_count;
  std::list<task_t*> tasks;
  allocator_t* allocator;
};


template <typename T>
class sink_t : virtual public task_t {
  friend class source_t<T>;
public:
  sink_t(size_t buffer_size) : buffer_size(buffer_size), buffer(nullptr) { }
  ~sink_t() {
    runner->get_allocator()->deallocate(buffer_data);
    runner->get_allocator()->deallocate(buffer);
  }
  void assign_runner(runner_t* runner) {
    this->runner = runner;
    void* buff = runner->get_allocator()->allocate(sizeof(circular_buffer_t<T>));
    buffer_data = runner->get_allocator()->allocate(buffer_size * sizeof(T));
    buffer = new (buff) circular_buffer_t<T>(buffer_size, buffer_data);
  }
  size_t buffer_size;
  circular_buffer_t<T>* buffer;
private:
  runner_t* runner;
  void* buffer_data;
};

template <typename T>
class source_t : virtual public task_t {
public:
  void chain(sink_t<T>* next) {
    if (this->next) {
      PRINTF("Failed chain\n");
      exit(1);
    }
    this->next = next;
  }
  void terminate(T* out) {
    if (this->next) {
      PRINTF("Failed chain\n");
      exit(1);
    }
    output = out;
  }
protected:
  sink_t<T>* next;
  alignas(64) T* output;
};


template <typename T, typename U>
class pipe_task_t : public source_t<U>, public sink_t<T> {
public:
  pipe_task_t(size_t buffer_size, size_t max_chunk) : max_chunk(max_chunk), sink_t<T>(buffer_size) {
    //PRINTF("%p %p %p %p %p\n", &this->next, &this->output, &this->max_chunk, &this->buffer, &this->may_finish);
  }
  bool has_work() { return this->buffer->busy(); }
  virtual void kernel(T* in, U* out, size_t& n) = 0;
private:
  void run() {
    bool has_next = this->next != nullptr;
    circular_buffer_t<U>* next_buffer = has_next ? this->next->buffer : nullptr;
    size_t max_chunk = this->max_chunk;
    circular_buffer_t<T>* buffer = this->buffer;
    if (has_next) {
      while (1) {
	size_t n = max_chunk;
	T* input = buffer->pop_allocate(n);
	U* output = next_buffer->push_allocate(n);
	if (n == 0) break;
	kernel(input, output, n);
	buffer->pop_complete(n);
        asm volatile("fence");
	next_buffer->push_complete(n);
      }
      if (this->is_finished()) { this->next->set_may_finish(); }
    } else {
      while (1) {
	size_t n = max_chunk;
	T* input = buffer->pop_allocate(n);
	U* output = this->output;
	if (n == 0) break;
	kernel(input, output, n);
	buffer->pop_complete(n);
	this->output += n;
      }
    }
  }
  size_t max_chunk;
};

template <typename T>
class source_task_t : public source_t<T> {
public:
  source_task_t(size_t max_chunk) : max_chunk(max_chunk) { }
  virtual void kernel(T* out, size_t& n) = 0;
  virtual bool has_work() = 0;
private:
  void run() {
    bool has_next = this->next != nullptr;
    circular_buffer_t<T>* next_buffer = has_next ? this->next->buffer : nullptr;
    size_t max_chunk = this->max_chunk;
    if (has_next) {
      while (has_work()) {
	size_t n = max_chunk;
	T* output = next_buffer->push_allocate(n);
	kernel(output, n);
        asm volatile("fence");
	next_buffer->push_complete(n);
      }
      if (this->is_finished()) { this->next->set_may_finish(); }
    } else {
      while (has_work()) {
	size_t n = max_chunk;
	T* output = this->output;
	kernel(output, n);
	this->output += n;
      }
    }
  }
  size_t max_chunk;
};

template <typename T>
class sink_task_t : public sink_t<T> {
public:
  sink_task_t(size_t buffer_size, size_t max_chunk) : max_chunk(max_chunk), sink_t<T>(buffer_size) { }
  bool has_work() { return this->buffer->busy(); }
  virtual void kernel(T* in, size_t& n) = 0;
private:
  void run() {
    while (has_work()) {
      size_t n = this->max_chunk;
      T* input = this->buffer->pop_allocate(n);
      if (n == 0) return;
      kernel(input, n);
      this->buffer->pop_complete(n);
    }
  }
  size_t max_chunk;
};



#endif
