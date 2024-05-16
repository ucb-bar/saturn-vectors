#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "util.h"

const char* input = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum";


size_t strlen_rvv(const char *s);

int main() {
  size_t cycles1, cycles2;
  size_t max = strlen(input);
  printf("Performing strlen with max len = %ld\n", max);

  cycles1 = read_csr(mcycle);
  for (size_t i = 0; i < max; i += 15) {
    size_t r = strlen_rvv(input + i);
    if (r != max - i) {
      return 1;
    }
  }
  cycles2 = read_csr(mcycle);
  printf("The execution took %ld cycles.\n", cycles2 - cycles1);
  return 0;
}
