#include <riscv_vector.h>
#include <stdio.h>
#include <stdlib.h>

#define ELEMENT_COUNT 2048

extern char v8a[] __attribute__((aligned(256)));
extern char v8b[] __attribute__((aligned(256)));
extern char v8c[] __attribute__((aligned(256)));

int main() {
	uint64_t avl = ELEMENT_COUNT;
	size_t vl = __riscv_vsetvl_e8m8(avl);

	char *a = v8a;
	char *b = v8b;
	char *c = v8c;
	char res = 0;

	for (; avl > 0; avl -= vl) {
		vl = __riscv_vsetvl_e8m8(avl);
		printf("%d\n", vl);

		asm volatile("vle8.v v0, (%0)" : : "r"(a)); // Load a
		asm volatile("vle8.v v8, (%0)" : : "r"(b)); // Load b
		asm volatile("vle8.v v16, (%0)" : : "r"(c)); // Load c
		asm volatile("vfmul.vv v3, v2, v1"); // Multiply a * b
		asm volatile("vmsne.vv v24, v3, v2"); // Check where a * b != c
		asm volatile("vmv.v.i v0, 0"); // Clear accumulator
		asm volatile("vredor.vs v0, v24, v0"); // Reduce with or
		asm volatile("vmv.x.s %0, v24" : "=r"(res)); // Get result
		printf("%d\n", res);
		if (res) {
			printf("Incorrect\n");
			exit(1);
		return 3;
		}

		a += vl;
		b += vl;
	}
	printf("Correct\n");
	return 0;
}