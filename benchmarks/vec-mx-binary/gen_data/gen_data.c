#include <stdio.h>
#include <stdint.h>
#include "../../common-data-gen/mx_data_gen.h"

#define COUNT 128
#define SPECIAL_COUNT 20

void test(size_t isize, size_t osize, char *name, double min, double max, double sn_min, double sn_max, mx_generator gen, mx_op_binary op) {

	fp_t vals_a[COUNT];
	fp_t vals_b[COUNT];
	fp_t vals_out[COUNT];

	for (size_t i = 0; i < COUNT; i ++) {
		vals_a[i] = gen(GM_RAND, min, max);
		vals_b[i] = gen(GM_RAND, min, max);
	}

	for (size_t i = 9; i < SPECIAL_COUNT; i ++) {
		vals_a[i] = gen(GM_RAND, sn_min, sn_max);
		vals_b[i] = gen(GM_RAND, sn_min, sn_max);
	}

	vals_a[0] = gen(GM_INF, min, max);
	vals_a[1] = gen(GM_NAN, min, max);

	vals_b[2] = gen(GM_INF, min, max);
	vals_b[3] = gen(GM_NAN, min, max);

	vals_a[4] = gen(GM_NINF, min, max);

	vals_a[5] = gen(GM_ZERO, min, max);
	vals_a[6] = gen(GM_NZERO, min, max);

	vals_a[7] = gen(GM_INF, min, max);
	vals_b[7] = gen(GM_NAN, min, max);

	vals_a[8] = gen(GM_INF, min, max);
	vals_b[8] = gen(GM_ZERO, min, max);

    for (size_t i = 0; i < COUNT; i ++) {
		vals_out[i] = op(vals_a[i], vals_b[i]);
	}

	print_array(name, "_a", vals_a, isize, COUNT);
	print_array(name, "_b", vals_b, isize, COUNT);
	print_array(name, "_out", vals_out, osize, COUNT);
}

#define TESTS_FMA_BINARY(size, wsize, name, min, max, sn_min, sn_max, gen, ops_name) \
	test(size, size, name "_mul", min, max, sn_min, sn_max, gen, ops_name ## _mul); \
	test(size, size, name "_add", min, max, sn_min, sn_max, gen, ops_name ## _add); \
	test(size, size, name "_sub", min, max, sn_min, sn_max, gen, ops_name ## _sub); \
	test(size, wsize, name "_wmul", min, max, sn_min, sn_max, gen, ops_name ## _wmul); \
	test(size, wsize, name "_wadd", min, max, sn_min, sn_max, gen, ops_name ## _wadd); \
	test(size, wsize, name "_wsub", min, max, sn_min, sn_max, gen, ops_name ## _wsub);

int main() {

	print_header();

    print_uint32("N", COUNT);

	TESTS_FMA_BINARY(2, 4, "fp16", -1e2, 1e2, -1e-6, 1e-6, gen_fp16, fp16)
	TESTS_FMA_BINARY(2, 4, "bf16", -1e15, 1e15, -2e-38, 2e-38, gen_bf16, bf16)
	TESTS_FMA_BINARY(1, 2, "e5m2", -1e2, 1e2, -1e-4, 1e-4, gen_e5m2, e5m2)
	TESTS_FMA_BINARY(1, 2, "e4m3", -3e1, 3e1, -1e-1, 1e-1, gen_e4m3, e4m3)

	/*
	Section from old tests for reference for decent min/max values
	
	test<fp32<rm>, 4, fp32name, -1e2, 1e2, -1e-6, 1e-6, false, fp32<rm>, rm_name>(); \
	test<fp16<rm>, 2, fp16name, -1e2, 1e2, -1e-6, 1e-6, false, fp16<rm>, rm_name>(); \
	test<bf16<rm>, 2, bf16name, -1e15, 1e15, -2e-38, 2e-38, false, bf16<rm>, rm_name>(); \
	test<ofp8e5m2<rm>, 1, ofp8e5m2name, -1e2, 1e2, -1e-4, 1e-4, false, ofp8e5m2<rm>, rm_name>(); \
	test<ofp8e4m3<rm>, 1, ofp8e4m3name, -3e1, 3e1, -1e-1, 1e-1, false, ofp8e4m3<rm>, rm_name>(); \
	test<fp16<rm>, 2, fp16Wname, -1e2, 1e2, -1e-6, 1e-6, true, fp32<rm>, rm_name>(); \
	test<bf16<rm>, 2, bf16Wname, -1e15, 1e15, -2e-38, 2e-38, true, fp32<rm>, rm_name>(); \
	test<ofp8e5m2<rm>, 1, ofp8e5m2Wname, -1e2, 1e2, -1e-4, 1e-4, true, bf16<rm>, rm_name>(); \
	test<ofp8e4m3<rm>, 1, ofp8e4m3Wname, -1e2, 1e2, -1e-3, 1e-3, true, bf16<rm>, rm_name>(); \
	*/

	return 0;
}