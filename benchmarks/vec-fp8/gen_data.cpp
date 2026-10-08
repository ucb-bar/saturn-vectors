#include <random>
#include <cstdio>
#include <iomanip>
#include <limits>

#include <climits>
#include "lo_float.h"

#define ELEMENT_COUNT 2048

int main() {

	// using fp8 = lo_float::float8_e4m3_fn<lo_float::RoundToNearestEven>;
	using fp8 = lo_float::float8_e5m2<lo_float::RoundToNearestEven>;

	std::random_device rand;
	std::mt19937 gen(rand());
	std::uniform_real_distribution<float> dist(0.5, 0.1);

	fp8 v8a[ELEMENT_COUNT];
	fp8 v8b[ELEMENT_COUNT];

	for (size_t i = 0; i < ELEMENT_COUNT; i ++) {
		v8a[i] = static_cast<fp8>(dist(gen));
		v8b[i] = static_cast<fp8>(dist(gen));
	}
	
	std::ios default_state(NULL);
	std::cout << ".section .data,\"aw\",@progbits" << std::endl;

	default_state.copyfmt(std::cout);
	std::cout << ".global v8a" << std::endl;
	std::cout << ".balign 8" << std::endl;
	std::cout << "v8a:" << std::endl;
	std::cout << std::hex << std::setw(8) << std::setfill('0');
	for (size_t i = 0; i < ELEMENT_COUNT / 4; i ++) {
		std::cout << "    .word 0x";
		for (size_t j = 0; j < 4; j ++) {
			std::cout << std::hex << std::setw(2) << std::setfill('0') << (int) v8a[i * 4 + j].rep();
		}
		std::cout << std::endl;
	}
	std::cout.copyfmt(default_state);

	default_state.copyfmt(std::cout);
	std::cout << ".global v8b" << std::endl;
	std::cout << ".balign 8" << std::endl;
	std::cout << "v8b:" << std::endl;
	std::cout << std::hex << std::setw(8) << std::setfill('0');
	for (size_t i = 0; i < ELEMENT_COUNT / 4; i ++) {
		std::cout << "    .word 0x";
		for (size_t j = 0; j < 4; j ++) {
			std::cout << std::hex << std::setw(2) << std::setfill('0') << (int) v8b[i * 4 + j].rep();
		}
		std::cout << std::endl;
	}
	std::cout.copyfmt(default_state);

	default_state.copyfmt(std::cout);
	std::cout << ".section .data,\"aw\",@progbits" << std::endl;
	std::cout << ".global v8c" << std::endl;
	std::cout << ".balign 8" << std::endl;
	std::cout << "v8c:" << std::endl;
	for (size_t i = 0; i < ELEMENT_COUNT / 4; i ++) {
		std::cout << "    .word 0x";
		for (size_t j = 0; j < 4; j ++) {
			std::cout << std::hex << std::setw(2) << std::setfill('0') << (int) (v8a[i * 4 + j] * v8b[i * 4 + j]).rep();
		}
		std::cout << std::endl;
	}
	std::cout.copyfmt(default_state);

	fp8 ia = static_cast<fp8>(std::numeric_limits<float>::infinity());
	fp8 ib = static_cast<fp8>(dist(gen));
	std::cout << std::hex << (int) ia.rep() << std::endl;
	std::cout << std::hex << (int) ib.rep() << std::endl;
	std::cout << std::hex << (int) (ia * ib).rep() << std::endl;

	return 0;
}