#!/usr/bin/env python3

# Script for generating a basic transpose test case

import numpy as np

dim_m = 32
dim_n = 32

input_matrix = np.random.randint(0, 100, size=(dim_m, dim_n), dtype=np.int32)
transpose_matrix = input_matrix.T

print('''#define DIM_M {}
#define DIM_N {}
#define ARRAY_SIZE {}

'''.format(dim_m, dim_n, dim_m*dim_n))

def print_array(name, data, data_size, data_type='float', data_fmt='{}', fold=8):
    print('{} {}[{}] = {{'.format(data_type, name, data_size))
    for i in range(0, len(data), fold):
        print('  ', ', '.join(data_fmt.format(x) for x in data[i:i+fold]), ',', sep='')
    print('};')

print_array('input_matrix', input_matrix.flatten(), 'ARRAY_SIZE')
print_array('verify_data', input_matrix.T.flatten(), 'ARRAY_SIZE')
