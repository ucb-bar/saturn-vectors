#!/usr/bin/env python3

import numpy as np
import argparse

n = 10000

parser = argparse.ArgumentParser(description='A script to generate input data for the vec-tasks benchmark.')

parser.add_argument('-n', type=int, help='N dimension of inputs')

args = parser.parse_args()

if args.n:
    n = args.mdim


print(f'''#define DATA_SIZE {n}
''')

def print_array(name, data, data_size, data_type='uint32_t', data_fmt='{}', fold=10):
    print(f"{name} [{data_size}] = {{")
    for i in range(0, len(data), fold):
        print('  ', ', '.join(data_fmt.format(x) for x in data[i:i+fold]), ',', sep='')
    print('};')

input_data = np.arange(0, n, 1, dtype=np.uintc)
verify_data = (input_data * 2) + 3

print_array('static uint32_t input_data', input_data, 'DATA_SIZE')
print_array('static uint32_t verify_data', verify_data, 'DATA_SIZE')
