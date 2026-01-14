#!/usr/bin/env python3

import numpy as np
import argparse

m_dim = 32
n_dim = 32

parser = argparse.ArgumentParser(description='A script to generate input data for an SGEMM kernel.')

parser.add_argument('--mdim', type=int, help='M dimension of inputs')
parser.add_argument('--ndim', type=int, help='N dimension of inputs')
parser.add_argument('--size', type=int, help='Dimensions of NxN inputs')

args = parser.parse_args()

if args.size:
    m_dim = args.size
    n_dim = args.size
else:
    if args.mdim:
        m_dim = args.mdim
    if args.ndim:
        n_dim = args.ndim

c_in = np.arange(m_dim*n_dim).reshape(m_dim, n_dim).astype(np.int32)
c_matrix = c_in.T

print(f'''#define M_DIM {m_dim}
#define N_DIM {n_dim}

''')

def print_array(name, data, data_size, data_type='int8_t', data_fmt='{}', fold=10):
    print(f"{name} [{data_size}] = {{")
    for i in range(0, len(data), fold):
        print('  ', ', '.join(data_fmt.format(x) for x in data[i:i+fold]), ',', sep='')
    print('};')

print_array('static int32_t c_in', c_in.flatten(), 'M_DIM*N_DIM', data_type='int32_t')
print_array('static int32_t verify_data', c_matrix.flatten(), 'N_DIM*M_DIM', data_type='int32_t')
