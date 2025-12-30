#!/usr/bin/env python3

import numpy as np
import argparse

m_dim = 49
n_dim = 49
k_dim = 3

parser = argparse.ArgumentParser(description='A script to generate input data for an SGEMM kernel.')

parser.add_argument('--mdim', type=int, help='M dimension of inputs')
parser.add_argument('--kdim', type=int, help='K dimension of inputs')
parser.add_argument('--ndim', type=int, help='N dimension of inputs')
parser.add_argument('--size', type=int, help='Dimensions of NxN inputs')

args = parser.parse_args()

if args.size:
    m_dim = args.size
    k_dim = args.size
    n_dim = args.size
else:
    if args.mdim:
        m_dim = args.mdim
    if args.kdim:
        k_dim = args.kdim
    if args.ndim:
        n_dim = args.ndim

a_matrix = np.arange(k_dim*m_dim).reshape(k_dim, m_dim).astype(np.int8)
b_matrix = np.arange(k_dim*n_dim).reshape(k_dim, n_dim).astype(np.int8)
c_bias = np.arange(n_dim).astype(np.int32)
c_matrix = np.matmul(a_matrix.T.astype(np.int32), b_matrix.astype(np.int32)) + c_bias[None, :]
c_matrix = c_matrix.T

print(f'''#define M_DIM {m_dim}
#define K_DIM {k_dim}
#define N_DIM {n_dim}

''')

def print_array(name, data, data_size, data_type='int8_t', data_fmt='{}', fold=10):
    print(f"{name} [{data_size}] = {{")
    for i in range(0, len(data), fold):
        print('  ', ', '.join(data_fmt.format(x) for x in data[i:i+fold]), ',', sep='')
    print('};')

print_array('static int8_t a_matrix', a_matrix.flatten(), 'M_DIM*K_DIM')
print_array('static int8_t b_matrix', b_matrix.flatten(), 'K_DIM*N_DIM')
print_array('static int32_t c_bias', c_bias.flatten(), 'N_DIM', data_type='int32_t')
print_array('static int32_t verify_data', c_matrix.flatten(), 'M_DIM*N_DIM', data_type='int32_t')
