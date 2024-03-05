#!/usr/bin/env python3

import numpy as np
import argparse


m_dim = 8
k_dim = 8
n_dim = 8

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

a_array_size = m_dim * k_dim
b_array_size = k_dim * n_dim
c_array_size = m_dim * n_dim

info = np.finfo(np.float32)
maxmant = 1 << 5
minexp = -10
maxexp = 10

# Generate floating-point values with exact mantissa and exponent
randf = lambda n: np.ldexp(
    np.random.randint(-1*maxmant, maxmant, size=n),
    np.random.randint(minexp, maxexp, size=n))

a_matrix = randf((m_dim, k_dim)).astype(np.float32)
b_matrix = randf((k_dim, n_dim)).astype(np.float32)

c_matrix = np.dot(a_matrix, b_matrix)

print(f'''#define M_DIM {m_dim}
#define K_DIM {k_dim}
#define N_DIM {n_dim}

typedef float data_t;

''')

def print_array(name, data, data_size, data_type='float', data_fmt='{}', fold=10):
    print(f"{name} [{data_size}] = {{")
    for i in range(0, len(data), fold):
        print('  ', ', '.join(data_fmt.format(x) for x in data[i:i+fold]), ',', sep='')
    print('};')

print_array('static data_t a_matrix', a_matrix.flatten(), 'M_DIM*K_DIM')
print_array('static data_t b_matrix', b_matrix.flatten(), 'K_DIM*N_DIM')
print_array('static data_t verify_data', c_matrix.flatten(), 'M_DIM*N_DIM')
