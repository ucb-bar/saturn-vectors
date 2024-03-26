#!/usr/bin/env python3

import numpy

M_DIM = 128
N_DIM = 128

info = numpy.finfo(numpy.float64)
nmant = 1 # Limit precision to avoid rounding errors
maxmant = 1 << nmant
minexp = 0
maxexp = 3

# Generate floating-point values with exact mantissa and exponent
randf = lambda n: numpy.ldexp(
    numpy.random.randint(maxmant, size=n),
    numpy.random.randint(minexp, maxexp, size=n))

A = randf((M_DIM, N_DIM)).astype(numpy.float64)
x = randf(M_DIM).astype(numpy.float64)
result = numpy.dot(numpy.transpose(x), A)

def print_array(name, data, data_size, data_type='float', data_fmt='{}', fold=8):
    print('{} {}[{}] = {{'.format(data_type, name, data_size))
    for i in range(0, len(data), fold):
        print('  ', ', '.join(data_fmt.format(x) for x in data[i:i+fold]), ',', sep='')
    print('};')

print('#define M_DIM {}'.format(M_DIM))
print('#define N_DIM {}'.format(N_DIM))
print('#define DIM_SIZE {}'.format(M_DIM*N_DIM))

print_array('input_data_A', A.flatten(), 'M_DIM * N_DIM')
print_array('input_data_x', x, 'M_DIM')
print_array('verify_data', result, 'N_DIM')
