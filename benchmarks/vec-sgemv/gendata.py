#!/usr/bin/env python3

import numpy

row_size = 128
col_size = 128

info = numpy.finfo(numpy.float64)
nmant = 1 # Limit precision to avoid rounding errors
maxmant = 1 << nmant
minexp = 0
maxexp = 3

# Generate floating-point values with exact mantissa and exponent
randf = lambda n: numpy.ldexp(
    numpy.random.randint(maxmant, size=n),
    numpy.random.randint(minexp, maxexp, size=n))

A = randf((row_size, col_size)).astype(numpy.float64)
x = randf(row_size).astype(numpy.float64)
result = numpy.dot(numpy.transpose(x), A)

def print_array(name, data, data_size, data_type='float', data_fmt='{}', fold=8):
    print('{} {}[{}] = {{'.format(data_type, name, data_size))
    for i in range(0, len(data), fold):
        print('  ', ', '.join(data_fmt.format(x) for x in data[i:i+fold]), ',', sep='')
    print('};')

print('#define ROW_SIZE {}'.format(row_size))
print('#define COL_SIZE {}'.format(col_size))
print('#define DIM_SIZE {}'.format(col_size*row_size))

print_array('input_data_A', A.flatten(), 'ROW_SIZE * COL_SIZE')
print_array('input_data_x', x, 'ROW_SIZE')
print_array('verify_data', result, 'ROW_SIZE')
