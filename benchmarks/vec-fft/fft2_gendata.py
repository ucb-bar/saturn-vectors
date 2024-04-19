#!/usr/bin/env python3

import numpy
import argparse

parser = argparse.ArgumentParser(description='Generate fft2 dataset')
parser.add_argument('log2size', metavar='M', type=int, nargs='?', default=6, help='log2(FFT size)')
args = parser.parse_args()

M = args.log2size
N = 1 << M

dtype = numpy.float32
info = numpy.finfo(dtype)
nmant = 8 # Limit precision to avoid rounding errors
maxmant = 1 << nmant
minexp = 1 - nmant # info.minexp / 2
maxexp = -3 # (info.maxexp / 2) - nmant

# Generate floating-point values with exact mantissa and exponent
randf = lambda n: numpy.ldexp(
    numpy.random.randint(maxmant, size=n),
    numpy.random.randint(minexp, maxexp, size=n))

Xr = randf(N).astype(dtype)
Xi = randf(N).astype(dtype)
X = Xr.astype(numpy.complex128 if dtype == numpy.float64 else numpy.complex64)
X.imag = Xi
Y = numpy.fft.fft(X)

# Precompute "long weight vector" (Baily 1987), which stores twiddle
# factors for each stage separately to enable unit-stride access
#
#   for stage p = 1 to M
#     for k = 0 to N/(2^p) - 1
#       A = k * 2^{p-1}
#       W = e^{-j 2\pi A / N} = e^{-j \pi k 2^p / N}
#
# Omit final two stages (i = 1, 0) with trivial twiddle factor
# components {-1, 0, 1}
angles = []
for i in range(M-1, -1, -1):
    size = 1 << i # N / 2^p
    for k in range(0, size):
        angles.append((-k / size) * numpy.pi)

Wr = numpy.cos(angles).astype(dtype)
Wi = numpy.sin(angles).astype(dtype)


def print_array(name, data, data_size='DATA_SIZE', fold=10):
    print('float {}[{}] = {{'.format(name, data_size))
    for i in range(0, len(data), fold):
        print('  ', ', '.join('{}f'.format(x) for x in data[i:i+fold]), ',', sep='')
    print('};')

print('#define LOG2_DATA_SIZE {}'.format(M))
print('#define DATA_SIZE {}'.format(N))
print_array('input_Xr', Xr)
print_array('input_Xi', Xi)
print_array('input_Wr', Wr, 'DATA_SIZE-1')
print_array('input_Wi', Wi, 'DATA_SIZE-1')
print_array('verify_Xr', Y.real.astype(dtype))
print_array('verify_Xi', Y.imag.astype(dtype))
