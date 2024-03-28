#!/usr/bin/env python3

import numpy as np

K_DIM = 3
IH = 100
IW = 100
OH = IH - K_DIM + 1
OW = IW - K_DIM + 1

info = np.finfo(np.float32)
nmant = 5 # Limit precision to avoid rounding errors
maxmant = 1 << nmant
minexp = 0
maxexp = 5

# Generate floating-point values with exact mantissa and exponent
randf = lambda n: np.ldexp(
    np.random.randint(maxmant, size=n),
    np.random.randint(minexp, maxexp, size=n))

inputs = randf((IH, IW)).astype(np.float32)
weights = np.ones((K_DIM, K_DIM), dtype=np.float32)
outputs = np.full((OH, OW), np.float32(0.0))

# Convolution
for kh in range(K_DIM):
    for kw in range(K_DIM):
        outputs += inputs[kh:(kh+OH),kw:(kw+OW)] * weights[kh][kw]

print('''#define K_DIM {}
#define IH {}
#define IW {}
#define I_SIZE {}
#define OH {}
#define OW {}
#define O_SIZE {}

'''.format(K_DIM, IH, IW, IH*IW, OH, OW, OH*OW))

def print_array(name, data, data_size, data_type='float', data_fmt='{}', fold=10):
    print('{} {}[{}] = {{'.format(data_type, name, data_size))
    for i in range(0, len(data), fold):
        print('  ', ', '.join(data_fmt.format(x) for x in data[i:i+fold]), ',', sep='')
    print('};')

print_array('input_k', weights.flatten(), 'K_DIM*K_DIM')
print_array('input_image', inputs.flatten(), 'I_SIZE')
print_array('verify_data', outputs.flatten(), 'O_SIZE')
