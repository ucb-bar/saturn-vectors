#!/usr/bin/env python3

import numpy as np

KH = 3
KW = 3
IH = 10
IW = 10
OH = IH - KH + 1
OW = IW - KW + 1

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
weights = np.ones((KH, KW), dtype=np.float32)
weights_1 = np.ones(KW, dtype=np.float32)
weights_2 = np.ones(KH, dtype=np.float32)
outputs = np.full((OH, OW), np.float32(0.0))

# Convolution
for kh in range(KH):
    for kw in range(KW):
        outputs += inputs[kh:(kh+OH),kw:(kw+OW)] * weights[kh][kw]

print('''#define KH {}
#define KW {}
#define IH {}
#define IW {}
#define I_SIZE {}
#define OH {}
#define OW {}
#define O_SIZE {}

'''.format(KH, KW, IH, IW, IH*IW, OH, OW, OH*OW))

def print_array(name, data, data_size, data_type='float', data_fmt='{}', fold=10):
    print('{} {}[{}] = {{'.format(data_type, name, data_size))
    for i in range(0, len(data), fold):
        print('  ', ', '.join(data_fmt.format(x) for x in data[i:i+fold]), ',', sep='')
    print('};')

print_array('input_k1', weights_1, 'IW')
print_array('input_k2', weights_2, 'IH')
print_array('input_image', inputs.flatten(), 'I_SIZE')
print_array('verify_data', outputs.flatten(), 'O_SIZE')
