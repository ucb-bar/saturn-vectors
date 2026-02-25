import struct

def float_to_bits(f):
    """Convert a Python float to its IEEE754 binary32 bit pattern."""
    [bits] = struct.unpack('>I', struct.pack('>f', f))
    return bits

def encode_fp8(value, exp_bits, mant_bits):
    """
    Encode a Python float into an 8-bit FP format with given exponent and mantissa bits.
    Uses round-to-nearest-even on the mantissa and clamps overflow to Inf, underflow to zero.
    """
    bias = (1 << (exp_bits - 1)) - 1
    bits = float_to_bits(value)
    sign = (bits >> 31) & 0x1
    exp = (bits >> 23) & 0xFF
    mant = bits & 0x7FFFFF

    # Handle Inf/NaN
    if exp == 0xFF:
        new_exp = (1 << exp_bits) - 1
        new_mant = 1 if mant != 0 else 0  # NaN if mantissa non-zero
    else:
        new_exp = exp - 127 + bias
        if new_exp >= (1 << exp_bits) - 1:
            # Overflow → Inf
            new_exp = (1 << exp_bits) - 1
            new_mant = 0
        elif new_exp <= 0:
            # Underflow → zero (ignoring subnormals)
            new_exp = 0
            new_mant = 0
        else:
            # Round mantissa down to target bits
            shift = 23 - mant_bits
            mant_rounded = mant + (1 << (shift - 1))  # add half for round-to-nearest
            new_mant = mant_rounded >> shift
            # handle mantissa overflow from rounding
            if new_mant == (1 << mant_bits):
                new_exp += 1
                new_mant = 0
            if new_exp >= (1 << exp_bits) - 1:
                # became too large
                new_exp = (1 << exp_bits) - 1
                new_mant = 0

    # Pack into 8 bits
    return (sign << (exp_bits + mant_bits)) | (new_exp << mant_bits) | new_mant

def encode_e4m3_array(arr):
    """Encode a list of floats to FP8 E4M3 (1 sign, 4 exp, 3 mant)"""
    return [encode_fp8(x, 4, 3) for x in arr]

def encode_e5m2_array(arr):
    """Encode a list of floats to FP8 E5M2 (1 sign, 5 exp, 2 mant)"""
    return [encode_fp8(x, 5, 2) for x in arr]

# Example usage with a 16-element array:
fp8_input = [
    0.0, 1.0, -1.0, 0.5,
    -0.5, 1.5, -2.3, 3.5,
    -0.1, 255.0, 1e-3, 1e3,
    float('inf'), float('-inf'), float('nan'), 2**-8
]

e4m3_out = encode_e4m3_array(fp8_input)
e5m2_out = encode_e5m2_array(fp8_input)

print("FP8 E4M3 Encoded uint8 values:", e4m3_out)
print("FP8 E5M2 Encoded uint8 values:", e5m2_out)
