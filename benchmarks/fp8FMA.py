import struct
from typing import List

# ---------- float32 <-> bits ----------
def f32_to_bits(f: float) -> int:
    return struct.unpack('>I', struct.pack('>f', float(f)))[0]

def bits_to_f32(u: int) -> float:
    return struct.unpack('>f', struct.pack('>I', u & 0xFFFFFFFF))[0]

# ---------- FP8 encode (same policy you had; subnormals -> 0 for MVP) ----------
def encode_fp8(value: float, exp_bits: int, mant_bits: int) -> int:
    bias = (1 << (exp_bits - 1)) - 1
    bits = f32_to_bits(value)
    sign = (bits >> 31) & 0x1
    exp  = (bits >> 23) & 0xFF
    mant = bits & 0x7FFFFF

    if exp == 0xFF:  # Inf/NaN
        new_exp = (1 << exp_bits) - 1
        new_mant = 1 if mant != 0 else 0
    else:
        new_exp = exp - 127 + bias
        if new_exp >= (1 << exp_bits) - 1:
            new_exp = (1 << exp_bits) - 1
            new_mant = 0
        elif new_exp <= 0:
            new_exp = 0
            new_mant = 0  # drop subnormals (simple MVP)
        else:
            shift = 23 - mant_bits
            mant_rounded = mant + (1 << (shift - 1))  # nearest
            new_mant = mant_rounded >> shift
            if new_mant == (1 << mant_bits):
                new_exp += 1
                new_mant = 0
            if new_exp >= (1 << exp_bits) - 1:
                new_exp = (1 << exp_bits) - 1
                new_mant = 0

    return (sign << (exp_bits + mant_bits)) | (new_exp << mant_bits) | new_mant

def encode_e4m3_array(arr: List[float]) -> List[int]:
    return [encode_fp8(x, 4, 3) for x in arr]

def encode_e5m2_array(arr: List[float]) -> List[int]:
    return [encode_fp8(x, 5, 2) for x in arr]

# ---------- FP8 decode -> float32 (mirrors MVP encode; subnormals -> 0) ----------
def decode_fp8(u8: int, exp_bits: int, mant_bits: int) -> float:
    sign = (u8 >> (exp_bits + mant_bits)) & 0x1
    exp  = (u8 >> mant_bits) & ((1 << exp_bits) - 1)
    mant = u8 & ((1 << mant_bits) - 1)
    bias = (1 << (exp_bits - 1)) - 1

    if exp == 0:
        return -0.0 if sign else 0.0
    if exp == (1 << exp_bits) - 1:
        return float('nan') if mant != 0 else (float('-inf') if sign else float('inf'))

    e32 = (exp - bias) + 127
    m32 = mant << (23 - mant_bits)
    bits = (sign << 31) | (e32 << 23) | m32
    return bits_to_f32(bits)

# ---------- float32 -> BF16 (round-to-nearest-even) ----------
def f32_to_bf16_bits(f: float) -> int:
    u = f32_to_bits(f)
    sign = (u >> 31) & 1
    exp  = (u >> 23) & 0xFF
    mant = u & 0x7FFFFF

    if exp == 0xFF:  # Inf/NaN: copy top bits; force qNaN if NaN mant collapses
        top = (u >> 16) & 0xFFFF
        if mant == 0:
            return top  # Inf
        # Ensure quiet NaN bit (MSB of BF16 mantissa) is set
        bf_mant = ((mant >> 16) | 0x40) & 0x7F
        return (sign << 15) | (0xFF << 7) | bf_mant

    # Round-to-nearest-even from f32 to bf16
    rounded = u + 0x00007FFF + ((u >> 16) & 1)
    return (rounded >> 16) & 0xFFFF

def to_bin16(u16: int) -> str:
    return format(u16 & 0xFFFF, '016b')

# ---------- Make two 8-lane FP8 vectors, multiply, emit BF16 goldens ----------
def make_golden_bf16(fp8_fmt: str, A_vals: List[float], B_vals: List[float]) -> None:
    assert len(A_vals) == len(B_vals) == 8
    if fp8_fmt.lower() == "e4m3":
        e, m = 4, 3
    elif fp8_fmt.lower() == "e5m2":
        e, m = 5, 2
    else:
        raise ValueError("fp8_fmt must be 'e4m3' or 'e5m2'")

    A_fp8 = [encode_fp8(x, e, m) for x in A_vals]
    B_fp8 = [encode_fp8(x, e, m) for x in B_vals]
    A_dec = [decode_fp8(x, e, m) for x in A_fp8]
    B_dec = [decode_fp8(x, e, m) for x in B_fp8]

    prod_f32   = [float(a*b) for a, b in zip(A_dec, B_dec)]
    prod_bf16  = [f32_to_bf16_bits(x) for x in prod_f32]

    print(f"\n=== FP8 {fp8_fmt.upper()} → multiply → BF16 goldens ===")
    print("A fp8 bytes:", ["0x%02x" % x for x in A_fp8])
    print("B fp8 bytes:", ["0x%02x" % x for x in B_fp8])
    for i,(af,bf,p,h) in enumerate(zip(A_dec, B_dec, prod_f32, prod_bf16)):
        print(f"lane{i}: A={af:+.6g}  B={bf:+.6g}  A*B={p:+.6g}  bf16=0x{h:04x} ({to_bin16(h)})")

if __name__ == "__main__":
    # Example vectors (tweak these, then copy the 8 bytes into the C test's A/B arrays)
    A = [0.0, 1.0, -1.0, 0.5, -0.5, 1.5, -2.0, 3.0]
    B = [0.0, 0.5, -0.5, 2.0,  3.0, -1.5, 1.25, -4.0]
    make_golden_bf16("e4m3", A, B)
    make_golden_bf16("e5m2", A, B)
