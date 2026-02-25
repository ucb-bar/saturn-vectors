#!/usr/bin/env python3
from math import inf, nan
from typing import List, Tuple

def _bits_to_u8(b: str) -> int:
    """Accepts '01010101', '0b01010101', or with underscores; returns 0..255."""
    s = b.strip().lower().replace("_", "")
    if s.startswith("0b"):
        s = s[2:]
    if len(s) != 8 or any(ch not in "01" for ch in s):
        raise ValueError(f"Not an 8-bit binary literal: {b!r}")
    return int(s, 2)

def _decode_e4m3(u: int) -> Tuple[str, float]:
    s = (u >> 7) & 1
    e = (u >> 3) & 0xF   # 4 exponent bits
    m = u & 0x7          # 3 mantissa bits
    bias = 7
    mbits = 3

    if e == 0:
        if m == 0:
            return ("zero", -0.0 if s else 0.0)
        # subnormal: v = (-1)^s * 2^(1-bias) * (m / 2^mbits)
        v = ((-1.0) ** s) * (2.0 ** (1 - bias)) * (m / (1 << mbits))
        return ("subnormal", v)
    if e == 0b1111:
        # E4M3 has no infinity; only one NaN payload is defined: mantissa=0b111
        if m == 0b111:
            return ("nan", nan)
        # otherwise this is a normal number with unbiased exponent e-bias = 8
    # normal: v = (-1)^s * 2^(e-bias) * (1 + m/2^mbits)
    v = ((-1.0) ** s) * (2.0 ** (e - bias)) * (1.0 + (m / (1 << mbits)))
    return ("normal", v)

def _decode_e5m2(u: int) -> Tuple[str, float]:
    s = (u >> 7) & 1
    e = (u >> 2) & 0x1F  # 5 exponent bits
    m = u & 0x3          # 2 mantissa bits
    bias = 15
    mbits = 2

    if e == 0:
        if m == 0:
            return ("zero", -0.0 if s else 0.0)
        # subnormal
        v = ((-1.0) ** s) * (2.0 ** (1 - bias)) * (m / (1 << mbits))
        return ("subnormal", v)
    if e == 0b11111:
        if m == 0:
            return ("inf", -inf if s else inf)
        return ("nan", nan)
    # normal
    v = ((-1.0) ** s) * (2.0 ** (e - bias)) * (1.0 + (m / (1 << mbits)))
    return ("normal", v)

def decode_fp8_list(bin_list: List[str], fmt: str):
    fmt = fmt.strip().upper()
    if fmt not in {"E4M3", "E5M2"}:
        raise ValueError("fmt must be 'E4M3' or 'E5M2'")
    decoder = _decode_e4m3 if fmt == "E4M3" else _decode_e5m2

    out = []
    for idx, b in enumerate(bin_list):
        u8 = _bits_to_u8(b)
        kind, val = decoder(u8)
        out.append((idx, b, u8, kind, val))
    return out

if __name__ == "__main__":
    # Example: replace with your 15 binaries
    bins = [
        "00000000","00000001","00000100","01111110","01111111",
        "10000000","11110000","11110111","11111000","11111111",
        "00101010","10101010","01010101","00001111","10001111",
    ]
    fmt = "E4M3"  # or "E5M2"

    rows = decode_fp8_list(bins, fmt)
    print("Uint8 representation:", [l[2] for l in rows])
    print(f"Format: {fmt}")
    print(f"{'i':>2}  {'bits':8}  {'u8':>3}  {'class':10}  value")
    for i, bits, u8, kind, val in rows:
        # Pretty print special values without scientific notation noise
        if kind in {"nan", "inf"}:
            val_str = str(val)
        else:
            val_str = f"{val:.10g}"
        print(f"{i:2d}  {bits:8}  {u8:3d}  {kind:10}  {val_str}")

