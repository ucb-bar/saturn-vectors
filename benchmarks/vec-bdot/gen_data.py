#!/usr/bin/env python3

import random
from typing import TextIO

n = 64

a = [random.randrange(256) for _ in range(n ** 2)]
b = [random.randrange(256) for _ in range(n ** 2)]

r = [
    sum(a[x * n + i] * b[y * n + i] for i in range(n))
    for x in range(n)
    for y in range(n)
]

rt = [
    sum(a[x + i * n] * b[y + i * n] for i in range(n))
    for x in range(n)
    for y in range(n)
]

def write_matrix(file: TextIO, name: str, data: list[int], size: int):
    file.write(f".global {name}\n")
    file.write(f".balign 64\n")
    file.write(f"{name}:\n")
    for i in range((n * n * size) // 32):
        data_slice = reversed(data[i * (32 // size):(i + 1) * (32 // size)])
        data_hex = "".join(map(lambda x: format(x, f"0{size // 4}x"), data_slice))
        file.write(f"\t.word 0x{data_hex}\n")

with open("data.S", "w") as file:
    file.write(".section .data,\"aw\",@progbits\n")
    file.write(".global N\n")
    file.write(".balign 8\n")
    file.write("N:\n")
    file.write(f"\t.word 0x{n:08x}\n")
    write_matrix(file, "a", a, 8)
    write_matrix(file, "b", b, 8)
    write_matrix(file, "r", r, 32)
    write_matrix(file, "rt", rt, 32)