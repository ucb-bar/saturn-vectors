#!/usr/bin/env python3
"""Fit cycles = a + b * iterations per pattern from opu-timing's output.

b is the steady-state cost per iteration. Also prints the largest residual
and whether the two runs at each count took identical cycles."""
import sys
from collections import defaultdict

runs, names = defaultdict(list), {}
for line in open(sys.argv[1]):
    if line.startswith("# "):
        tag, rest = line[2:].split(" ", 1)
        names[tag] = rest.strip()
        continue
    parts = line.split()
    if len(parts) == 3 and parts[0] in names:
        runs[parts[0]].append((int(parts[1]), int(parts[2])))

print(f"{'':4} {'cycles/iter':>11} {'start-up':>9} {'max resid':>9} {'repeats':>9}  pattern")
for tag, pts in runs.items():
    xs, ys = [p[0] for p in pts], [p[1] for p in pts]
    n, mx, my = len(xs), sum(xs) / len(xs), sum(ys) / len(ys)
    b = sum((x - mx) * (y - my) for x, y in pts) / sum((x - mx) ** 2 for x in xs)
    a = my - b * mx
    resid = max(abs(y - (a + b * x)) for x, y in pts)
    by_n = defaultdict(set)
    for x, y in pts:
        by_n[x].add(y)
    same = "identical" if all(len(v) == 1 for v in by_n.values()) else "DIFFER"
    print(f"{tag:4} {b:11.2f} {a:9.0f} {resid:9.1f} {same:>9}  {names[tag]}")
