"""List the Verilog files that make up one module's hierarchy.

    collect_hier.py <gen-collateral dir> <top module>

CIRCT writes one module per file in gen-collateral, named after the module, so
the hierarchy is found by following instantiations from the top file.
"""
import os
import re
import sys

# "  ModuleName instName (" or "  ModuleName #(...) instName ("
INST = re.compile(r"^\s*([A-Za-z_][A-Za-z0-9_$]*)\s+(?:#\s*\(.*?\)\s*)?[A-Za-z_][A-Za-z0-9_$]*\s*\(",
                  re.M)


def hierarchy(gen, top):
    files = {os.path.splitext(f)[0]: os.path.join(gen, f)
             for f in os.listdir(gen) if f.endswith((".sv", ".v"))}
    seen, todo = set(), [top]
    while todo:
        m = todo.pop()
        if m in seen:
            continue
        if m not in files:
            sys.exit(f"module {m} has no file in {gen}")
        seen.add(m)
        with open(files[m]) as f:
            todo += [n for n in INST.findall(f.read()) if n in files and n not in seen]
    return sorted(files[m] for m in seen)


if __name__ == "__main__":
    if len(sys.argv) != 3:
        sys.exit("usage: collect_hier.py <gen-collateral dir> <top module>")
    print("\n".join(hierarchy(sys.argv[1], sys.argv[2])))
