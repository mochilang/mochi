# Code generated by Mochi transpiler.
# Version 0.10.42, generated on 2025-07-28 08:06 +0700
import json
import os
import resource
import time

import sys
sys.set_int_max_str_digits(0)


_now_seed = 0
_now_seeded = False
s = os.getenv("MOCHI_NOW_SEED")
if s and s != "":
    try:
        _now_seed = int(s)
        _now_seeded = True
    except Exception:
        pass

def _now():
    global _now_seed
    if _now_seeded:
        _now_seed = (_now_seed * 1664525 + 1013904223) % 2147483647
        return _now_seed
    return int(time.time_ns())

_bench_mem_start = resource.getrusage(resource.RUSAGE_SELF).ru_maxrss
_bench_start = _now()
def det(m):
    n = len(m)
    if n == 1:
        return m[0][0]
    total = 0.0
    sign = 1.0
    c = 0
    while c < n:
        sub = []
        r = 1
        while r < n:
            row = []
            cc = 0
            while cc < n:
                if cc != c:
                    row = row + [m[r][cc]]
                cc = cc + 1
            sub = sub + [row]
            r = r + 1
        total = total + sign * m[0][c] * det(sub)
        sign = sign * (-1.0)
        c = c + 1
    return total
def replaceCol(m, col, v):
    res = []
    r = 0
    while r < len(m):
        row = []
        c = 0
        while c < len(m[r]):
            if c == col:
                row = row + [v[r]]
            else:
                row = row + [m[r][c]]
            c = c + 1
        res = res + [row]
        r = r + 1
    return res
m = [[2.0, -1.0, 5.0, 1.0], [3.0, 2.0, 2.0, -6.0], [1.0, 3.0, 3.0, -1.0], [5.0, -2.0, -3.0, 3.0]]
v = [-3.0, -32.0, -47.0, 49.0]
d = det(m)
x = []
i = 0
while i < len(v):
    mc = replaceCol(m, i, v)
    x = x + [det(mc) / d]
    i = i + 1
s = "["
j = 0
while j < len(x):
    s = s + str(x[j])
    if j < len(x) - 1:
        s = s + " "
    j = j + 1
s = s + "]"
print(s)
_bench_end = _now()
_bench_mem_end = resource.getrusage(resource.RUSAGE_SELF).ru_maxrss
print(json.dumps({"duration_us": (_bench_end - _bench_start)//1000, "memory_bytes": _bench_mem_end*1024, "name": "main"}, indent=2))
