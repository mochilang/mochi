# Code generated by Mochi transpiler.
# Version 0.10.42, generated on 2025-07-28 00:50 +0700
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

def powf(base, exp):
    result = 1.0
    i = 0
    while i < exp:
        result = result * base
        i = i + 1
    return result
def nthRoot(x, n):
    low = 0.0
    high = x
    i = 0
    while i < 60:
        mid = (low + high) / 2.0
        if powf(mid, n) > x:
            high = mid
        else:
            low = mid
        i = i + 1
    return low
def main():
    _bench_mem_start = resource.getrusage(resource.RUSAGE_SELF).ru_maxrss
    _bench_start = _now()
    sum = 0.0
    sumRecip = 0.0
    prod = 1.0
    n = 1
    while n <= 10:
        f = float(n)
        sum = sum + f
        sumRecip = sumRecip + 1.0 / f
        prod = prod * f
        n = n + 1
    count = 10.0
    a = sum / count
    g = nthRoot(prod, 10)
    h = count / sumRecip
    print("A: " + str(a) + " G: " + str(g) + " H: " + str(h))
    print("A >= G >= H: " + str(a >= g and g >= h))
    _bench_end = _now()
    _bench_mem_end = resource.getrusage(resource.RUSAGE_SELF).ru_maxrss
    print(json.dumps({"duration_us": (_bench_end - _bench_start)//1000, "memory_bytes": _bench_mem_end*1024, "name": "main"}, indent=2))
main()
