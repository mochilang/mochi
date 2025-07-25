# Code generated by Mochi transpiler.
# Version 0.10.41, generated on 2025-07-26 17:30 +0700
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

def listToStringInts(xs):
    s = "["
    i = 0
    while i < len(xs):
        s = s + str(int(xs[i]))
        if i < len(xs) - 1:
            s = s + " "
        i = i + 1
    return s + "]"
def deconv(g, f):
    h = []
    n = 0
    hn = len(g) - len(f) + 1
    while n < hn:
        v = g[n]
        lower = 0
        if n >= len(f):
            lower = n - len(f) + 1
        i = lower
        while i < n:
            v = v - h[i] * f[n - i]
            i = i + 1
        v = v / f[0]
        h = h + [v]
        n = n + 1
    return h
def main():
    _bench_mem_start = resource.getrusage(resource.RUSAGE_SELF).ru_maxrss
    _bench_start = _now()
    h = [-8.0, -9.0, -3.0, -1.0, -6.0, 7.0]
    f = [-3.0, -6.0, -1.0, 8.0, -6.0, 3.0, -1.0, -9.0, -9.0, 3.0, -2.0, 5.0, 2.0, -2.0, -7.0, -1.0]
    g = [24.0, 75.0, 71.0, -34.0, 3.0, 22.0, -45.0, 23.0, 245.0, 25.0, 52.0, 25.0, -67.0, -96.0, 96.0, 31.0, 55.0, 36.0, 29.0, -43.0, -7.0]
    print(listToStringInts(h))
    print(listToStringInts(deconv(g, f)))
    print(listToStringInts(f))
    print(listToStringInts(deconv(g, h)))
    _bench_end = _now()
    _bench_mem_end = resource.getrusage(resource.RUSAGE_SELF).ru_maxrss
    print(json.dumps({"duration_us": (_bench_end - _bench_start)//1000, "memory_bytes": _bench_mem_end*1024, "name": "main"}, indent=2))
main()
