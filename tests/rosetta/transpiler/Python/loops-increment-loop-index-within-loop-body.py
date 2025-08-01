# Code generated by Mochi transpiler.
# Version 0.10.50, generated on 2025-07-30 21:42 +0700
import json
import os
import resource
import time

import sys
sys.set_int_max_str_digits(0)
import os
if os.path.dirname(__file__) in sys.path:
    sys.path.remove(os.path.dirname(__file__))


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

def isPrime(n):
    if n < 2:
        return False
    if n % 2 == 0:
        return n == 2
    if n % 3 == 0:
        return n == 3
    d = 5
    while d * d <= n:
        if n % d == 0:
            return False
        d = d + 2
        if n % d == 0:
            return False
        d = d + 4
    return True
def commatize(n):
    s = str(n)
    i = len(s) - 3
    while i >= 1:
        s = "".join(s[0:i]) + "," + "".join(s[i:len(s)])
        i = i - 3
    return s
def padLeft(s, w):
    out = s
    while len(out) < w:
        out = " " + out
    return out
def padRight(s, w):
    out = s
    while len(out) < w:
        out = out + " "
    return out
limit = 42
def main():
    _bench_mem_start = resource.getrusage(resource.RUSAGE_SELF).ru_maxrss
    _bench_start = _now()
    i = limit
    n = 0
    while n < limit:
        if isPrime(i):
            n = n + 1
            nStr = padRight(str(n), 2)
            pStr = padLeft(commatize(i), 19)
            print("n = " + nStr + "  " + pStr)
            i = i + i - 1
        i = i + 1
    _bench_end = _now()
    _bench_mem_end = resource.getrusage(resource.RUSAGE_SELF).ru_maxrss
    print(json.dumps({"duration_us": (_bench_end - _bench_start)//1000, "memory_bytes": _bench_mem_end*1024, "name": "main"}, indent=2))
main()
