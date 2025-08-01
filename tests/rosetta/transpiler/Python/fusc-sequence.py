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

def fuscVal(n):
    a = 1
    b = 0
    x = n
    while x > 0:
        if x % 2 == 0:
            x = x // 2
            a = a + b
        else:
            x = (x - 1) // 2
            b = a + b
    if n == 0:
        return 0
    return b
def firstFusc(n):
    arr = []
    i = 0
    while i < n:
        arr = arr + [fuscVal(i)]
        i = i + 1
    return arr
def commatize(n):
    s = str(n)
    neg = False
    if n < 0:
        neg = True
        s = s[1:len(s)]
    i = len(s) - 3
    while i >= 1:
        s = "".join(s[0:i]) + "," + "".join(s[i:len(s)])
        i = i - 3
    if neg:
        return "-" + s
    return s
def padLeft(s, w):
    out = s
    while len(out) < w:
        out = " " + out
    return out
def main():
    _bench_mem_start = resource.getrusage(resource.RUSAGE_SELF).ru_maxrss
    _bench_start = _now()
    print("The first 61 fusc numbers are:")
    print(str(firstFusc(61)))
    print("\nThe fusc numbers whose length > any previous fusc number length are:")
    idxs = [0, 37, 1173, 35499, 699051, 19573419]
    i = 0
    while i < len(idxs):
        idx = idxs[i]
        val = fuscVal(idx)
        numStr = padLeft(commatize(val), 7)
        idxStr = padLeft(commatize(idx), 10)
        print(numStr + " (index " + idxStr + ")")
        i = i + 1
    _bench_end = _now()
    _bench_mem_end = resource.getrusage(resource.RUSAGE_SELF).ru_maxrss
    print(json.dumps({"duration_us": (_bench_end - _bench_start)//1000, "memory_bytes": _bench_mem_end*1024, "name": "main"}, indent=2))
main()
