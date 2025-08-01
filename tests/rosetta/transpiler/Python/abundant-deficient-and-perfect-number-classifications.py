# Code generated by Mochi transpiler.
# Version 0.10.55, generated on 2025-08-02 17:26 +0700
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

def pfacSum(i):
    sum = 0
    p = 1
    while p <= (i // 2):
        if i % p == 0:
            sum = sum + p
        p = p + 1
    return sum
def main():
    _bench_mem_start = resource.getrusage(resource.RUSAGE_SELF).ru_maxrss
    _bench_start = _now()
    d = 0
    a = 0
    pnum = 0
    i = 1
    while i <= 20000:
        j = pfacSum(i)
        if j < i:
            d = d + 1
        if j == i:
            pnum = pnum + 1
        if j > i:
            a = a + 1
        i = i + 1
    print("There are " + str(d) + " deficient numbers between 1 and 20000")
    print("There are " + str(a) + " abundant numbers  between 1 and 20000")
    print("There are " + str(pnum) + " perfect numbers between 1 and 20000")
    _bench_end = _now()
    _bench_mem_end = resource.getrusage(resource.RUSAGE_SELF).ru_maxrss
    print(json.dumps({"duration_us": (_bench_end - _bench_start)//1000, "memory_bytes": _bench_mem_end*1024, "name": "main"}, indent=2))
main()
