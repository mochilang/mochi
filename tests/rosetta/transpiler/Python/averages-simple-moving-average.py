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

def indexOf(s, ch):
    i = 0
    while i < len(s):
        if s[i:i + 1] == ch:
            return i
        i = i + 1
    return -1
def fmt3(x):
    y = float(int(((x * 1000.0) + 0.5))) / 1000.0
    s = str(y)
    dot = s.find(".")
    if dot == 0 - 1:
        s = s + ".000"
    else:
        decs = len(s) - dot - 1
        if decs > 3:
            s = s[0:dot + 4]
        else:
            while decs < 3:
                s = s + "0"
                decs = decs + 1
    return s
def pad(s, width):
    out = s
    while len(out) < width:
        out = " " + out
    return out
def smaSeries(xs, period):
    res = []
    sum = 0.0
    i = 0
    while i < len(xs):
        sum = sum + xs[i]
        if i >= period:
            sum = sum - xs[i - period]
        denom = i + 1
        if denom > period:
            denom = period
        res = res + [sum / (float(denom))]
        i = i + 1
    return res
def main():
    _bench_mem_start = resource.getrusage(resource.RUSAGE_SELF).ru_maxrss
    _bench_start = _now()
    xs = [1.0, 2.0, 3.0, 4.0, 5.0, 5.0, 4.0, 3.0, 2.0, 1.0]
    sma3 = smaSeries(xs, 3)
    sma5 = smaSeries(xs, 5)
    print("x       sma3   sma5")
    i = 0
    while i < len(xs):
        line = pad(fmt3(xs[i]), 5) + "  " + pad(fmt3(sma3[i]), 5) + "  " + pad(fmt3(sma5[i]), 5)
        print(line)
        i = i + 1
    _bench_end = _now()
    _bench_mem_end = resource.getrusage(resource.RUSAGE_SELF).ru_maxrss
    print(json.dumps({"duration_us": (_bench_end - _bench_start)//1000, "memory_bytes": _bench_mem_end*1024, "name": "main"}, indent=2))
main()
