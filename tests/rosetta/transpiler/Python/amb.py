# Code generated by Mochi transpiler.
# Version 0.10.42, generated on 2025-07-28 10:14 +0700
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

def amb(wordsets, res, idx):
    if idx == len(wordsets):
        return True
    prev = ""
    if idx > 0:
        prev = res[idx - 1]
    i = 0
    while i < len(wordsets[idx]):
        w = wordsets[idx][i]
        if idx == 0 or prev[len(prev) - 1:len(prev)] == w[0:1]:
            res[idx] = w
            if amb(wordsets, res, idx + 1):
                return True
        i = i + 1
    return False
def main():
    _bench_mem_start = resource.getrusage(resource.RUSAGE_SELF).ru_maxrss
    _bench_start = _now()
    wordset = [["the", "that", "a"], ["frog", "elephant", "thing"], ["walked", "treaded", "grows"], ["slowly", "quickly"]]
    res = []
    i = 0
    while i < len(wordset):
        res = res + [""]
        i = i + 1
    if amb(wordset, res, 0):
        out = "[" + res[0]
        j = 1
        while j < len(res):
            out = out + " " + res[j]
            j = j + 1
        out = out + "]"
        print(out)
    else:
        print("No amb found")
    _bench_end = _now()
    _bench_mem_end = resource.getrusage(resource.RUSAGE_SELF).ru_maxrss
    print(json.dumps({"duration_us": (_bench_end - _bench_start)//1000, "memory_bytes": _bench_mem_end*1024, "name": "main"}, indent=2))
main()
