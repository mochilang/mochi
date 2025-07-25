# Code generated by Mochi transpiler.
# Version 0.10.40, generated on 2025-07-25 19:02 +0700
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

def indexOfStr(h, n):
    hlen = len(h)
    nlen = len(n)
    if nlen == 0:
        return 0
    i = 0
    while i <= hlen - nlen:
        if h[i:i + nlen] == n:
            return i
        i = i + 1
    return -1
def stringSearchSingle(h, n):
    return indexOfStr(h, n)
def stringSearch(h, n):
    result = []
    start = 0
    hlen = len(h)
    nlen = len(n)
    while start < hlen:
        idx = indexOfStr(h[start:hlen], n)
        if idx >= 0:
            result = result + [start + idx]
            start = start + idx + nlen
        else:
            break
    return result
def display(nums):
    s = "["
    i = 0
    while i < len(nums):
        if i > 0:
            s = s + ", "
        s = s + str(nums[i])
        i = i + 1
    s = s + "]"
    return s
def main():
    _bench_mem_start = resource.getrusage(resource.RUSAGE_SELF).ru_maxrss
    _bench_start = _now()
    texts = ["GCTAGCTCTACGAGTCTA", "GGCTATAATGCGTA", "there would have been a time for such a word", "needle need noodle needle", "DKnuthusesandprogramsanimaginarycomputertheMIXanditsassociatedmachinecodeandassemblylanguages", "Nearby farms grew an acre of alfalfa on the dairy's behalf, with bales of that alfalfa exchanged for milk."]
    patterns = ["TCTA", "TAATAAA", "word", "needle", "and", "alfalfa"]
    i = 0
    while i < len(texts):
        print("text" + str(i + 1) + " = " + texts[i])
        i = i + 1
    print("")
    j = 0
    while j < len(texts):
        idxs = stringSearch(texts[j], patterns[j])
        print("Found \"" + patterns[j] + "\" in 'text" + str(j + 1) + "' at indexes " + display(idxs))
        j = j + 1
    _bench_end = _now()
    _bench_mem_end = resource.getrusage(resource.RUSAGE_SELF).ru_maxrss
    print(json.dumps({"duration_us": (_bench_end - _bench_start)//1000, "memory_bytes": _bench_mem_end*1024, "name": "main"}, indent=2))
main()
