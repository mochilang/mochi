# Code generated by Mochi transpiler.
# Version 0.10.42, generated on 2025-07-28 07:52 +0700
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
width = 81
height = 5
lines = []
for i in range(0, height):
    row = ""
    j = 0
    while j < width:
        row = row + "*"
        j = j + 1
    lines = lines + [row]
def setChar(s, idx, ch):
    return "".join(s[0:idx]) + ch + "".join(s[idx + 1:len(s)])
stack = [{"start": 0, "len": width, "index": 1}]
while len(stack) > 0:
    frame = stack[len(stack) - 1]
    stack = stack[:len(stack) - 1]
    start = frame.get("start")
    lenSeg = frame.get("len")
    index = frame.get("index")
    seg = int((lenSeg / 3))
    if seg == 0:
        continue
    i = index
    while i < height:
        j = start + seg
        while j < start + 2 * seg:
            lines[i] = setChar(lines[i], j, " ")
            j = j + 1
        i = i + 1
    stack = stack + [{"start": start, "len": seg, "index": index + 1}]
    stack = stack + [{"start": start + seg * 2, "len": seg, "index": index + 1}]
for line in lines:
    print(line)
_bench_end = _now()
_bench_mem_end = resource.getrusage(resource.RUSAGE_SELF).ru_maxrss
print(json.dumps({"duration_us": (_bench_end - _bench_start)//1000, "memory_bytes": _bench_mem_end*1024, "name": "main"}, indent=2))
