# Code generated by Mochi transpiler.
# Version 0.10.41, generated on 2025-07-26 17:30 +0700
from __future__ import annotations
from dataclasses import dataclass
from typing import List, Dict
import dataclasses
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

@dataclass
class DivResult:
    q: int
    ok: bool

def divCheck(x, y):
    if y == 0:
        return DivResult(q=0, ok=False)
    return DivResult(q=x // y, ok=True)
def printResult(r):
    print(str(r.q) + " " + str(r.ok))
def main():
    _bench_mem_start = resource.getrusage(resource.RUSAGE_SELF).ru_maxrss
    _bench_start = _now()
    printResult(divCheck(3, 2))
    printResult(divCheck(3, 0))
    _bench_end = _now()
    _bench_mem_end = resource.getrusage(resource.RUSAGE_SELF).ru_maxrss
    print(json.dumps({"duration_us": (_bench_end - _bench_start)//1000, "memory_bytes": _bench_mem_end*1024, "name": "main"}, indent=2))
main()
