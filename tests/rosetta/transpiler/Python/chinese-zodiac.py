# Code generated by Mochi transpiler.
# Version 0.10.42, generated on 2025-07-28 08:06 +0700
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

_bench_mem_start = resource.getrusage(resource.RUSAGE_SELF).ru_maxrss
_bench_start = _now()
animal = ["Rat", "Ox", "Tiger", "Rabbit", "Dragon", "Snake", "Horse", "Goat", "Monkey", "Rooster", "Dog", "Pig"]
yinYang = ["Yang", "Yin"]
element = ["Wood", "Fire", "Earth", "Metal", "Water"]
stemChArr = ["甲", "乙", "丙", "丁", "戊", "己", "庚", "辛", "壬", "癸"]
branchChArr = ["子", "丑", "寅", "卯", "辰", "巳", "午", "未", "申", "酉", "戌", "亥"]
@dataclass
class Info:
    animal: str
    yinYang: str
    element: str
    stemBranch: str
    cycle: int
def cz(yr, animal, yinYang, element, sc, bc):
    y = yr - 4
    stem = y % 10
    branch = y % 12
    sb = sc[stem] + bc[branch]
    return Info(animal=str(animal[branch]), yinYang=str(yinYang[stem % 2]), element=str(element[int((stem // 2))]), stemBranch=sb, cycle=y % 60 + 1)
for yr in [1935, 1938, 1968, 1972, 1976]:
    r = cz(yr, animal, yinYang, element, stemChArr, branchChArr)
    print(str(yr) + ": " + r.element + " " + r.animal + ", " + r.yinYang + ", Cycle year " + str(r.cycle) + " " + r.stemBranch)
_bench_end = _now()
_bench_mem_end = resource.getrusage(resource.RUSAGE_SELF).ru_maxrss
print(json.dumps({"duration_us": (_bench_end - _bench_start)//1000, "memory_bytes": _bench_mem_end*1024, "name": "main"}, indent=2))
