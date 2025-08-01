# Code generated by Mochi transpiler.
# Version 0.10.42, generated on 2025-07-28 08:06 +0700
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
def countChange(amount):
    ways = []
    i = 0
    while i <= amount:
        ways = ways + [0]
        i = i + 1
    ways[0] = 1
    coins = [1, 5, 10, 25]
    idx = 0
    while idx < len(coins):
        coin = coins[idx]
        j = coin
        while j <= amount:
            ways[j] = ways[j] + ways[j - coin]
            j = j + 1
        idx = idx + 1
    return ways[amount]
amount = 10
print("amount, ways to make change: " + str(amount) + " " + str(countChange(amount)))
_bench_end = _now()
_bench_mem_end = resource.getrusage(resource.RUSAGE_SELF).ru_maxrss
print(json.dumps({"duration_us": (_bench_end - _bench_start)//1000, "memory_bytes": _bench_mem_end*1024, "name": "main"}, indent=2))
