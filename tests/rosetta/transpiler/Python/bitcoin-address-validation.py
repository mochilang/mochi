# Code generated by Mochi transpiler.
# Version 0.10.42, generated on 2025-07-28 01:25 +0700
import hashlib
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
def indexOf(s, ch):
    i = 0
    while i < len(s):
        if s[i:i + 1] == ch:
            return i
        i = i + 1
    return -1
def set58(addr):
    tmpl = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
    a = []
    i = 0
    while i < 25:
        a = a + [0]
        i = i + 1
    idx = 0
    while idx < len(addr):
        ch = addr[idx:idx + 1]
        c = tmpl.find(ch)
        if c < 0:
            return []
        j = 24
        while j >= 0:
            c = c + 58 * a[j]
            a[j] = c % 256
            c = int((c // 256))
            j = j - 1
        if c > 0:
            return []
        idx = idx + 1
    return a
def doubleSHA256(bs):
    first = list(hashlib.sha256(bytes(bs)).digest())
    return list(hashlib.sha256(bytes(first)).digest())
def computeChecksum(a):
    hash = doubleSHA256(a[0:21])
    return hash[0:4]
def validA58(addr):
    a = set58(addr)
    if len(a) != 25:
        return False
    if a[0] != 0:
        return False
    sum = computeChecksum(a)
    i = 0
    while i < 4:
        if a[21 + i] != sum[i]:
            return False
        i = i + 1
    return True
print(str(validA58("1AGNa15ZQXAZUgFiqJ3i7Z2DPU2J6hW62i")))
print(str(validA58("17NdbrSGoUotzeGCcMMCqnFkEvLymoou9j")))
_bench_end = _now()
_bench_mem_end = resource.getrusage(resource.RUSAGE_SELF).ru_maxrss
print(json.dumps({"duration_us": (_bench_end - _bench_start)//1000, "memory_bytes": _bench_mem_end*1024, "name": "main"}, indent=2))
