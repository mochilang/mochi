# Code generated by Mochi transpiler.
# Version 0.10.41, generated on 2025-07-26 17:30 +0700
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

def ord(ch):
    if ch == "5":
        return 53
    if ch == "T":
        return 84
    if ch == " ":
        return 32
    if ch == "é":
        return 233
    if ch == "🐺":
        return 128058
    return 0
def hex(n):
    digits = "0123456789abcdef"
    if n == 0:
        return "0x0"
    m = n
    out = ""
    while m > 0:
        d = m % 16
        out = "".join(digits[d:d + 1]) + out
        m = m // 16
    return "0x" + out
def quote(s):
    return "'" + s + "'"
def analyze(s):
    le = len(s)
    print("Analyzing " + quote(s) + " which has a length of " + str(le) + ":")
    if le > 1:
        i = 1
        while i < le:
            cur = s[i:i + 1]
            prev = s[i - 1:i]
            if cur != prev:
                print("  Not all characters in the string are the same.")
                print("  " + quote(cur) + " (" + hex(ord(cur)) + ") is different at position " + str(i + 1) + ".")
                print("")
                return
            i = i + 1
    print("  All characters in the string are the same.")
    print("")
def main():
    _bench_mem_start = resource.getrusage(resource.RUSAGE_SELF).ru_maxrss
    _bench_start = _now()
    strings = ["", "   ", "2", "333", ".55", "tttTTT", "4444 444k", "pépé", "🐶🐶🐺🐶", "🎄🎄🎄🎄"]
    i = 0
    while i < len(strings):
        analyze(strings[i])
        i = i + 1
    _bench_end = _now()
    _bench_mem_end = resource.getrusage(resource.RUSAGE_SELF).ru_maxrss
    print(json.dumps({"duration_us": (_bench_end - _bench_start)//1000, "memory_bytes": _bench_mem_end*1024, "name": "main"}, indent=2))
main()
