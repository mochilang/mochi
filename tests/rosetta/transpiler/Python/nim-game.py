# Code generated by Mochi transpiler.
# Version 0.10.50, generated on 2025-07-30 21:42 +0700
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

def parseIntStr(str):
    i = 0
    neg = False
    if len(str) > 0 and str[0:1] == "-":
        neg = True
        i = 1
    n = 0
    digits = {"0": 0, "1": 1, "2": 2, "3": 3, "4": 4, "5": 5, "6": 6, "7": 7, "8": 8, "9": 9}
    while i < len(str):
        n = n * 10 + digits[str[i:i + 1]]
        i = i + 1
    if neg:
        n = -n
    return n
def showTokens(tokens):
    print("Tokens remaining " + str(tokens))
def main():
    _bench_mem_start = resource.getrusage(resource.RUSAGE_SELF).ru_maxrss
    _bench_start = _now()
    tokens = 12
    done = False
    while not done:
        showTokens(tokens)
        print("")
        print("How many tokens 1, 2 or 3?")
        line = input()
        t = 0
        if len(line) > 0:
            t = parseIntStr(line)
        if t < 1 or t > 3:
            print("\nMust be a number between 1 and 3, try again.\n")
        else:
            ct = 4 - t
            s = "s"
            if ct == 1:
                s = ""
            print("  Computer takes " + str(ct) + " token" + s + "\n\n")
            tokens = tokens - 4
        if tokens == 0:
            showTokens(0)
            print("  Computer wins!")
            done = True
    _bench_end = _now()
    _bench_mem_end = resource.getrusage(resource.RUSAGE_SELF).ru_maxrss
    print(json.dumps({"duration_us": (_bench_end - _bench_start)//1000, "memory_bytes": _bench_mem_end*1024, "name": "main"}, indent=2))
main()
