# Code generated by Mochi transpiler.
# Version 0.10.50, generated on 2025-07-31 01:05 +0700
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

g2lMap = {"♜": "R", "♞": "N", "♝": "B", "♛": "Q", "♚": "K", "♖": "R", "♘": "N", "♗": "B", "♕": "Q", "♔": "K"}
names = {"R": "rook", "N": "knight", "B": "bishop", "Q": "queen", "K": "king"}
ntable = {"01": 0, "02": 1, "03": 2, "04": 3, "12": 4, "13": 5, "14": 6, "23": 7, "24": 8, "34": 9}
def indexOf(s, sub):
    i = 0
    while i <= len(s) - len(sub):
        if s[i:i + len(sub)] == sub:
            return i
        i = i + 1
    return -1
def removeChar(s, ch):
    res = ""
    i = 0
    while i < len(s):
        c = s[i:i + 1]
        if c != ch:
            res = res + c
        i = i + 1
    return res
def g2l(pieces):
    res = ""
    i = 0
    while i < len(pieces):
        ch = pieces[i:i + 1]
        res = res + g2lMap.get(ch)
        i = i + 1
    return res
def countChar(s, ch):
    c = 0
    i = 0
    while i < len(s):
        if s[i:i + 1] == ch:
            c = c + 1
        i = i + 1
    return c
def spid(pieces):
    pieces = g2l(pieces)
    if len(pieces) != 8:
        return -1
    for one in ["K", "Q"]:
        if countChar(pieces, one) != 1:
            return -1
    for two in ["R", "N", "B"]:
        if countChar(pieces, two) != 2:
            return -1
    r1 = pieces.find("R")
    r2 = pieces[r1 + 1:len(pieces)].find("R") + r1 + 1
    k = pieces.find("K")
    if k < r1 or k > r2:
        return -1
    b1 = pieces.find("B")
    b2 = pieces[b1 + 1:len(pieces)].find("B") + b1 + 1
    if (b2 - b1) % 2 == 0:
        return -1
    piecesN = removeChar(removeChar(pieces, "Q"), "B")
    n1 = piecesN.find("N")
    n2 = piecesN[n1 + 1:len(piecesN)].find("N") + n1 + 1
    N = ntable.get(str(n1) + str(n2))
    piecesQ = removeChar(pieces, "B")
    Q = piecesQ.find("Q")
    D = "0246".find(str(b1))
    L = "1357".find(str(b2))
    if D == (0 - 1):
        D = "0246".find(str(b2))
        L = "1357".find(str(b1))
    return 96 * N + 16 * Q + 4 * D + L
def main():
    _bench_mem_start = resource.getrusage(resource.RUSAGE_SELF).ru_maxrss
    _bench_start = _now()
    for pieces in ["♕♘♖♗♗♘♔♖", "♖♘♗♕♔♗♘♖", "♖♕♘♗♗♔♖♘", "♖♘♕♗♗♔♖♘"]:
        print(pieces + " or " + g2l(pieces) + " has SP-ID of " + str(spid(pieces)))
    _bench_end = _now()
    _bench_mem_end = resource.getrusage(resource.RUSAGE_SELF).ru_maxrss
    print(json.dumps({"duration_us": (_bench_end - _bench_start)//1000, "memory_bytes": _bench_mem_end*1024, "name": "main"}, indent=2))
main()
