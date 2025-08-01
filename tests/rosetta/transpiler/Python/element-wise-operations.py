# Code generated by Mochi transpiler.
# Version 0.10.41, generated on 2025-07-27 10:59 +0700
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

def pow10(n):
    r = 1.0
    i = 0
    while i < n:
        r = r * 10.0
        i = i + 1
    return r
def powf(base, exp):
    if exp == 0.5:
        guess = base
        i = 0
        while i < 20:
            guess = (guess + base // guess) / 2.0
            i = i + 1
        return guess
    result = 1.0
    n = int(exp)
    i = 0
    while i < n:
        result = result * base
        i = i + 1
    return result
def formatFloat(f, prec):
    scale = pow10(prec)
    scaled = (f * scale) + 0.5
    n = (int(scaled))
    digits = str(n)
    while len(digits) <= prec:
        digits = "0" + digits
    intPart = digits[0:len(digits) - prec]
    fracPart = digits[len(digits) - prec:len(digits)]
    return intPart + "." + fracPart
def padLeft(s, w):
    res = ""
    n = w - len(s)
    while n > 0:
        res = res + " "
        n = n - 1
    return res + s
def rowString(row):
    s = "["
    i = 0
    while i < len(row):
        s = s + padLeft(formatFloat(row[i], 3), 6)
        if i < len(row) - 1:
            s = s + " "
        i = i + 1
    return s + "] "
def printMatrix(heading, m):
    print(heading)
    i = 0
    while i < len(m):
        print(rowString(m[i]))
        i = i + 1
def elementWiseMM(m1, m2, f):
    z = []
    r = 0
    while r < len(m1):
        row = []
        c = 0
        while c < len(m1[r]):
            row = row + [f(m1[r][c], m2[r][c])]
            c = c + 1
        z = z + [row]
        r = r + 1
    return z
def elementWiseMS(m, s, f):
    z = []
    r = 0
    while r < len(m):
        row = []
        c = 0
        while c < len(m[r]):
            row = row + [f(m[r][c], s)]
            c = c + 1
        z = z + [row]
        r = r + 1
    return z
def add(a, b):
    return a + b
def sub(a, b):
    return a - b
def mul(a, b):
    return a * b
def div(a, b):
    return a // b
def exp(a, b):
    return powf(a, b)
def main():
    _bench_mem_start = resource.getrusage(resource.RUSAGE_SELF).ru_maxrss
    _bench_start = _now()
    m1 = [[3.0, 1.0, 4.0], [1.0, 5.0, 9.0]]
    m2 = [[2.0, 7.0, 1.0], [8.0, 2.0, 8.0]]
    printMatrix("m1:", m1)
    printMatrix("m2:", m2)
    print("")
    printMatrix("m1 + m2:", elementWiseMM(m1, m2, add))
    printMatrix("m1 - m2:", elementWiseMM(m1, m2, sub))
    printMatrix("m1 * m2:", elementWiseMM(m1, m2, mul))
    printMatrix("m1 / m2:", elementWiseMM(m1, m2, div))
    printMatrix("m1 ^ m2:", elementWiseMM(m1, m2, exp))
    print("")
    s = 0.5
    print("s: " + str(s))
    printMatrix("m1 + s:", elementWiseMS(m1, s, add))
    printMatrix("m1 - s:", elementWiseMS(m1, s, sub))
    printMatrix("m1 * s:", elementWiseMS(m1, s, mul))
    printMatrix("m1 / s:", elementWiseMS(m1, s, div))
    printMatrix("m1 ^ s:", elementWiseMS(m1, s, exp))
    _bench_end = _now()
    _bench_mem_end = resource.getrusage(resource.RUSAGE_SELF).ru_maxrss
    print(json.dumps({"duration_us": (_bench_end - _bench_start)//1000, "memory_bytes": _bench_mem_end*1024, "name": "main"}, indent=2))
main()
