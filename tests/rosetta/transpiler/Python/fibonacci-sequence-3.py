# Code generated by Mochi transpiler.
# Version 0.10.42, generated on 2025-07-27 17:35 +0700
import sys
sys.set_int_max_str_digits(0)

def _lambda0():
    tmp = a + b
    a = b
    b = tmp
    return a
def fibNumber():
    a = 0
    b = 1
    return _lambda0
def fibSequence(n):
    f = fibNumber()
    r = 0
    i = 0
    while i < n:
        r = f()
        i = i + 1
    return r
