# Code generated by Mochi transpiler.
# Version 0.10.50, generated on 2025-07-31 00:23 +0700
import sys
sys.set_int_max_str_digits(0)
import os
if os.path.dirname(__file__) in sys.path:
    sys.path.remove(os.path.dirname(__file__))

def hailstone(n):
    seq = []
    x = n
    seq = seq + [x]
    while x > 1:
        if x % 2 == 0:
            x = x // 2
        else:
            x = 3 * x + 1
        seq = seq + [x]
    return seq
def listString(xs):
    s = "["
    i = 0
    while i < len(xs):
        s = s + str(xs[i])
        if i < len(xs) - 1:
            s = s + " "
        i = i + 1
    s = s + "]"
    return s
def libMain():
    seq = hailstone(27)
    print("")
    print("Hailstone sequence for the number 27:")
    print("  has " + str(len(seq)) + " elements")
    print("  starts with " + listString(seq[0:4]))
    print("  ends with " + listString(seq[len(seq) - 4:len(seq)]))
    longest = 0
    length = 0
    i = 1
    while i < 100000:
        l = len(hailstone(i))
        if l > length:
            longest = i
            length = l
        i = i + 1
    print("")
    print(str(longest) + " has the longest Hailstone sequence, its length being " + str(length) + ".")
libMain()
