# Code generated by Mochi transpiler.
# Version 0.10.41, generated on 2025-07-26 19:01 +0700
import sys
sys.set_int_max_str_digits(0)

def isInt(s):
    if len(s) == 0:
        return False
    for ch in s:
        if ch < "0" or ch > "9":
            return False
    return True
def main():
    print("Are these strings integers?")
    v = "1"
    b = False
    if isInt(v):
        b = True
    print("  " + v + " -> " + str(b))
    i = "one"
    print("  " + i + " -> " + str(isInt(i)))
main()
