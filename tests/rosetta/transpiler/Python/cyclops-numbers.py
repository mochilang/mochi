# Code generated by Mochi transpiler.
# Version 0.10.39, generated on 2025-07-24 20:06 +0700
import sys
sys.set_int_max_str_digits(0)

def digits(n):
    if n == 0:
        return [0]
    rev = []
    x = n
    while x > 0:
        rev = rev + [x % 10]
        x = int((x // 10))
    out = []
    i = len(rev) - 1
    while i >= 0:
        out = out + [rev[i]]
        i = i - 1
    return out
def commatize(n):
    s = str(n)
    out = ""
    i = len(s)
    while i > 3:
        out = "," + "".join(s[i - 3:i]) + out
        i = i - 3
    out = "".join(s[0:i]) + out
    return out
def isPrime(n):
    if n < 2:
        return False
    if n % 2 == 0:
        return n == 2
    if n % 3 == 0:
        return n == 3
    d = 5
    while d * d <= n:
        if n % d == 0:
            return False
        d = d + 2
        if n % d == 0:
            return False
        d = d + 4
    return True
def split(s, sep):
    parts = []
    cur = ""
    i = 0
    while i < len(s):
        if i + len(sep) <= len(s) and s[i:i + len(sep)] == sep:
            parts = parts + [cur]
            cur = ""
            i = i + len(sep)
        else:
            cur = cur + "".join(s[i:i + 1])
            i = i + 1
    parts = parts + [cur]
    return parts
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
def reverseStr(s):
    out = ""
    i = len(s) - 1
    while i >= 0:
        out = out + "".join(s[i:i + 1])
        i = i - 1
    return out
def pad(s, w):
    out = s
    while len(out) < w:
        out = " " + out
    return out
def findFirst(list):
    i = 0
    while i < len(list):
        if list[i] > 10000000:
            return [list[i], i]
        i = i + 1
    return [-1, -1]
def main():
    ranges = [[0, 0], [101, 909], [11011, 99099], [1110111, 9990999], [111101111, 119101111]]
    cyclops = []
    for r in ranges:
        start = r[0]
        end = r[1]
        numDigits = len(str(start))
        center = numDigits // 2
        i = start
        while i <= end:
            ds = digits(i)
            if ds[center] == 0:
                count = 0
                for d in ds:
                    if d == 0:
                        count = count + 1
                if count == 1:
                    cyclops = cyclops + [i]
            i = i + 1
    print("The first 50 cyclops numbers are:")
    idx = 0
    while idx < 50:
        print(pad(commatize(cyclops[idx]), 6) + " ")
        idx = idx + 1
        if idx % 10 == 0:
            print("\n")
    fi = findFirst(cyclops)
    print("\nFirst such number > 10 million is " + commatize(fi[0]) + " at zero-based index " + commatize(fi[1]))
    primes = []
    for n in cyclops:
        if isPrime(n):
            primes = primes + [n]
    print("\n\nThe first 50 prime cyclops numbers are:")
    idx = 0
    while idx < 50:
        print(pad(commatize(primes[idx]), 6) + " ")
        idx = idx + 1
        if idx % 10 == 0:
            print("\n")
    fp = findFirst(primes)
    print("\nFirst such number > 10 million is " + commatize(fp[0]) + " at zero-based index " + commatize(fp[1]))
    bpcyclops = []
    ppcyclops = []
    for p in primes:
        ps = str(p)
        splitp = split(ps, "0")
        noMiddle = parseIntStr(splitp[0] + splitp[1])
        if isPrime(noMiddle):
            bpcyclops = bpcyclops + [p]
        if ps == reverseStr(ps):
            ppcyclops = ppcyclops + [p]
    print("\n\nThe first 50 blind prime cyclops numbers are:")
    idx = 0
    while idx < 50:
        print(pad(commatize(bpcyclops[idx]), 6) + " ")
        idx = idx + 1
        if idx % 10 == 0:
            print("\n")
    fb = findFirst(bpcyclops)
    print("\nFirst such number > 10 million is " + commatize(fb[0]) + " at zero-based index " + commatize(fb[1]))
    print("\n\nThe first 50 palindromic prime cyclops numbers are:")
    idx = 0
    while idx < 50:
        print(pad(commatize(ppcyclops[idx]), 9) + " ")
        idx = idx + 1
        if idx % 8 == 0:
            print("\n")
    fpp = findFirst(ppcyclops)
    print("\n\nFirst such number > 10 million is " + commatize(fpp[0]) + " at zero-based index " + commatize(fpp[1]))
