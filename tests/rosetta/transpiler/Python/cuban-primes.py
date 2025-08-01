# Code generated by Mochi transpiler.
# Version 0.10.39, generated on 2025-07-24 20:06 +0700
import sys
sys.set_int_max_str_digits(0)

def modPow(base, exp, m):
    result = 1 % m
    b = base % m
    e = exp
    while e > 0:
        if e % 2 == 1:
            result = (result * b) % m
        b = (b * b) % m
        e = int((e // 2))
    return result
def isPrime(n):
    if n < 2:
        return False
    for p in [2, 3, 5, 7, 11, 13, 17, 19, 23, 29]:
        if n % p == 0:
            return n == p
    d = n - 1
    s = 0
    while d % 2 == 0:
        d = d // 2
        s = s + 1
    for a in [2, 325, 9375, 28178, 450775, 9780504, 1795265022]:
        if a % n == 0:
            return True
        x = modPow(a, d, n)
        if x == 1 or x == n - 1:
            continue
        r = 1
        passed = False
        while r < s:
            x = (x * x) % n
            if x == n - 1:
                passed = True
                break
            r = r + 1
        if not passed:
            return False
    return True
def commatize(n):
    s = str(n)
    i = len(s) - 3
    while i > 0:
        s = "".join(s[0:i]) + "," + "".join(s[i:len(s)])
        i = i - 3
    return s
def pad(s, width):
    out = s
    while len(out) < width:
        out = " " + out
    return out
def join(xs, sep):
    res = ""
    i = 0
    while i < len(xs):
        if i > 0:
            res = res + sep
        res = res + xs[i]
        i = i + 1
    return res
def formatRow(row):
    padded = []
    i = 0
    while i < len(row):
        padded = padded + [pad(row[i], 9)]
        i = i + 1
    return "[" + join(padded, " ") + "]"
def main():
    cubans = []
    cube1 = 1
    count = 0
    cube100k = 0
    i = 1
    while True:
        j = i + 1
        cube2 = j * j * j
        diff = cube2 - cube1
        if isPrime(diff):
            if count < 200:
                cubans = cubans + [commatize(diff)]
            count = count + 1
            if count == 100000:
                cube100k = diff
                break
        cube1 = cube2
        i = i + 1
    print("The first 200 cuban primes are:-")
    row = 0
    while row < 20:
        slice = []
        k = 0
        while k < 10:
            slice = slice + [cubans[row * 10 + k]]
            k = k + 1
        print(formatRow(slice))
        row = row + 1
    print("\nThe 100,000th cuban prime is " + commatize(cube100k))
main()
