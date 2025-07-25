# Code generated by Mochi transpiler.
# Version 0.10.41, generated on 2025-07-26 19:54 +0700
import sys
sys.set_int_max_str_digits(0)

def padRight(s, w):
    r = s
    while len(r) < w:
        r = r + " "
    return r
def linearCombo(c):
    out = ""
    i = 0
    while i < len(c):
        n = c[i]
        if n != 0:
            op = ""
            if n < 0 and len(out) == 0:
                op = "-"
            else:
                if n < 0:
                    op = " - "
                else:
                    if n > 0 and len(out) == 0:
                        op = ""
                    else:
                        op = " + "
            av = n
            if av < 0:
                av = -av
            coeff = str(av) + "*"
            if av == 1:
                coeff = ""
            out = out + op + coeff + "e(" + str(i + 1) + ")"
        i = i + 1
    if len(out) == 0:
        return "0"
    return out
def main():
    combos = [[1, 2, 3], [0, 1, 2, 3], [1, 0, 3, 4], [1, 2, 0], [0, 0, 0], [0], [1, 1, 1], [-1, -1, -1], [-1, -2, 0, -3], [-1]]
    idx = 0
    while idx < len(combos):
        c = combos[idx]
        t = "["
        j = 0
        while j < len(c):
            t = t + str(c[j])
            if j < len(c) - 1:
                t = t + ", "
            j = j + 1
        t = t + "]"
        lc = linearCombo(c)
        print(padRight(t, 15) + "  ->  " + lc)
        idx = idx + 1
main()
