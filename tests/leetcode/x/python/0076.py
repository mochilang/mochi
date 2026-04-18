import sys

def min_window(s, t):
    need = [0] * 128
    missing = len(t)
    for ch in t:
        need[ord(ch)] += 1
    left = 0
    best_start = 0
    best_len = len(s) + 1
    for right, ch in enumerate(s):
        c = ord(ch)
        if need[c] > 0:
            missing -= 1
        need[c] -= 1
        while missing == 0:
            if right - left + 1 < best_len:
                best_start = left
                best_len = right - left + 1
            lc = ord(s[left])
            need[lc] += 1
            if need[lc] > 0:
                missing += 1
            left += 1
    return "" if best_len > len(s) else s[best_start:best_start + best_len]

lines = sys.stdin.read().splitlines()
if lines:
    t = int(lines[0])
    out = [min_window(lines[1 + 2*i], lines[2 + 2*i]) for i in range(t)]
    sys.stdout.write('\n'.join(out))
