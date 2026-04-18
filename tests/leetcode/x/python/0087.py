import sys
from functools import lru_cache

def is_scramble(s1, s2):
    n = len(s1)
    if n != len(s2):
        return False

    @lru_cache(None)
    def dfs(i1, i2, length):
        a = s1[i1:i1 + length]
        b = s2[i2:i2 + length]
        if a == b:
            return True
        if sorted(a) != sorted(b):
            return False
        for k in range(1, length):
            if dfs(i1, i2, k) and dfs(i1 + k, i2 + k, length - k):
                return True
            if dfs(i1, i2 + length - k, k) and dfs(i1 + k, i2, length - k):
                return True
        return False

    return dfs(0, 0, n)

lines = sys.stdin.read().splitlines()
if lines:
    t = int(lines[0])
    out = []
    for i in range(t):
        out.append("true" if is_scramble(lines[1 + 2 * i], lines[2 + 2 * i]) else "false")
    sys.stdout.write("\n".join(out))
