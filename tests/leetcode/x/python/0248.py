import sys

PAIRS = [("0", "0"), ("1", "1"), ("6", "9"), ("8", "8"), ("9", "6")]


def build(n, m):
    if n == 0:
        return [""]
    if n == 1:
        return ["0", "1", "8"]
    res = []
    for mid in build(n - 2, m):
        for a, b in PAIRS:
            if n == m and a == "0":
                continue
            res.append(a + mid + b)
    return res


def count_range(low, high):
    ans = 0
    for length in range(len(low), len(high) + 1):
        for s in build(length, length):
            if length == len(low) and s < low:
                continue
            if length == len(high) and s > high:
                continue
            ans += 1
    return ans


lines = sys.stdin.read().splitlines()
if lines:
    t = int(lines[0].strip())
    out = []
    idx = 1
    for _ in range(t):
        low = lines[idx].strip()
        high = lines[idx + 1].strip()
        idx += 2
        out.append(str(count_range(low, high)))
    sys.stdout.write("\n".join(out))
