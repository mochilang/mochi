import sys

def solve(a):
    best = 0
    for i in range(len(a)):
        mn = a[i]
        for j in range(i, len(a)):
            mn = min(mn, a[j])
            best = max(best, mn * (j - i + 1))
    return best

data = sys.stdin.read().split()
if data:
    idx = 0
    t = int(data[idx]); idx += 1
    out = []
    for _ in range(t):
        n = int(data[idx]); idx += 1
        a = list(map(int, data[idx:idx+n])); idx += n
        out.append(str(solve(a)))
    sys.stdout.write("\n".join(out))
