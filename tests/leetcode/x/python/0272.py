import sys
from bisect import bisect_left


def solve(values, target, k):
    i = bisect_left(values, target)
    left = i - 1
    right = i
    ans = []
    while len(ans) < k:
        if left < 0:
            ans.append(values[right])
            right += 1
        elif right >= len(values):
            ans.append(values[left])
            left -= 1
        elif abs(values[left] - target) <= abs(values[right] - target):
            ans.append(values[left])
            left -= 1
        else:
            ans.append(values[right])
            right += 1
    return ans


data = sys.stdin.read().split()
if data:
    idx = 0
    t = int(data[idx]); idx += 1
    out = []
    for _ in range(t):
        n = int(data[idx]); idx += 1
        values = list(map(int, data[idx:idx + n])); idx += n
        target = float(data[idx]); idx += 1
        k = int(data[idx]); idx += 1
        ans = solve(values, target, k)
        out.append("\n".join([str(len(ans))] + [str(x) for x in ans]))
    sys.stdout.write("\n\n".join(out))
