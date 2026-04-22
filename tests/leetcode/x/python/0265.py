import sys


def solve(costs):
    if not costs:
        return 0
    prev = costs[0][:]
    for row in costs[1:]:
        min1 = float("inf")
        min2 = float("inf")
        idx1 = -1
        for i, v in enumerate(prev):
            if v < min1:
                min2 = min1
                min1 = v
                idx1 = i
            elif v < min2:
                min2 = v
        cur = []
        for i, c in enumerate(row):
            cur.append(c + (min2 if i == idx1 else min1))
        prev = cur
    return min(prev)


data = sys.stdin.read().split()
if data:
    idx = 0
    t = int(data[idx]); idx += 1
    out = []
    for _ in range(t):
        n = int(data[idx]); idx += 1
        k = int(data[idx]); idx += 1
        costs = []
        for _ in range(n):
            costs.append(list(map(int, data[idx:idx + k])))
            idx += k
        out.append(str(solve(costs)))
    sys.stdout.write("\n".join(out))
