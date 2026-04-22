import sys

def solve(triangle):
    dp = triangle[-1][:]
    for i in range(len(triangle) - 2, -1, -1):
        for j in range(i + 1):
            dp[j] = triangle[i][j] + min(dp[j], dp[j + 1])
    return dp[0]

data = sys.stdin.read().split()
if data:
    idx = 0
    t = int(data[idx]); idx += 1
    out = []
    for _ in range(t):
        rows = int(data[idx]); idx += 1
        tri = []
        for r in range(1, rows + 1):
            tri.append(list(map(int, data[idx:idx+r])))
            idx += r
        out.append(str(solve(tri)))
    sys.stdout.write("\n".join(out))
