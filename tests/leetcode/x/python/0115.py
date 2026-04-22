import sys

def solve(s, t):
    dp = [0] * (len(t) + 1)
    dp[0] = 1
    for ch in s:
        for j in range(len(t), 0, -1):
            if ch == t[j - 1]:
                dp[j] += dp[j - 1]
    return dp[-1]

lines = sys.stdin.read().splitlines()
if lines:
    tc = int(lines[0])
    out = []
    for i in range(tc):
        out.append(str(solve(lines[1 + 2 * i], lines[2 + 2 * i])))
    sys.stdout.write("\n".join(out))
