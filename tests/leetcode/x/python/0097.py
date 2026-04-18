import sys

def solve(s1, s2, s3):
    m, n = len(s1), len(s2)
    if m + n != len(s3):
        return False
    dp = [[False] * (n + 1) for _ in range(m + 1)]
    dp[0][0] = True
    for i in range(m + 1):
        for j in range(n + 1):
            if i > 0 and dp[i - 1][j] and s1[i - 1] == s3[i + j - 1]:
                dp[i][j] = True
            if j > 0 and dp[i][j - 1] and s2[j - 1] == s3[i + j - 1]:
                dp[i][j] = True
    return dp[m][n]

lines = sys.stdin.read().splitlines()
if lines:
    t = int(lines[0])
    out = []
    for i in range(t):
        out.append("true" if solve(lines[1 + 3*i], lines[2 + 3*i], lines[3 + 3*i]) else "false")
    sys.stdout.write("\n".join(out))
