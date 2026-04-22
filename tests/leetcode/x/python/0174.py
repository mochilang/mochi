import sys


def solve(dungeon):
    cols = len(dungeon[0])
    inf = 10**18
    dp = [inf] * (cols + 1)
    dp[cols - 1] = 1
    for i in range(len(dungeon) - 1, -1, -1):
        for j in range(cols - 1, -1, -1):
            need = min(dp[j], dp[j + 1]) - dungeon[i][j]
            dp[j] = 1 if need <= 1 else need
    return dp[0]


data = sys.stdin.read().split()
if data:
    idx = 0
    t = int(data[idx])
    idx += 1
    out = []
    for _ in range(t):
        rows = int(data[idx])
        cols = int(data[idx + 1])
        idx += 2
        dungeon = []
        for _ in range(rows):
            row = list(map(int, data[idx:idx + cols]))
            idx += cols
            dungeon.append(row)
        out.append(str(solve(dungeon)))
    sys.stdout.write("\n".join(out))
