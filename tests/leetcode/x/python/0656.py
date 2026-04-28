import sys
from math import inf


def solve(coins: list[int], max_jump: int) -> list[int]:
    n = len(coins)
    dp = [inf] * n
    path: list[list[int] | None] = [None] * n
    if coins[-1] == -1:
        return []
    dp[-1] = coins[-1]
    path[-1] = [n]
    for i in range(n - 2, -1, -1):
        if coins[i] == -1:
            continue
        best_cost = inf
        best_path: list[int] | None = None
        for j in range(i + 1, min(n, i + max_jump + 1)):
            if dp[j] == inf or path[j] is None:
                continue
            cand_cost = coins[i] + dp[j]
            cand_path = [i + 1] + path[j]
            if cand_cost < best_cost or (cand_cost == best_cost and (best_path is None or cand_path < best_path)):
                best_cost = cand_cost
                best_path = cand_path
        dp[i] = best_cost
        path[i] = best_path
    return path[0] or []


def fmt(path: list[int]) -> str:
    return "[" + ",".join(map(str, path)) + "]"


def main() -> None:
    data = sys.stdin.read().strip().split()
    if not data:
        return
    t = int(data[0])
    idx = 1
    out: list[str] = []
    for _ in range(t):
        n = int(data[idx])
        max_jump = int(data[idx + 1])
        idx += 2
        coins = list(map(int, data[idx:idx + n]))
        idx += n
        out.append(fmt(solve(coins, max_jump)))
    sys.stdout.write("\n\n".join(out))


if __name__ == "__main__":
    main()
