import sys

MOD = 1_000_000_007


def solve(n: int, k: int) -> int:
    dp = [0] * (k + 1)
    dp[0] = 1
    for num in range(1, n + 1):
        ndp = [0] * (k + 1)
        window = 0
        for inv in range(k + 1):
            window = (window + dp[inv]) % MOD
            if inv >= num:
                window = (window - dp[inv - num]) % MOD
            ndp[inv] = window
        dp = ndp
    return dp[k]


def main() -> None:
    data = sys.stdin.read().strip().split()
    if not data:
        return
    idx = 0
    t = int(data[idx]); idx += 1
    out: list[str] = []
    for _ in range(t):
        n = int(data[idx]); k = int(data[idx + 1]); idx += 2
        out.append(str(solve(n, k)))
    sys.stdout.write("\n\n".join(out))


if __name__ == "__main__":
    main()
