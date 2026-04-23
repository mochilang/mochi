import sys


def better(a: str, b: str) -> str:
    if len(a) != len(b):
        return a if len(a) < len(b) else b
    return a if a < b else b


def repeat_len(s: str) -> int:
    pos = (s + s).find(s, 1)
    if 0 <= pos < len(s) and len(s) % pos == 0:
        return pos
    return len(s)


def encode(s: str) -> str:
    n = len(s)
    dp = [[""] * n for _ in range(n)]
    for length in range(1, n + 1):
        for i in range(n - length + 1):
            j = i + length - 1
            sub = s[i : j + 1]
            best = sub
            for k in range(i, j):
                best = better(best, dp[i][k] + dp[k + 1][j])
            part = repeat_len(sub)
            if part < length:
                best = better(best, f"{length // part}[{dp[i][i + part - 1]}]")
            dp[i][j] = best
    return dp[0][n - 1]


def main() -> None:
    data = sys.stdin.read().strip().split()
    if not data:
        return
    t = int(data[0])
    ans = [encode(data[i]) for i in range(1, t + 1)]
    sys.stdout.write("\n\n".join(ans))


if __name__ == "__main__":
    main()
