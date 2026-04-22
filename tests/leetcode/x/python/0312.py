import sys


def max_coins(nums: list[int]) -> int:
    vals = [1] + nums + [1]
    n = len(vals)
    dp = [[0] * n for _ in range(n)]
    for length in range(2, n):
        for left in range(0, n - length):
            right = left + length
            best = 0
            for k in range(left + 1, right):
                best = max(best, dp[left][k] + dp[k][right] + vals[left] * vals[k] * vals[right])
            dp[left][right] = best
    return dp[0][n - 1]


def main() -> None:
    data = sys.stdin.read().strip().split()
    if not data:
        return
    t = int(data[0])
    idx = 1
    out: list[str] = []
    for tc in range(t):
        n = int(data[idx])
        idx += 1
        nums = list(map(int, data[idx:idx + n]))
        idx += n
        if tc:
            out.append("")
        out.append(str(max_coins(nums)))
    sys.stdout.write("\n".join(out))


if __name__ == "__main__":
    main()
