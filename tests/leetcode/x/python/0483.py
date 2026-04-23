import sys


def value(base: int, m: int, limit: int) -> int:
    total = 1
    cur = 1
    for _ in range(m):
        if cur > limit // base:
            return limit + 1
        cur *= base
        if total > limit - cur:
            return limit + 1
        total += cur
    return total


def solve(n: int) -> str:
    max_m = n.bit_length() - 1
    for m in range(max_m, 1, -1):
        lo, hi = 2, int(round(n ** (1.0 / m))) + 1
        while lo <= hi:
            mid = (lo + hi) // 2
            s = value(mid, m, n)
            if s == n:
                return str(mid)
            if s < n:
                lo = mid + 1
            else:
                hi = mid - 1
    return str(n - 1)


def main() -> None:
    data = sys.stdin.read().strip().split()
    if not data:
        return
    t = int(data[0])
    ans = [solve(int(data[i])) for i in range(1, t + 1)]
    sys.stdout.write("\n\n".join(ans))


if __name__ == "__main__":
    main()
