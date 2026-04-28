import sys


def solve(flights: list[list[int]], days: list[list[int]]) -> int:
    n = len(flights)
    weeks = len(days[0])
    neg = -10**18
    dp = [neg] * n
    dp[0] = 0
    for week in range(weeks):
        ndp = [neg] * n
        for city in range(n):
            if dp[city] == neg:
                continue
            for nxt in range(n):
                if city == nxt or flights[city][nxt]:
                    ndp[nxt] = max(ndp[nxt], dp[city] + days[nxt][week])
        dp = ndp
    return max(dp)


def main() -> None:
    data = sys.stdin.read().strip().split()
    if not data:
        return
    idx = 0
    t = int(data[idx]); idx += 1
    out: list[str] = []
    for _ in range(t):
        n = int(data[idx]); idx += 1
        w = int(data[idx]); idx += 1
        flights = []
        for _ in range(n):
            row = list(map(int, data[idx:idx + n]))
            idx += n
            flights.append(row)
        days = []
        for _ in range(n):
            row = list(map(int, data[idx:idx + w]))
            idx += w
            days.append(row)
        out.append(str(solve(flights, days)))
    sys.stdout.write("\n\n".join(out))


if __name__ == "__main__":
    main()
