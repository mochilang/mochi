import heapq
import sys


def solve(k: int, w: int, profits: list[int], capital: list[int]) -> int:
    projects = sorted(zip(capital, profits))
    available: list[int] = []
    i = 0
    cur = w
    for _ in range(k):
        while i < len(projects) and projects[i][0] <= cur:
            heapq.heappush(available, -projects[i][1])
            i += 1
        if not available:
            break
        cur -= heapq.heappop(available)
    return cur


def main() -> None:
    data = sys.stdin.read().strip().split()
    if not data:
        return
    idx = 0
    t = int(data[idx])
    idx += 1
    out: list[str] = []
    for _ in range(t):
        n = int(data[idx])
        k = int(data[idx + 1])
        w = int(data[idx + 2])
        idx += 3
        profits = list(map(int, data[idx:idx + n]))
        idx += n
        capital = list(map(int, data[idx:idx + n]))
        idx += n
        out.append(str(solve(k, w, profits, capital)))
    sys.stdout.write("\n\n".join(out))


if __name__ == "__main__":
    main()
