import sys
from math import inf


def solve(bulbs: list[int], k: int) -> int:
    n = len(bulbs)
    days = [0] * n
    for day, bulb in enumerate(bulbs, 1):
        days[bulb - 1] = day
    ans = inf
    left, right = 0, k + 1
    while right < n:
        valid = True
        for i in range(left + 1, right):
            if days[i] < max(days[left], days[right]):
                left = i
                right = i + k + 1
                valid = False
                break
        if valid:
            ans = min(ans, max(days[left], days[right]))
            left = right
            right = left + k + 1
    return -1 if ans == inf else ans


def main() -> None:
    data = sys.stdin.read().strip().split()
    if not data:
        return
    t = int(data[0])
    idx = 1
    out: list[str] = []
    for _ in range(t):
        n = int(data[idx])
        k = int(data[idx + 1])
        idx += 2
        bulbs = list(map(int, data[idx:idx + n]))
        idx += n
        out.append(str(solve(bulbs, k)))
    sys.stdout.write("\n\n".join(out))


if __name__ == "__main__":
    main()
