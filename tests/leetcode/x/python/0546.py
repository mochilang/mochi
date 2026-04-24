import sys
from functools import lru_cache


def solve(boxes: list[int]) -> int:
    @lru_cache(maxsize=None)
    def dp(l: int, r: int, k: int) -> int:
        if l > r:
            return 0
        while l < r and boxes[r] == boxes[r - 1]:
            r -= 1
            k += 1
        best = dp(l, r - 1, 0) + (k + 1) * (k + 1)
        for i in range(l, r):
            if boxes[i] == boxes[r]:
                best = max(best, dp(l, i, k + 1) + dp(i + 1, r - 1, 0))
        return best

    return dp(0, len(boxes) - 1, 0)


def main() -> None:
    data = sys.stdin.read().strip().split()
    if not data:
        return
    idx = 0
    t = int(data[idx]); idx += 1
    out: list[str] = []
    for _ in range(t):
        n = int(data[idx]); idx += 1
        boxes = list(map(int, data[idx:idx + n]))
        idx += n
        out.append(str(solve(boxes)))
    sys.stdout.write("\n\n".join(out))


if __name__ == "__main__":
    main()
