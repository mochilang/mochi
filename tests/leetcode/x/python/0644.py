import sys


def feasible(nums: list[int], k: int, mid: float) -> bool:
    total = 0.0
    prev = 0.0
    min_prev = 0.0
    for i, x in enumerate(nums):
        total += x - mid
        if i >= k:
            prev += nums[i - k] - mid
            min_prev = min(min_prev, prev)
        if i + 1 >= k and total - min_prev >= 0:
            return True
    return False


def solve(nums: list[int], k: int) -> float:
    lo = float(min(nums))
    hi = float(max(nums))
    for _ in range(60):
        mid = (lo + hi) / 2.0
        if feasible(nums, k, mid):
            lo = mid
        else:
            hi = mid
    return lo


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
        nums = list(map(int, data[idx:idx + n]))
        idx += n
        out.append(f"{solve(nums, k):.5f}")
    sys.stdout.write("\n\n".join(out))


if __name__ == "__main__":
    main()
