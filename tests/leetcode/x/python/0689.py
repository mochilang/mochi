import sys


def solve(nums: list[int], k: int) -> list[int]:
    n = len(nums)
    m = n - k + 1
    windows = [sum(nums[i:i + k]) for i in range(m)]

    left = [0] * m
    best = 0
    for i in range(m):
        if windows[i] > windows[best]:
            best = i
        left[i] = best

    right = [0] * m
    best = m - 1
    for i in range(m - 1, -1, -1):
        if windows[i] >= windows[best]:
            best = i
        right[i] = best

    answer = [0, 0, 0]
    best_sum = -1
    for mid in range(k, m - k):
        a = left[mid - k]
        c = right[mid + k]
        total = windows[a] + windows[mid] + windows[c]
        candidate = [a, mid, c]
        if total > best_sum or (total == best_sum and candidate < answer):
            best_sum = total
            answer = candidate
    return answer


def fmt(arr: list[int]) -> str:
    return "[" + ",".join(map(str, arr)) + "]"


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
        out.append(fmt(solve(nums, k)))
    sys.stdout.write("\n\n".join(out))


if __name__ == "__main__":
    main()
