import heapq
import sys


def smallest_range(nums: list[list[int]]) -> tuple[int, int]:
    heap: list[tuple[int, int, int]] = []
    current_max = -10**18
    for i, row in enumerate(nums):
        value = row[0]
        heapq.heappush(heap, (value, i, 0))
        current_max = max(current_max, value)
    best_left, best_right = heap[0][0], current_max
    while True:
        current_min, i, j = heapq.heappop(heap)
        if current_max - current_min < best_right - best_left or (
            current_max - current_min == best_right - best_left and current_min < best_left
        ):
            best_left, best_right = current_min, current_max
        if j + 1 == len(nums[i]):
            return best_left, best_right
        nxt = nums[i][j + 1]
        heapq.heappush(heap, (nxt, i, j + 1))
        current_max = max(current_max, nxt)


def main() -> None:
    data = sys.stdin.read().strip().split()
    if not data:
        return
    t = int(data[0])
    idx = 1
    out: list[str] = []
    for _ in range(t):
        k = int(data[idx])
        idx += 1
        nums: list[list[int]] = []
        for _ in range(k):
            n = int(data[idx])
            idx += 1
            row = list(map(int, data[idx:idx + n]))
            idx += n
            nums.append(row)
        left, right = smallest_range(nums)
        out.append(f"[{left},{right}]")
    sys.stdout.write("\n\n".join(out))


if __name__ == "__main__":
    main()
