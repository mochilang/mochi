import heapq
import sys
from collections import defaultdict


class DualHeap:
    def __init__(self, k: int) -> None:
        self.small: list[int] = []
        self.large: list[int] = []
        self.delayed: dict[int, int] = defaultdict(int)
        self.k = k
        self.small_size = 0
        self.large_size = 0

    def prune_small(self) -> None:
        while self.small and self.delayed[-self.small[0]]:
            x = -heapq.heappop(self.small)
            self.delayed[x] -= 1

    def prune_large(self) -> None:
        while self.large and self.delayed[self.large[0]]:
            x = heapq.heappop(self.large)
            self.delayed[x] -= 1

    def rebalance(self) -> None:
        if self.small_size > self.large_size + 1:
            heapq.heappush(self.large, -heapq.heappop(self.small))
            self.small_size -= 1
            self.large_size += 1
            self.prune_small()
        elif self.small_size < self.large_size:
            heapq.heappush(self.small, -heapq.heappop(self.large))
            self.small_size += 1
            self.large_size -= 1
            self.prune_large()

    def add(self, x: int) -> None:
        if not self.small or x <= -self.small[0]:
            heapq.heappush(self.small, -x)
            self.small_size += 1
        else:
            heapq.heappush(self.large, x)
            self.large_size += 1
        self.rebalance()

    def remove(self, x: int) -> None:
        self.delayed[x] += 1
        if x <= -self.small[0]:
            self.small_size -= 1
            if x == -self.small[0]:
                self.prune_small()
        else:
            self.large_size -= 1
            if self.large and x == self.large[0]:
                self.prune_large()
        self.rebalance()

    def median2(self) -> int:
        self.prune_small()
        self.prune_large()
        if self.k % 2 == 1:
            return -self.small[0] * 2
        return -self.small[0] + self.large[0]


def fmt_val(v2: int) -> str:
    if v2 % 2 == 0:
        return str(v2 // 2)
    sign = "-" if v2 < 0 else ""
    a = abs(v2)
    return f"{sign}{a // 2}.5"


def solve(nums: list[int], k: int) -> str:
    dh = DualHeap(k)
    for x in nums[:k]:
        dh.add(x)
    out = [fmt_val(dh.median2())]
    for i in range(k, len(nums)):
        dh.add(nums[i])
        dh.remove(nums[i - k])
        out.append(fmt_val(dh.median2()))
    return "[" + ",".join(out) + "]"


def main() -> None:
    data = sys.stdin.read().strip().split()
    if not data:
        return
    idx = 0
    t = int(data[idx]); idx += 1
    ans: list[str] = []
    for _ in range(t):
        n = int(data[idx]); idx += 1
        k = int(data[idx]); idx += 1
        nums = list(map(int, data[idx:idx + n]))
        idx += n
        ans.append(solve(nums, k))
    sys.stdout.write("\n\n".join(ans))


if __name__ == "__main__":
    main()
