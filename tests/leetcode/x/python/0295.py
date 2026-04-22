import heapq
import sys


class MedianFinder:
    def __init__(self) -> None:
        self.low: list[int] = []
        self.high: list[int] = []

    def addNum(self, num: int) -> None:
        if not self.low or num <= -self.low[0]:
            heapq.heappush(self.low, -num)
        else:
            heapq.heappush(self.high, num)
        if len(self.low) > len(self.high) + 1:
            heapq.heappush(self.high, -heapq.heappop(self.low))
        elif len(self.high) > len(self.low):
            heapq.heappush(self.low, -heapq.heappop(self.high))

    def findMedian(self) -> float:
        if len(self.low) > len(self.high):
            return float(-self.low[0])
        return (-self.low[0] + self.high[0]) / 2.0


def main() -> None:
    lines = [line.strip() for line in sys.stdin if line.strip()]
    if not lines:
        return
    t = int(lines[0])
    idx = 1
    blocks: list[str] = []
    for _ in range(t):
        m = int(lines[idx])
        idx += 1
        mf = MedianFinder()
        out: list[str] = []
        for _ in range(m):
            parts = lines[idx].split()
            idx += 1
            if parts[0] == "addNum":
                mf.addNum(int(parts[1]))
            else:
                out.append(f"{mf.findMedian():.1f}")
        blocks.append("\n".join(out))
    sys.stdout.write("\n\n".join(blocks))


if __name__ == "__main__":
    main()
