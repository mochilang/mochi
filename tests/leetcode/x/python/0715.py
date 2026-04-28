import bisect
import sys


class RangeModule:
    def __init__(self) -> None:
        self.intervals: list[list[int]] = []

    def add_range(self, left: int, right: int) -> None:
        merged: list[list[int]] = []
        i = 0
        while i < len(self.intervals) and self.intervals[i][1] < left:
            merged.append(self.intervals[i])
            i += 1
        new_left, new_right = left, right
        while i < len(self.intervals) and self.intervals[i][0] <= new_right:
            new_left = min(new_left, self.intervals[i][0])
            new_right = max(new_right, self.intervals[i][1])
            i += 1
        merged.append([new_left, new_right])
        merged.extend(self.intervals[i:])
        self.intervals = merged

    def query_range(self, left: int, right: int) -> bool:
        lo, hi = 0, len(self.intervals) - 1
        while lo <= hi:
            mid = (lo + hi) // 2
            start, end = self.intervals[mid]
            if start <= left:
                if right <= end:
                    return True
                lo = mid + 1
            else:
                hi = mid - 1
        return False

    def remove_range(self, left: int, right: int) -> None:
        kept: list[list[int]] = []
        for start, end in self.intervals:
            if end <= left or right <= start:
                kept.append([start, end])
            else:
                if start < left:
                    kept.append([start, left])
                if right < end:
                    kept.append([right, end])
        self.intervals = kept


def main() -> None:
    data = sys.stdin.read().strip().split()
    if not data:
        return
    t = int(data[0])
    idx = 1
    cases: list[str] = []
    for _ in range(t):
        ops = int(data[idx])
        idx += 1
        rm: RangeModule | None = None
        out: list[str] = []
        for _ in range(ops):
            op = data[idx]
            idx += 1
            if op == "C":
                rm = RangeModule()
                out.append("null")
            elif op == "A":
                assert rm is not None
                left = int(data[idx])
                right = int(data[idx + 1])
                idx += 2
                rm.add_range(left, right)
                out.append("null")
            elif op == "R":
                assert rm is not None
                left = int(data[idx])
                right = int(data[idx + 1])
                idx += 2
                rm.remove_range(left, right)
                out.append("null")
            else:
                assert rm is not None
                left = int(data[idx])
                right = int(data[idx + 1])
                idx += 2
                out.append("true" if rm.query_range(left, right) else "false")
        cases.append("[" + ",".join(out) + "]")
    sys.stdout.write("\n\n".join(cases))


if __name__ == "__main__":
    main()
