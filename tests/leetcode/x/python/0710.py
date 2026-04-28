import sys


class Solution:
    def __init__(self, n: int, blacklist: list[int]) -> None:
        blocked = set(blacklist)
        self.bound = n - len(blacklist)
        self.remap: dict[int, int] = {}
        tail = [value for value in range(self.bound, n) if value not in blocked]
        idx = 0
        for value in blacklist:
            if value < self.bound:
                self.remap[value] = tail[idx]
                idx += 1
        self.state = 1

    def pick(self) -> int:
        self.state = (self.state * 1664525 + 1013904223) & 0xFFFFFFFF
        value = self.state % self.bound
        return self.remap.get(value, value)


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
        out: list[str] = []
        solution: Solution | None = None
        for _ in range(ops):
            op = data[idx]
            idx += 1
            if op == "C":
                n = int(data[idx])
                b = int(data[idx + 1])
                idx += 2
                blacklist = list(map(int, data[idx : idx + b]))
                idx += b
                solution = Solution(n, blacklist)
                out.append("null")
            else:
                assert solution is not None
                out.append(str(solution.pick()))
        cases.append("[" + ",".join(out) + "]")
    sys.stdout.write("\n\n".join(cases))


if __name__ == "__main__":
    main()
