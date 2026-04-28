import sys
from collections import Counter


class Excel:
    def __init__(self, height: int, width: str):
        self.cells: dict[tuple[int, int], tuple[int, Counter[tuple[int, int]] | int]] = {}

    def set(self, row: int, col: str, val: int) -> None:
        self.cells[(row, ord(col) - 65)] = (0, val)

    def get(self, row: int, col: str) -> int:
        key = (row, ord(col) - 65)
        if key not in self.cells:
            return 0
        kind, payload = self.cells[key]
        if kind == 0:
            return int(payload)
        refs = payload
        assert isinstance(refs, Counter)
        return sum(self.get(r, chr(c + 65)) * count for (r, c), count in refs.items())

    def sum(self, row: int, col: str, numbers: list[str]) -> int:
        refs: Counter[tuple[int, int]] = Counter()
        for token in numbers:
            if ":" not in token:
                refs[self.parse_cell(token)] += 1
                continue
            left, right = token.split(":")
            r1, c1 = self.parse_cell(left)
            r2, c2 = self.parse_cell(right)
            for c in range(c1, c2 + 1):
                for r in range(r1, r2 + 1):
                    refs[(r, c)] += 1
        self.cells[(row, ord(col) - 65)] = (1, refs)
        return self.get(row, col)

    @staticmethod
    def parse_cell(token: str) -> tuple[int, int]:
        return int(token[1:]), ord(token[0]) - 65


def main() -> None:
    data = sys.stdin.read().strip().split()
    if not data:
        return
    t = int(data[0])
    idx = 1
    parts: list[str] = []
    for _ in range(t):
        q = int(data[idx])
        idx += 1
        excel: Excel | None = None
        out: list[str] = []
        for _ in range(q):
            op = data[idx]
            idx += 1
            if op == "C":
                excel = Excel(int(data[idx]), data[idx + 1])
                idx += 2
                out.append("null")
            elif op == "S":
                assert excel is not None
                excel.set(int(data[idx]), data[idx + 1], int(data[idx + 2]))
                idx += 3
                out.append("null")
            elif op == "G":
                assert excel is not None
                out.append(str(excel.get(int(data[idx]), data[idx + 1])))
                idx += 2
            else:
                assert excel is not None
                row = int(data[idx])
                col = data[idx + 1]
                k = int(data[idx + 2])
                refs = data[idx + 3:idx + 3 + k]
                idx += 3 + k
                out.append(str(excel.sum(row, col, refs)))
        parts.append("[" + ",".join(out) + "]")
    sys.stdout.write("\n\n".join(parts))


if __name__ == "__main__":
    main()
