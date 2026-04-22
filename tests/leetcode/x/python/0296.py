import sys


def min_total_distance(grid: list[list[int]]) -> int:
    rows: list[int] = []
    cols: list[int] = []
    for i, row in enumerate(grid):
        for val in row:
            if val:
                rows.append(i)
    for j in range(len(grid[0])):
        for i in range(len(grid)):
            if grid[i][j]:
                cols.append(j)
    mr = rows[len(rows) // 2]
    mc = cols[len(cols) // 2]
    return sum(abs(r - mr) for r in rows) + sum(abs(c - mc) for c in cols)


def main() -> None:
    data = sys.stdin.read().strip().split()
    if not data:
        return
    t = int(data[0])
    idx = 1
    out: list[str] = []
    for tc in range(t):
        r = int(data[idx])
        c = int(data[idx + 1])
        idx += 2
        grid = []
        for _ in range(r):
            row = list(map(int, data[idx:idx + c]))
            idx += c
            grid.append(row)
        if tc:
            out.append("")
        out.append(str(min_total_distance(grid)))
    sys.stdout.write("\n".join(out))


if __name__ == "__main__":
    main()
