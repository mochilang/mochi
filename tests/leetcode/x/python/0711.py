import sys


def canonical(cells: list[tuple[int, int]]) -> tuple[tuple[int, int], ...]:
    transforms: list[list[tuple[int, int]]] = [[] for _ in range(8)]
    for x, y in cells:
        variants = [
            (x, y),
            (x, -y),
            (-x, y),
            (-x, -y),
            (y, x),
            (y, -x),
            (-y, x),
            (-y, -x),
        ]
        for i, point in enumerate(variants):
            transforms[i].append(point)
    normalized: list[tuple[tuple[int, int], ...]] = []
    for points in transforms:
        min_x = min(x for x, _ in points)
        min_y = min(y for _, y in points)
        normalized.append(tuple(sorted((x - min_x, y - min_y) for x, y in points)))
    return min(normalized)


def solve(grid: list[list[int]]) -> int:
    rows = len(grid)
    cols = len(grid[0])
    seen = [[False] * cols for _ in range(rows)]
    shapes: set[tuple[tuple[int, int], ...]] = set()

    for r in range(rows):
        for c in range(cols):
            if grid[r][c] == 0 or seen[r][c]:
                continue
            stack = [(r, c)]
            seen[r][c] = True
            cells: list[tuple[int, int]] = []
            while stack:
                x, y = stack.pop()
                cells.append((x - r, y - c))
                for dx, dy in ((1, 0), (-1, 0), (0, 1), (0, -1)):
                    nx = x + dx
                    ny = y + dy
                    if 0 <= nx < rows and 0 <= ny < cols and grid[nx][ny] == 1 and not seen[nx][ny]:
                        seen[nx][ny] = True
                        stack.append((nx, ny))
            shapes.add(canonical(cells))
    return len(shapes)


def main() -> None:
    data = sys.stdin.read().strip().split()
    if not data:
        return
    t = int(data[0])
    idx = 1
    out: list[str] = []
    for _ in range(t):
        rows = int(data[idx])
        cols = int(data[idx + 1])
        idx += 2
        grid: list[list[int]] = []
        for _ in range(rows):
            row = list(map(int, data[idx : idx + cols]))
            idx += cols
            grid.append(row)
        out.append(str(solve(grid)))
    sys.stdout.write("\n\n".join(out))


if __name__ == "__main__":
    main()
