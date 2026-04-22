import collections
import sys


def shortest_distance(grid: list[list[int]]) -> int:
    rows, cols = len(grid), len(grid[0])
    dist = [[0] * cols for _ in range(rows)]
    reach = [[0] * cols for _ in range(rows)]
    buildings = 0

    for sr in range(rows):
        for sc in range(cols):
            if grid[sr][sc] != 1:
                continue
            buildings += 1
            q = collections.deque([(sr, sc, 0)])
            seen = [[False] * cols for _ in range(rows)]
            seen[sr][sc] = True
            while q:
                r, c, d = q.popleft()
                for dr, dc in ((1, 0), (-1, 0), (0, 1), (0, -1)):
                    nr, nc = r + dr, c + dc
                    if 0 <= nr < rows and 0 <= nc < cols and not seen[nr][nc]:
                        seen[nr][nc] = True
                        if grid[nr][nc] == 0:
                            dist[nr][nc] += d + 1
                            reach[nr][nc] += 1
                            q.append((nr, nc, d + 1))

    ans = min((dist[r][c] for r in range(rows) for c in range(cols) if grid[r][c] == 0 and reach[r][c] == buildings), default=None)
    return -1 if ans is None else ans


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
        out.append(str(shortest_distance(grid)))
    sys.stdout.write("\n".join(out))


if __name__ == "__main__":
    main()
