import sys
from collections import deque


def bfs(forest: list[list[int]], sr: int, sc: int, tr: int, tc: int) -> int:
    if (sr, sc) == (tr, tc):
        return 0
    m, n = len(forest), len(forest[0])
    q = deque([(sr, sc, 0)])
    seen = {(sr, sc)}
    while q:
        r, c, d = q.popleft()
        for dr, dc in ((1, 0), (-1, 0), (0, 1), (0, -1)):
            nr, nc = r + dr, c + dc
            if 0 <= nr < m and 0 <= nc < n and forest[nr][nc] != 0 and (nr, nc) not in seen:
                if (nr, nc) == (tr, tc):
                    return d + 1
                seen.add((nr, nc))
                q.append((nr, nc, d + 1))
    return -1


def solve(forest: list[list[int]]) -> int:
    trees = sorted((forest[i][j], i, j) for i in range(len(forest)) for j in range(len(forest[0])) if forest[i][j] > 1)
    sr = sc = 0
    total = 0
    for _, tr, tc in trees:
        dist = bfs(forest, sr, sc, tr, tc)
        if dist < 0:
            return -1
        total += dist
        sr, sc = tr, tc
    return total


def main() -> None:
    data = sys.stdin.read().strip().split()
    if not data:
        return
    t = int(data[0])
    idx = 1
    out: list[str] = []
    for _ in range(t):
        m = int(data[idx])
        n = int(data[idx + 1])
        idx += 2
        forest = []
        for _ in range(m):
            row = list(map(int, data[idx:idx + n]))
            idx += n
            forest.append(row)
        out.append(str(solve(forest)))
    sys.stdout.write("\n\n".join(out))


if __name__ == "__main__":
    main()
