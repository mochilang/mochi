import heapq
import sys


def solve(maze: list[list[int]], ball: tuple[int, int], hole: tuple[int, int]) -> str:
    m, n = len(maze), len(maze[0])
    inf = 10**18
    dist = [[inf] * n for _ in range(m)]
    path = [[""] * n for _ in range(m)]
    pq: list[tuple[int, str, int, int]] = [(0, "", ball[0], ball[1])]
    dist[ball[0]][ball[1]] = 0
    dirs = ((1, 0, "d"), (0, -1, "l"), (0, 1, "r"), (-1, 0, "u"))
    while pq:
        d, p, r, c = heapq.heappop(pq)
        if (r, c) == hole:
            return p
        if d != dist[r][c] or p != path[r][c]:
            continue
        for dr, dc, ch in dirs:
            nr, nc, nd = r, c, d
            while 0 <= nr + dr < m and 0 <= nc + dc < n and maze[nr + dr][nc + dc] == 0:
                nr += dr
                nc += dc
                nd += 1
                if (nr, nc) == hole:
                    break
            if (nr, nc) == (r, c):
                continue
            np = p + ch
            if nd < dist[nr][nc] or (nd == dist[nr][nc] and (path[nr][nc] == "" or np < path[nr][nc])):
                dist[nr][nc] = nd
                path[nr][nc] = np
                heapq.heappush(pq, (nd, np, nr, nc))
    return "impossible"


def main() -> None:
    data = sys.stdin.read().strip().split()
    if not data:
        return
    idx = 0
    t = int(data[idx]); idx += 1
    out: list[str] = []
    for _ in range(t):
        m = int(data[idx]); idx += 1
        n = int(data[idx]); idx += 1
        maze = []
        for _ in range(m):
            row = list(map(int, data[idx:idx + n]))
            idx += n
            maze.append(row)
        ball = (int(data[idx]), int(data[idx + 1])); idx += 2
        hole = (int(data[idx]), int(data[idx + 1])); idx += 2
        out.append(solve(maze, ball, hole))
    sys.stdout.write("\n\n".join(out))


if __name__ == "__main__":
    main()
