import heapq
import sys

def trap_rain_water(h: list[list[int]]) -> int:
    m = len(h)
    n = len(h[0]) if m else 0
    if m < 3 or n < 3:
        return 0
    seen = [[False] * n for _ in range(m)]
    heap: list[tuple[int, int, int]] = []
    for r in range(m):
        for c in (0, n - 1):
            if not seen[r][c]:
                seen[r][c] = True
                heapq.heappush(heap, (h[r][c], r, c))
    for c in range(n):
        for r in (0, m - 1):
            if not seen[r][c]:
                seen[r][c] = True
                heapq.heappush(heap, (h[r][c], r, c))
    water = 0
    while heap:
        wall, r, c = heapq.heappop(heap)
        for dr, dc in ((1, 0), (-1, 0), (0, 1), (0, -1)):
            nr, nc = r + dr, c + dc
            if nr < 0 or nr >= m or nc < 0 or nc >= n or seen[nr][nc]:
                continue
            seen[nr][nc] = True
            nh = h[nr][nc]
            if nh < wall:
                water += wall - nh
            heapq.heappush(heap, (max(wall, nh), nr, nc))
    return water

def main() -> None:
    data = sys.stdin.read().strip().split()
    if not data:
        return
    idx = 0
    t = int(data[idx]); idx += 1
    ans: list[str] = []
    for _ in range(t):
        m = int(data[idx]); n = int(data[idx + 1]); idx += 2
        grid = []
        for _ in range(m):
            grid.append([int(x) for x in data[idx:idx+n]])
            idx += n
        ans.append(str(trap_rain_water(grid)))
    sys.stdout.write('\n\n'.join(ans))

if __name__ == '__main__':
    main()
