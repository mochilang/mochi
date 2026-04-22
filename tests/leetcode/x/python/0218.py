import heapq
import sys
from collections import Counter


def solve(buildings):
    events = []
    for l, r, h in buildings:
        events.append((l, -h, r))
        events.append((r, 0, 0))
    events.sort()
    heap = [(0, float("inf"))]
    ans = []
    i = 0
    while i < len(events):
        x = events[i][0]
        while i < len(events) and events[i][0] == x:
            _, neg_h, r = events[i]
            if neg_h != 0:
                heapq.heappush(heap, (neg_h, r))
            i += 1
        while heap and heap[0][1] <= x:
            heapq.heappop(heap)
        height = -heap[0][0]
        if not ans or ans[-1][1] != height:
            ans.append([x, height])
    return ans


data = sys.stdin.read().split()
if data:
    idx = 0
    t = int(data[idx]); idx += 1
    out = []
    for _ in range(t):
        n = int(data[idx]); idx += 1
        buildings = []
        for _ in range(n):
            buildings.append([int(data[idx]), int(data[idx + 1]), int(data[idx + 2])])
            idx += 3
        ans = solve(buildings)
        lines = [str(len(ans))]
        lines.extend(f"{x} {y}" for x, y in ans)
        out.append("\n".join(lines))
    sys.stdout.write("\n\n".join(out))
