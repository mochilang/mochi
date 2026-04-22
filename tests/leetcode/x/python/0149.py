import math
import sys
from collections import defaultdict


def max_points(points):
    n = len(points)
    if n <= 2:
        return n
    best = 0
    for i in range(n):
        slopes = defaultdict(int)
        local = 0
        x1, y1 = points[i]
        for j in range(i + 1, n):
            x2, y2 = points[j]
            dx = x2 - x1
            dy = y2 - y1
            g = math.gcd(abs(dx), abs(dy))
            dx //= g
            dy //= g
            if dx < 0:
                dx = -dx
                dy = -dy
            elif dx == 0:
                dy = 1
            elif dy == 0:
                dx = 1
            slopes[(dy, dx)] += 1
            local = max(local, slopes[(dy, dx)])
        best = max(best, local + 1)
    return best


lines = sys.stdin.read().splitlines()
if lines:
    tc = int(lines[0])
    idx = 1
    out = []
    for _ in range(tc):
        n = int(lines[idx])
        idx += 1
        pts = []
        for _ in range(n):
            x, y = map(int, lines[idx].split())
            idx += 1
            pts.append((x, y))
        out.append(str(max_points(pts)))
    sys.stdout.write("\n\n".join(out))
