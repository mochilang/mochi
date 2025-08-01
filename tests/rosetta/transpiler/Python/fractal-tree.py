# Code generated by Mochi transpiler.
# Version 0.10.50, generated on 2025-07-30 21:42 +0700
import json
import os
import resource
import time

import sys
sys.set_int_max_str_digits(0)
import os
if os.path.dirname(__file__) in sys.path:
    sys.path.remove(os.path.dirname(__file__))


_now_seed = 0
_now_seeded = False
s = os.getenv("MOCHI_NOW_SEED")
if s and s != "":
    try:
        _now_seed = int(s)
        _now_seeded = True
    except Exception:
        pass

def _now():
    global _now_seed
    if _now_seeded:
        _now_seed = (_now_seed * 1664525 + 1013904223) % 2147483647
        return _now_seed
    return int(time.time_ns())

PI = 3.141592653589793
def _mod(x, m):
    return x - (float(int((x // m)))) * m
def _sin(x):
    y = _mod(x + PI, 2.0 * PI) - PI
    y2 = y * y
    y3 = y2 * y
    y5 = y3 * y2
    y7 = y5 * y2
    return y - y3 / 6.0 + y5 / 120.0 - y7 / 5040.0
def _cos(x):
    y = _mod(x + PI, 2.0 * PI) - PI
    y2 = y * y
    y4 = y2 * y2
    y6 = y4 * y2
    return 1.0 - y2 / 2.0 + y4 / 24.0 - y6 / 720.0
width = 80
height = 40
depth = 6
angle = 12.0
length = 12.0
frac = 0.8
def clearGrid():
    g = []
    y = 0
    while y < height:
        row = []
        x = 0
        while x < width:
            row = row + [" "]
            x = x + 1
        g = g + [row]
        y = y + 1
    return g
def drawPoint(g, x, y):
    if x >= 0 and x < width and y >= 0 and y < height:
        row = g[y]
        row[x] = "#"
        g[y] = row
def bresenham(x0, y0, x1, y1, g):
    dx = x1 - x0
    if dx < 0:
        dx = -dx
    dy = y1 - y0
    if dy < 0:
        dy = -dy
    sx = -1
    if x0 < x1:
        sx = 1
    sy = -1
    if y0 < y1:
        sy = 1
    err = dx - dy
    while True:
        drawPoint(g, x0, y0)
        if x0 == x1 and y0 == y1:
            break
        e2 = 2 * err
        if e2 > (-dy):
            err = err - dy
            x0 = x0 + sx
        if e2 < dx:
            err = err + dx
            y0 = y0 + sy
def ftree(g, x, y, dist, dir, d):
    rad = dir * PI / 180.0
    x2 = x + dist * _sin(rad)
    y2 = y - dist * _cos(rad)
    bresenham(int(x), int(y), int(x2), int(y2), g)
    if d > 0:
        ftree(g, x2, y2, dist * frac, dir - angle, d - 1)
        ftree(g, x2, y2, dist * frac, dir + angle, d - 1)
def render(g):
    out = ""
    y = 0
    while y < height:
        line = ""
        x = 0
        while x < width:
            line = line + g[y][x]
            x = x + 1
        out = out + line
        if y < height - 1:
            out = out + "\n"
        y = y + 1
    return out
def main():
    _bench_mem_start = resource.getrusage(resource.RUSAGE_SELF).ru_maxrss
    _bench_start = _now()
    grid = clearGrid()
    ftree(grid, float((width // 2)), float((height - 1)), length, 0.0, depth)
    print(render(grid))
    _bench_end = _now()
    _bench_mem_end = resource.getrusage(resource.RUSAGE_SELF).ru_maxrss
    print(json.dumps({"duration_us": (_bench_end - _bench_start)//1000, "memory_bytes": _bench_mem_end*1024, "name": "main"}, indent=2))
main()
