import json
import time


def escape_count(cx, cy, max_iter):
    zr = 0.0
    zi = 0.0
    n = 0
    while n < max_iter:
        r2 = zr * zr
        i2 = zi * zi
        if r2 + i2 > 4.0:
            return n
        nzi = 2.0 * zr * zi + cy
        nzr = (r2 - i2) + cx
        zr = nzr
        zi = nzi
        n += 1
    return max_iter


side = {{ .N }}
max_iter = 50
side_f = float(side)
total = 0

start = time.perf_counter()
for row in range(side):
    cy = row / side_f * 2.0 - 1.0
    for col in range(side):
        cx = col / side_f * 3.0 - 2.0
        total += escape_count(cx, cy, max_iter)
duration = (time.perf_counter() - start) * 1e6

print(json.dumps({
    "duration_us": duration,
    "output": total,
}))
