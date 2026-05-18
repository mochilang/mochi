import json
import time


N = {{ .N }}

start = time.perf_counter()
seed = 42
win = 0
count = 0
for i in range(N):
    seed = (seed * 3877 + 29573) % 139968
    code = (seed * 4) // 139968
    win = ((win << 2) & 0xFF) | code
    if i >= 3:
        if win == 47 or win == 248:
            count += 1

duration_us = (time.perf_counter() - start) * 1e6

print(json.dumps({
    "duration_us": duration_us,
    "output": count,
}))
