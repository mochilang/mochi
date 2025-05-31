import json
import time

def sum_loop(n):
    total = 0
    for i in range(1, n + 1):
        total += i
    return total

n = {{ .N }}
repeat = 1000
last = 0

start = time.perf_counter()
for _ in range(repeat):
    last = sum_loop(n)
duration = (time.perf_counter() - start) * 1000

print(json.dumps({
    "duration_ms": duration,
    "output": last,
}))
