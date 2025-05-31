import json
import time

def fact(n):
    if n == 0:
        return 1
    return n * fact(n - 1)

n = {{ .N }}
repeat = 1000
last = 0

start = time.perf_counter()
for _ in range(repeat):
    last = fact(n)
duration = (time.perf_counter() - start) * 1000

print(json.dumps({
    "duration_ms": duration,
    "output": last,
}))
