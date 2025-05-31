import json
import time

def fib(n):
    a, b = 0, 1
    for _ in range(n):
        a, b = b, a + b
    return a

n = {{ .N }}
repeat = 1000
last = 0

start = time.perf_counter()
for _ in range(repeat):
    last = fib(n)
duration = (time.perf_counter() - start) * 1000

print(json.dumps({
    "duration_ms": duration,
    "output": last,
}))
