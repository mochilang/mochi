import json
import time

def mul(n):
    result = 1
    for i in range(1, n + 1):
        result *= i
    return result

n = {{ .N }}
repeat = 1000
last = 0

start = time.perf_counter()
for _ in range(repeat):
    last = mul(n)
duration = (time.perf_counter() - start) * 1000

print(json.dumps({
    "duration_ms": duration,
    "output": last,
}))
