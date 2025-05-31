import json
import time

def is_prime(n):
    if n < 2:
        return False
    for i in range(2, n):
        if n % i == 0:
            return False
    return True

n = {{ .N }}
repeat = 100
last = 0

start = time.perf_counter()
for _ in range(repeat):
    count = 0
    for i in range(2, n + 1):
        if is_prime(i):
            count += 1
    last = count
duration = (time.perf_counter() - start) * 1000

print(json.dumps({
    "duration_ms": duration,
    "output": last,
}))
