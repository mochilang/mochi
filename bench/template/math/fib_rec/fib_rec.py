import json
import time

def fib(n):
    if n <= 1:
        return n
    return fib(n - 1) + fib(n - 2)

n = {{ .N }}

start = time.perf_counter()
result = fib(n)
duration = (time.perf_counter() - start) * 1000

print(json.dumps({
    "duration_ms": duration,
    "output": result,
}))
