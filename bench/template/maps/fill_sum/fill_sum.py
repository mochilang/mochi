import json
import time

def fill_sum(n):
    m = {}
    for i in range(n):
        m[i] = i
    s = 0
    for j in range(n):
        s += m[j]
    return s

n = {{ .N }}
repeat = 1000
last = 0

start = time.perf_counter()
for _ in range(repeat):
    last = fill_sum(n)
duration = (time.perf_counter() - start) * 1e6

print(json.dumps({
    "duration_us": duration,
    "output": last,
}))
