import json
import time

def concat_loop(n):
    acc = "a"
    for _ in range(n):
        acc = acc + "a"
    return len(acc)

n = {{ .N }}
repeat = 1000
last = 0

start = time.perf_counter()
for _ in range(repeat):
    last = concat_loop(n)
duration = (time.perf_counter() - start) * 1e6

print(json.dumps({
    "duration_us": duration,
    "output": last,
}))
