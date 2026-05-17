import json
import time


def nsieve(n):
    xs = [0] * (n + 1)
    count = 0
    i = 2
    while i <= n:
        if xs[i] == 0:
            count += 1
            j = i * i
            while j <= n:
                xs[j] = 1
                j += i
        i += 1
    return count


n = {{ .N }}
repeat = 50
last = 0

start = time.perf_counter()
for _ in range(repeat):
    last = nsieve(n)
duration = (time.perf_counter() - start) * 1e6

print(json.dumps({
    "duration_us": duration,
    "output": last,
}))
