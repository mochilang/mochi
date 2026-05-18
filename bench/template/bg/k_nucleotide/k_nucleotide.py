import json
import time


def lookup(prob):
    if prob < 0.3029549426680:
        return 0
    if prob < 0.5009432431601:
        return 1
    if prob < 0.6984905497992:
        return 2
    return 3


N = {{ .N }}

start = time.perf_counter()
counts = {}
seed = (42 * 3877 + 29573) % 139968
prev = lookup(seed / 139968.0)
counts[prev] = 1
for i in range(1, N):
    seed = (seed * 3877 + 29573) % 139968
    code = lookup(seed / 139968.0)
    counts[code] = counts.get(code, 0) + 1
    key2 = 4 + prev * 4 + code
    counts[key2] = counts.get(key2, 0) + 1
    prev = code

h = 0
for k in range(20):
    h = (h * 1009 + counts.get(k, 0)) % 2147483647

duration_us = (time.perf_counter() - start) * 1e6

print(json.dumps({
    "duration_us": duration_us,
    "output": h,
}))
