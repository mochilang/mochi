import json
import time


def lookup(prob):
    if prob < 0.3029549426680:
        return 97
    if prob < 0.5009432431601:
        return 99
    if prob < 0.6984905497992:
        return 103
    return 116


N = {{ .N }}

start = time.perf_counter()
seed = 42
h = 0
for _ in range(N):
    seed = (seed * 3877 + 29573) % 139968
    prob = seed / 139968.0
    b = lookup(prob)
    h = (h * 1009 + b) % 2147483647

duration_us = (time.perf_counter() - start) * 1e6

print(json.dumps({
    "duration_us": duration_us,
    "output": h,
}))
