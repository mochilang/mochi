import json
import time


def count_flips(perm):
    flips = 0
    head = perm[0]
    while head != 1:
        lo, hi = 0, head - 1
        while lo < hi:
            perm[lo], perm[hi] = perm[hi], perm[lo]
            lo += 1
            hi -= 1
        head = perm[0]
        flips += 1
    return flips


trials = {{ .N }}
total = 0
perm = [0] * 7

start = time.perf_counter()
for k in range(trials):
    for i in range(7):
        perm[i] = ((i + k) % 7) + 1
    total += count_flips(perm)
duration = (time.perf_counter() - start) * 1e6

print(json.dumps({
    "duration_us": duration,
    "output": total,
}))
