import json
import math
import time


def eval_a(i, j):
    s = i + j
    return 1.0 / (s * (s + 1) // 2 + i + 1)


def mul_av(src, dst, n):
    for i in range(n):
        s = 0.0
        for j in range(n):
            s += eval_a(i, j) * src[j]
        dst[i] = s


def mul_atv(src, dst, n):
    for i in range(n):
        s = 0.0
        for j in range(n):
            s += eval_a(j, i) * src[j]
        dst[i] = s


N = {{ .N }}

u = [1.0] * N
v = [0.0] * N
tmp = [0.0] * N

start = time.perf_counter()
for _ in range(5):
    mul_av(u, tmp, N)
    mul_atv(tmp, v, N)
    mul_av(v, tmp, N)
    mul_atv(tmp, u, N)

uv = 0.0
vv = 0.0
for i in range(N):
    uv += u[i] * v[i]
    vv += v[i] * v[i]

output = int(math.sqrt(uv / vv) * 1e9)
duration_us = (time.perf_counter() - start) * 1e6

print(json.dumps({
    "duration_us": duration_us,
    "output": output,
}))
