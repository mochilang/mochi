import json, time

def matmul(a, b):
    n, m, p = len(a), len(b[0]), len(b)
    return [
        [sum(a[i][k] * b[k][j] for k in range(p)) for j in range(m)]
        for i in range(n)
    ]

size = {{ .N }}
repeat = 10

a = [[i + j for j in range(size)] for i in range(size)]
b = [[i * j for j in range(size)] for i in range(size)]

start = time.perf_counter()
for _ in range(repeat):
    last = matmul(a, b)
duration = (time.perf_counter() - start) * 1000

print(json.dumps({
    "duration_ms": duration,
    "output": last[0][0],
}))
