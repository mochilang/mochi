import json
import time


COMPL = {
    ord('A'): ord('T'),
    ord('T'): ord('A'),
    ord('C'): ord('G'),
    ord('G'): ord('C'),
}


def complement(c):
    return COMPL.get(c, c)


N = {{ .N }}

bases = (ord('A'), ord('C'), ord('G'), ord('T'))
in_buf = bytearray(N)
out_buf = bytearray(N)

start = time.perf_counter()
for i in range(N):
    in_buf[i] = bases[i % 4]
for i in range(N):
    out_buf[N - 1 - i] = complement(in_buf[i])

total = 0
for i in range(N):
    total += out_buf[i]

duration_us = (time.perf_counter() - start) * 1e6

print(json.dumps({
    "duration_us": duration_us,
    "output": total,
}))
