import json
import time


N = {{ .N }}

start = time.perf_counter()
q, r, t = 1, 0, 1
k, nd, l = 1, 3, 3
h = 0
rem = N
mod = 2147483647

while rem > 0:
    lhs = 4 * q + r
    ntp = nd * t
    rhs = ntp + t
    if lhs < rhs:
        h = (h * 10 + nd) % mod
        new_q = 10 * q
        new_r = 10 * (r - ntp)
        three_q_plus_r = 3 * q + r
        div = (10 * three_q_plus_r) // t
        new_nd = div - 10 * nd
        q = new_q
        r = new_r
        nd = new_nd
        rem -= 1
    else:
        nr = (2 * q + r) * l
        new_q = q * k
        new_t = t * l
        numer = q * (7 * k + 2) + r * l
        new_nd = numer // new_t
        q = new_q
        r = nr
        t = new_t
        nd = new_nd
        k += 1
        l += 2

duration_us = (time.perf_counter() - start) * 1e6

print(json.dumps({
    "duration_us": duration_us,
    "output": h,
}))
