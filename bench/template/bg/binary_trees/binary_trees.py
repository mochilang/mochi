import json
import sys
import time


# Match the Mochi peer: trees are nested 2-element lists, not tuples,
# because vm2 has no struct support and we want all four columns to
# walk the same data shape.
def make_tree(depth):
    if depth == 0:
        return []
    return [make_tree(depth - 1), make_tree(depth - 1)]


def check_tree(t):
    if len(t) == 0:
        return 1
    return 1 + check_tree(t[0]) + check_tree(t[1])


sys.setrecursionlimit(1 << 20)

depth = {{ .N }}
iters = 1 << depth

total = 0
start = time.perf_counter()
for _ in range(iters):
    total += check_tree(make_tree(depth))
duration = (time.perf_counter() - start) * 1e6

print(json.dumps({
    "duration_us": duration,
    "output": total,
}))
