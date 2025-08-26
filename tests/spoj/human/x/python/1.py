# Solution for SPOJ TEST - Life, the Universe, and Everything
# https://www.spoj.com/problems/TEST/

import sys

for line in sys.stdin:
    line = line.strip()
    if not line:
        continue
    n = int(line)
    if n == 42:
        break
    print(n)
