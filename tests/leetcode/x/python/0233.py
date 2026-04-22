import sys


def count_digit_one(n):
    total = 0
    m = 1
    while m <= n:
        high = n // (m * 10)
        cur = (n // m) % 10
        low = n % m
        if cur == 0:
            total += high * m
        elif cur == 1:
            total += high * m + low + 1
        else:
            total += (high + 1) * m
        m *= 10
    return total


lines = sys.stdin.read().splitlines()
if lines:
    t = int(lines[0].strip())
    out = [str(count_digit_one(int(lines[i + 1]))) for i in range(t)]
    sys.stdout.write("\n".join(out))
