import sys


def candy(ratings):
    n = len(ratings)
    candies = [1] * n
    for i in range(1, n):
        if ratings[i] > ratings[i - 1]:
            candies[i] = candies[i - 1] + 1
    for i in range(n - 2, -1, -1):
        if ratings[i] > ratings[i + 1]:
            candies[i] = max(candies[i], candies[i + 1] + 1)
    return sum(candies)


def read_case(lines, idx):
    n = int(lines[idx])
    idx += 1
    arr = [int(lines[idx + i]) for i in range(n)]
    return candy(arr), idx + n


lines = sys.stdin.read().splitlines()
if lines:
    tc = int(lines[0])
    idx = 1
    out = []
    for _ in range(tc):
        ans, idx = read_case(lines, idx)
        out.append(str(ans))
    sys.stdout.write("\n\n".join(out))
