import sys


def solve(k, prices):
    n = len(prices)
    if k >= n // 2:
        return sum(max(0, prices[i] - prices[i - 1]) for i in range(1, n))
    neg_inf = -10**18
    buy = [neg_inf] * (k + 1)
    sell = [0] * (k + 1)
    for price in prices:
        for t in range(1, k + 1):
            buy[t] = max(buy[t], sell[t - 1] - price)
            sell[t] = max(sell[t], buy[t] + price)
    return sell[k]


data = sys.stdin.read().split()
if data:
    idx = 0
    t = int(data[idx]); idx += 1
    out = []
    for _ in range(t):
        k = int(data[idx]); idx += 1
        n = int(data[idx]); idx += 1
        prices = list(map(int, data[idx:idx + n]))
        idx += n
        out.append(str(solve(k, prices)))
    sys.stdout.write("\n".join(out))
