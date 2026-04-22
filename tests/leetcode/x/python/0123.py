import sys

def max_profit(prices):
    buy1 = -10**18
    sell1 = 0
    buy2 = -10**18
    sell2 = 0
    for p in prices:
        if -p > buy1:
            buy1 = -p
        if buy1 + p > sell1:
            sell1 = buy1 + p
        if sell1 - p > buy2:
            buy2 = sell1 - p
        if buy2 + p > sell2:
            sell2 = buy2 + p
    return sell2

def main():
    lines = sys.stdin.read().splitlines()
    if not lines:
        return
    t = int(lines[0].strip())
    idx = 1
    out = []
    for _ in range(t):
        n = int(lines[idx].strip()) if idx < len(lines) else 0
        idx += 1
        prices = []
        for _ in range(n):
            prices.append(int(lines[idx].strip()) if idx < len(lines) else 0)
            idx += 1
        out.append(str(max_profit(prices)))
    sys.stdout.write("\n".join(out))

if __name__ == "__main__":
    main()
