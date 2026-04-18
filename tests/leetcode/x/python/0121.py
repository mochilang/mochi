import sys

def max_profit(prices):
    if not prices:
        return 0
    min_price = prices[0]
    best = 0
    for p in prices[1:]:
        if p - min_price > best:
            best = p - min_price
        if p < min_price:
            min_price = p
    return best

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
