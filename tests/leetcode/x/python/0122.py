import sys

def max_profit(prices):
    best = 0
    for i in range(1, len(prices)):
        if prices[i] > prices[i - 1]:
            best += prices[i] - prices[i - 1]
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
