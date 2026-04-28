import sys

MOD = 1_000_000_007


def one(c: str) -> int:
    if c == "*":
        return 9
    if c == "0":
        return 0
    return 1


def two(a: str, b: str) -> int:
    if a == "*" and b == "*":
        return 15
    if a == "*":
        return 2 if "0" <= b <= "6" else 1
    if b == "*":
        if a == "1":
            return 9
        if a == "2":
            return 6
        return 0
    value = int(a + b)
    return 1 if 10 <= value <= 26 else 0


def solve(s: str) -> int:
    prev2 = 1
    prev1 = one(s[0])
    for i in range(1, len(s)):
        cur = (one(s[i]) * prev1 + two(s[i - 1], s[i]) * prev2) % MOD
        prev2, prev1 = prev1, cur
    return prev1


def main() -> None:
    lines = [line.strip() for line in sys.stdin.read().splitlines() if line.strip()]
    if not lines:
        return
    out = [str(solve(s)) for s in lines[1:1 + int(lines[0])]]
    sys.stdout.write("\n\n".join(out))


if __name__ == "__main__":
    main()
