import sys

VALUES = {'I': 1, 'V': 5, 'X': 10, 'L': 50, 'C': 100, 'D': 500, 'M': 1000}


def roman_to_int(s: str) -> int:
    total = 0
    for i, ch in enumerate(s):
        cur = VALUES[ch]
        nxt = VALUES[s[i + 1]] if i + 1 < len(s) else 0
        total += -cur if cur < nxt else cur
    return total


def main() -> None:
    tokens = sys.stdin.read().split()
    if not tokens:
        return
    t = int(tokens[0])
    out = [str(roman_to_int(s)) for s in tokens[1:1 + t]]
    sys.stdout.write("\n".join(out))


if __name__ == "__main__":
    main()
