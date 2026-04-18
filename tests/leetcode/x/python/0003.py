import sys


def longest(s: str) -> int:
    last = {}
    left = 0
    best = 0
    for right, ch in enumerate(s):
        if ch in last and last[ch] >= left:
            left = last[ch] + 1
        last[ch] = right
        best = max(best, right - left + 1)
    return best


def main() -> None:
    data = sys.stdin.read().splitlines()
    if not data:
        return
    t = int(data[0].strip())
    lines = [str(longest(data[i + 1] if i + 1 < len(data) else "")) for i in range(t)]
    sys.stdout.write("\n".join(lines))


if __name__ == "__main__":
    main()
