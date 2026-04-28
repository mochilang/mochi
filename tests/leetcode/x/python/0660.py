import sys


def solve(n: int) -> int:
    if n == 0:
        return 0
    digits: list[str] = []
    while n:
        digits.append(str(n % 9))
        n //= 9
    return int("".join(reversed(digits)))


def main() -> None:
    data = sys.stdin.read().strip().split()
    if not data:
        return
    out = [str(solve(int(x))) for x in data[1:1 + int(data[0])]]
    sys.stdout.write("\n\n".join(out))


if __name__ == "__main__":
    main()
