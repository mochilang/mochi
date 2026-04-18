import sys

INT_MIN = -2147483648
INT_MAX = 2147483647


def reverse_int(x: int) -> int:
    ans = 0
    while x != 0:
        digit = int(x % 10) if x >= 0 else -int((-x) % 10)
        x = int(x / 10)
        if ans > INT_MAX // 10 or (ans == INT_MAX // 10 and digit > 7):
            return 0
        if ans < INT_MIN // 10 or (ans == INT_MIN // 10 and digit < -8):
            return 0
        ans = ans * 10 + digit
    return ans


def main() -> None:
    data = sys.stdin.read().splitlines()
    if not data:
        return
    t = int(data[0].strip())
    out = []
    for i in range(t):
        x = int(data[i + 1].strip()) if i + 1 < len(data) else 0
        out.append(str(reverse_int(x)))
    sys.stdout.write('\n'.join(out))


if __name__ == '__main__':
    main()
