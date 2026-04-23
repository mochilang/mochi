import sys


def make_pal(left: int) -> int:
    s = str(left)
    return int(s + s[::-1])


def solve(n: int) -> int:
    if n == 1:
        return 9
    upper = 10**n - 1
    lower = 10 ** (n - 1)
    for left in range(upper, lower - 1, -1):
        pal = make_pal(left)
        x = upper
        while x * x >= pal:
            if pal % x == 0:
                y = pal // x
                if lower <= y <= upper:
                    return pal % 1337
            x -= 1
    return -1


def main() -> None:
    data = sys.stdin.read().strip().split()
    if not data:
        return
    t = int(data[0])
    ans = [str(solve(int(data[i]))) for i in range(1, t + 1)]
    sys.stdout.write("\n\n".join(ans))


if __name__ == "__main__":
    main()
