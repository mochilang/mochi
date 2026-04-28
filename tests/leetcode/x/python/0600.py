import sys


def solve(n: int) -> int:
    f = [0] * 32
    f[0] = 1
    f[1] = 2
    for i in range(2, 32):
        f[i] = f[i - 1] + f[i - 2]

    ans = 0
    prev = 0
    for i in range(30, -1, -1):
        if n & (1 << i):
            ans += f[i]
            if prev == 1:
                return ans
            prev = 1
        else:
            prev = 0
    return ans + 1


def main() -> None:
    data = sys.stdin.read().strip().split()
    if not data:
        return
    t = int(data[0])
    nums = list(map(int, data[1:1 + t]))
    sys.stdout.write("\n\n".join(str(solve(n)) for n in nums))


if __name__ == "__main__":
    main()
