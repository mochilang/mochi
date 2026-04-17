import sys


def two_sum(nums, target):
    for i in range(len(nums)):
        for j in range(i + 1, len(nums)):
            if nums[i] + nums[j] == target:
                return i, j
    return 0, 0


def main() -> None:
    tokens = sys.stdin.read().split()
    if not tokens:
        return
    values = list(map(int, tokens))
    idx = 0
    t = values[idx]
    idx += 1
    out = []
    for _ in range(t):
        n = values[idx]
        target = values[idx + 1]
        idx += 2
        nums = values[idx:idx + n]
        idx += n
        a, b = two_sum(nums, target)
        out.append(f"{a} {b}")
    sys.stdout.write("\n".join(out))


if __name__ == "__main__":
    main()
