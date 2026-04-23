import sys


def min_patches(nums: list[int], n: int) -> int:
    miss = 1
    i = 0
    patches = 0
    while miss <= n:
        if i < len(nums) and nums[i] <= miss:
            miss += nums[i]
            i += 1
        else:
            miss += miss
            patches += 1
    return patches


def main() -> None:
    data = sys.stdin.read().strip().split()
    if not data:
        return
    t = int(data[0]); idx = 1; out: list[str] = []
    for tc in range(t):
        size = int(data[idx]); idx += 1
        nums = list(map(int, data[idx:idx + size])); idx += size
        n = int(data[idx]); idx += 1
        if tc: out.append("")
        out.append(str(min_patches(nums, n)))
    sys.stdout.write("\n".join(out))

if __name__ == "__main__":
    main()
