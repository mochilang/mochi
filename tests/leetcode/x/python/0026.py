import sys


def remove_duplicates(nums):
    if not nums:
        return 0
    k = 1
    for i in range(1, len(nums)):
        if nums[i] != nums[k - 1]:
            nums[k] = nums[i]
            k += 1
    return k


def main():
    tokens = sys.stdin.read().split()
    if not tokens:
        return
    it = iter(tokens)
    try:
        t_str = next(it)
    except StopIteration:
        return
    t = int(t_str)
    for _ in range(t):
        n = int(next(it))
        nums = [int(next(it)) for _ in range(n)]
        k = remove_duplicates(nums)
        print(" ".join(map(str, nums[:k])))


if __name__ == "__main__":
    main()
