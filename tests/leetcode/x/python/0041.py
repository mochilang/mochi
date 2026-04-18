import sys

def first_missing_positive(nums):
    n = len(nums)
    i = 0
    while i < n:
        v = nums[i]
        if 1 <= v <= n and nums[v - 1] != v:
            nums[i], nums[v - 1] = nums[v - 1], nums[i]
        else:
            i += 1
    for i, v in enumerate(nums):
        if v != i + 1:
            return i + 1
    return n + 1

def main():
    lines = sys.stdin.read().splitlines()
    if not lines:
        return
    t = int(lines[0].strip())
    idx = 1
    out = []
    for _ in range(t):
        n = int(lines[idx].strip()) if idx < len(lines) else 0
        idx += 1
        nums = []
        for _ in range(n):
            nums.append(int(lines[idx].strip()) if idx < len(lines) else 0)
            idx += 1
        out.append(str(first_missing_positive(nums)))
    sys.stdout.write("\n".join(out))

if __name__ == '__main__':
    main()
