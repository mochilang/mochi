import sys

def split_array(nums: list[int], k: int) -> int:
    lo = max(nums)
    hi = sum(nums)
    while lo < hi:
        mid = (lo + hi) // 2
        pieces = 1
        cur = 0
        for x in nums:
            if cur + x > mid:
                pieces += 1
                cur = x
            else:
                cur += x
        if pieces <= k:
            hi = mid
        else:
            lo = mid + 1
    return lo

def main() -> None:
    data = sys.stdin.read().strip().split()
    if not data:
        return
    idx = 0
    t = int(data[idx]); idx += 1
    ans: list[str] = []
    for _ in range(t):
        n = int(data[idx]); k = int(data[idx + 1]); idx += 2
        nums = [int(x) for x in data[idx:idx+n]]; idx += n
        ans.append(str(split_array(nums, k)))
    sys.stdout.write('\n\n'.join(ans))

if __name__ == '__main__':
    main()
