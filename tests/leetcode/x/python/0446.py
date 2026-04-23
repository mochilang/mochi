import sys
from collections import defaultdict

def number_of_arithmetic_slices(nums: list[int]) -> int:
    dp: list[dict[int, int]] = [defaultdict(int) for _ in nums]
    ans = 0
    for i, x in enumerate(nums):
        for j in range(i):
            diff = x - nums[j]
            prev = dp[j][diff]
            ans += prev
            dp[i][diff] += prev + 1
    return ans

def main() -> None:
    data = sys.stdin.read().strip().split()
    if not data:
        return
    idx = 0
    t = int(data[idx]); idx += 1
    ans: list[str] = []
    for _ in range(t):
        n = int(data[idx]); idx += 1
        nums = [int(x) for x in data[idx:idx+n]]; idx += n
        ans.append(str(number_of_arithmetic_slices(nums)))
    sys.stdout.write('\n\n'.join(ans))

if __name__ == '__main__':
    main()
