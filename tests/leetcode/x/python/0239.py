import sys
from collections import deque


def solve(nums, k):
    dq = deque()
    ans = []
    for i, x in enumerate(nums):
        while dq and dq[0] <= i - k:
            dq.popleft()
        while dq and nums[dq[-1]] <= x:
            dq.pop()
        dq.append(i)
        if i >= k - 1:
            ans.append(nums[dq[0]])
    return ans


data = sys.stdin.read().split()
if data:
    idx = 0
    t = int(data[idx]); idx += 1
    out = []
    for _ in range(t):
        n = int(data[idx]); idx += 1
        nums = list(map(int, data[idx:idx + n])); idx += n
        k = int(data[idx]); idx += 1
        ans = solve(nums, k)
        out.append("\n".join([str(len(ans))] + [str(x) for x in ans]))
    sys.stdout.write("\n\n".join(out))
