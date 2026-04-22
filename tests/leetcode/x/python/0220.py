import sys


def bucket_id(x, size):
    return x // size if x >= 0 else -((-x - 1) // size) - 1


def solve(nums, index_diff, value_diff):
    if value_diff < 0:
        return False
    size = value_diff + 1
    buckets = {}
    for i, x in enumerate(nums):
        bid = bucket_id(x, size)
        if bid in buckets:
            return True
        if bid - 1 in buckets and abs(x - buckets[bid - 1]) <= value_diff:
            return True
        if bid + 1 in buckets and abs(x - buckets[bid + 1]) <= value_diff:
            return True
        buckets[bid] = x
        if i >= index_diff:
            old = nums[i - index_diff]
            del buckets[bucket_id(old, size)]
    return False


data = sys.stdin.read().split()
if data:
    idx = 0
    t = int(data[idx]); idx += 1
    out = []
    for _ in range(t):
        n = int(data[idx]); idx += 1
        nums = list(map(int, data[idx:idx + n]))
        idx += n
        index_diff = int(data[idx]); value_diff = int(data[idx + 1]); idx += 2
        out.append(str(solve(nums, index_diff, value_diff)).lower())
    sys.stdout.write("\n".join(out))
