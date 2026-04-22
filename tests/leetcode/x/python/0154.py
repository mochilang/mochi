import sys


def find_min(nums):
    left = 0
    right = len(nums) - 1
    while left < right:
        mid = (left + right) // 2
        if nums[mid] < nums[right]:
            right = mid
        elif nums[mid] > nums[right]:
            left = mid + 1
        else:
            right -= 1
    return nums[left]


lines = sys.stdin.read().splitlines()
if lines:
    tc = int(lines[0])
    idx = 1
    out = []
    for _ in range(tc):
        n = int(lines[idx])
        idx += 1
        nums = [int(lines[idx + i]) for i in range(n)]
        idx += n
        out.append(str(find_min(nums)))
    sys.stdout.write("\n\n".join(out))
