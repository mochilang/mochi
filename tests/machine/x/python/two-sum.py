def twoSum(nums, target):
    n = len(nums)
    for i in range(n):
        for j in range(i + 1, n):
            if nums[i] + nums[j] == target:
                return [i, j]
    return [-1, -1]

result = twoSum([2,7,11,15], 9)
print(result[0])
print(result[1])
