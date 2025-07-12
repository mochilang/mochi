def twoSum(nums: list[int], target: int) -> list[int]:
    """twoSum(nums: list[int], target: int) -> list[int]"""
    n: int = len(nums)
    for i in range(0, n):
        for j in range(i + 1, n):
            if nums[i] + nums[j] == target:
                return [i, j]
    return [-1, -1]


result: list[int] = twoSum([2, 7, 11, 15], 9)
print(result[0])
print(result[1])
