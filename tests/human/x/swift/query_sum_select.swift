let nums = [1, 2, 3]
let result = nums.filter { $0 > 1 }.reduce(0, +)
print(result)
