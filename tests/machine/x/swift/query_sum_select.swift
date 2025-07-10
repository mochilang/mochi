var nums = [1, 2, 3]
var result = nums.filter { n in n > 1 }.reduce(0, +)
print(result)
