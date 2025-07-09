let nums = [1, 2, 3]
let result = nums.compactMap { n in n > 1 ? (n.reduce(0, +)) : nil }
print(result)
