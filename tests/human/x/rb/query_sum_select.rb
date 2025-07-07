nums = [1, 2, 3]
result = nums.select { |n| n > 1 }.sum
puts result
