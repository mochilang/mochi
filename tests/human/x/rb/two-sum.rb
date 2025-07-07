def two_sum(nums, target)
  n = nums.length
  (0...n).each do |i|
    (i+1...n).each do |j|
      return [i, j] if nums[i] + nums[j] == target
    end
  end
  [-1, -1]
end

result = two_sum([2,7,11,15], 9)
puts result[0]
puts result[1]
