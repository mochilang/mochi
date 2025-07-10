def twoSum(nums, target)
  n = (nums).length
  (0...n).each do |i|
    ((i + 1)...n).each do |j|
      if ((nums[i] + nums[j]) == target)
        return [i, j]
      end
    end
  end
  return [(-1), (-1)]
end

$result = twoSum([2, 7, 11, 15], 9)
puts($result[0])
puts($result[1])
