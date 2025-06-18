def twoSum(nums, target)
	n = (nums).length
	for i in 0...n
		for j in (i + 1)...n
			if ((nums[i] + nums[j]) == target)
				return [i, j]
			end
		end
	end
	return [(-1), (-1)]
end

result = twoSum([2, 7, 11, 15], 9)
puts([result[0]].join(" "))
puts([result[1]].join(" "))
