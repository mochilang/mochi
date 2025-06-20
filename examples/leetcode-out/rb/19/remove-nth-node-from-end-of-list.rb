def removeNthFromEnd(nums, n)
	idx = ((nums).length - n)
	result = []
	i = 0
	while (i < (nums).length)
		if (i != idx)
			result = (result + [nums[i]])
		end
		i = (i + 1)
	end
	return result
end

