def swapPairs(nums)
	i = 0
	result = []
	while (i < (nums).length)
		if ((i + 1) < (nums).length)
			result = (result + [nums[(i + 1)], nums[i]])
		else
			result = (result + [nums[i]])
		end
		i = (i + 2)
	end
	return result
end

