def reverseKGroup(nums, k)
	n = (nums).length
	if (k <= 1)
		return nums
	end
	result = []
	i = 0
	while (i < n)
		_end = (i + k)
		if (_end <= n)
			j = (_end - 1)
			while (j >= i)
				result = (result + [nums[j]])
				j = (j - 1)
			end
		else
			j = i
			while (j < n)
				result = (result + [nums[j]])
				j = (j + 1)
			end
		end
		i = (i + k)
	end
	return result
end

