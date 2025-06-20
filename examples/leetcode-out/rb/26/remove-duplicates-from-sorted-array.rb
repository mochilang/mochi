def removeDuplicates(nums)
	if ((nums).length == 0)
		return 0
	end
	count = 1
	prev = nums[0]
	i = 1
	while (i < (nums).length)
		cur = nums[i]
		if (cur != prev)
			count = (count + 1)
			prev = cur
		end
		i = (i + 1)
	end
	return count
end

