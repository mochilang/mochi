def removeElement(nums, val)
	k = 0
	i = 0
	while (i < (nums).length)
		if (nums[i] != val)
			nums[k] = nums[i]
			k = (k + 1)
		end
		i = (i + 1)
	end
	return k
end

