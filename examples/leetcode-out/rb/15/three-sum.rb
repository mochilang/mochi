def threeSum(nums)
	sorted = (((nums)).sort_by { |x| x }).map { |x| x }
	n = (sorted).length
	res = []
	i = 0
	while (i < n)
		if ((i > 0) && (sorted[i] == sorted[(i - 1)]))
			i = (i + 1)
			next
		end
		left = (i + 1)
		right = (n - 1)
		while (left < right)
			sum = ((sorted[i] + sorted[left]) + sorted[right])
			if (sum == 0)
				res = (res + [[sorted[i], sorted[left], sorted[right]]])
				left = (left + 1)
				while ((left < right) && (sorted[left] == sorted[(left - 1)]))
					left = (left + 1)
				end
				right = (right - 1)
				while ((left < right) && (sorted[right] == sorted[(right + 1)]))
					right = (right - 1)
				end
			elsif (sum < 0)
				left = (left + 1)
			else
				right = (right - 1)
			end
		end
		i = (i + 1)
	end
	return res
end

