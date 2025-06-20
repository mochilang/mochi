def fourSum(nums, target)
	sorted = (((nums)).sort_by { |n| n }).map { |n| n }
	n = (sorted).length
	result = []
	for i in 0...n
		if ((i > 0) && (sorted[i] == sorted[(i - 1)]))
			next
		end
		for j in (i + 1)...n
			if ((j > (i + 1)) && (sorted[j] == sorted[(j - 1)]))
				next
			end
			left = (j + 1)
			right = (n - 1)
			while (left < right)
				sum = (((sorted[i] + sorted[j]) + sorted[left]) + sorted[right])
				if (sum == target)
					result = (result + [[sorted[i], sorted[j], sorted[left], sorted[right]]])
					left = (left + 1)
					right = (right - 1)
					while ((left < right) && (sorted[left] == sorted[(left - 1)]))
						left = (left + 1)
					end
					while ((left < right) && (sorted[right] == sorted[(right + 1)]))
						right = (right - 1)
					end
				elsif (sum < target)
					left = (left + 1)
				else
					right = (right - 1)
				end
			end
		end
	end
	return result
end

