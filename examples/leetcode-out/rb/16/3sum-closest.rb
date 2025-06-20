def threeSumClosest(nums, target)
	sorted = (((nums)).sort_by { |n| n }).map { |n| n }
	n = (sorted).length
	best = ((sorted[0] + sorted[1]) + sorted[2])
	for i in 0...n
		left = (i + 1)
		right = (n - 1)
		while (left < right)
			sum = ((sorted[i] + sorted[left]) + sorted[right])
			if (sum == target)
				return target
			end
			diff = 0
			if (sum > target)
				diff = (sum - target)
			else
				diff = (target - sum)
			end
			bestDiff = 0
			if (best > target)
				bestDiff = (best - target)
			else
				bestDiff = (target - best)
			end
			if (diff < bestDiff)
				best = sum
			end
			if (sum < target)
				left = (left + 1)
			else
				right = (right - 1)
			end
		end
	end
	return best
end

