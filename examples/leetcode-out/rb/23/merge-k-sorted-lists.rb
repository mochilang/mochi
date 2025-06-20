def mergeKLists(lists)
	k = (lists).length
	indices = []
	i = 0
	while (i < k)
		indices = (indices + [0])
		i = (i + 1)
	end
	result = []
	while true
		best = 0
		bestList = (-1)
		found = false
		j = 0
		while (j < k)
			idx = indices[j]
			if (idx < (lists[j]).length)
				val = lists[j][idx]
				if ((!found) || (val < best))
					best = val
					bestList = j
					found = true
				end
			end
			j = (j + 1)
		end
		if (!found)
			break
		end
		result = (result + [best])
		indices[bestList] = (indices[bestList] + 1)
	end
	return result
end

