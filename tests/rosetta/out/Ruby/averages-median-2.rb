def sel(list, k)
	i = 0
	while (i <= k)
		minIndex = i
		j = (i + 1)
		while (j < (list).length)
			if (list[j] < list[minIndex])
				minIndex = j
			end
			j = (j + 1)
		end
		tmp = list[i]
		list[i] = list[minIndex]
		list[minIndex] = tmp
		i = (i + 1)
	end
	return list[k]
end

def median(a)
	arr = a
	half = (((arr).length / 2))
	med = sel(arr, half)
	if (((arr).length % 2) == 0)
		return (((med + arr[(half - 1)])) / 2.0)
	end
	return med
end

puts((median([3.0, 1.0, 4.0, 1.0])).to_s)
puts((median([3.0, 1.0, 4.0, 1.0, 5.0])).to_s)
