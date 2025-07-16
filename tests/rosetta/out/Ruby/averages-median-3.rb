def qsel(a, k)
	arr = a
	while ((arr).length > 1)
		px = (now.call() % (arr).length)
		pv = arr[px]
		last = ((arr).length - 1)
		tmp = arr[px]
		arr[px] = arr[last]
		arr[last] = tmp
		px = 0
		i = 0
		while (i < last)
			v = arr[i]
			if (v < pv)
				tmp2 = arr[px]
				arr[px] = arr[i]
				arr[i] = tmp2
				px = (px + 1)
			end
			i = (i + 1)
		end
		if (px == k)
			return pv
		end
		if (k < px)
			arr = arr[0...px]
		else
			tmp2 = arr[px]
			arr[px] = pv
			arr[last] = tmp2
			arr = arr[((px + 1))..-1]
			k = (k - ((px + 1)))
		end
	end
	return arr[0]
end

def median(list)
	arr = list
	half = (((arr).length / 2))
	med = qsel(arr, half)
	if (((arr).length % 2) == 0)
		return (((med + qsel(arr, (half - 1)))) / 2.0)
	end
	return med
end

puts((median([3.0, 1.0, 4.0, 1.0])).to_s)
puts((median([3.0, 1.0, 4.0, 1.0, 5.0])).to_s)
