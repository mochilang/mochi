def sortFloat(xs)
	arr = xs
	n = (arr).length
	i = 0
	while (i < n)
		j = 0
		while (j < (n - 1))
			if (arr[j] > arr[(j + 1)])
				tmp = arr[j]
				arr[j] = arr[(j + 1)]
				arr[(j + 1)] = tmp
			end
			j = (j + 1)
		end
		i = (i + 1)
	end
	return arr
end

def median(a)
	arr = sortFloat(a)
	half = (((arr).length / 2))
	m = arr[half]
	if (((arr).length % 2) == 0)
		m = (((m + arr[(half - 1)])) / 2.0)
	end
	return m
end

puts((median([3.0, 1.0, 4.0, 1.0])).to_s)
puts((median([3.0, 1.0, 4.0, 1.0, 5.0])).to_s)
