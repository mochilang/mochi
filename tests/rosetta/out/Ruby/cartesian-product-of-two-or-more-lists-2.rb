def listStr(xs)
	s = "["
	i = 0
	while (i < (xs).length)
		s = (s + (xs[i]).to_s)
		if (i < ((xs).length - 1))
			s = (s + " ")
		end
		i = (i + 1)
	end
	s = (s + "]")
	return s
end

def llStr(lst)
	s = "["
	i = 0
	while (i < (lst).length)
		s = (s + listStr(lst[i]))
		if (i < ((lst).length - 1))
			s = (s + " ")
		end
		i = (i + 1)
	end
	s = (s + "]")
	return s
end

def cartN(lists)
	if (lists == nil)
		return []
	end
	a = lists
	if ((a).length == 0)
		return [[]]
	end
	c = 1
	a.each do |xs|
		c = (c * (xs).length)
	end
	if (c == 0)
		return []
	end
	res = []
	idx = []
	a.each do |_|
		idx = (idx + [0])
	end
	n = (a).length
	count = 0
	while (count < c)
		row = []
		j = 0
		while (j < n)
			row = (row + [a[j][idx[j]]])
			j = (j + 1)
		end
		res = (res + [row])
		k = (n - 1)
		while (k >= 0)
			idx[k] = (idx[k] + 1)
			if (idx[k] < (a[k]).length)
				break
			end
			idx[k] = 0
			k = (k - 1)
		end
		count = (count + 1)
	end
	return res
end

def main()
	puts(llStr(cartN([[1, 2], [3, 4]])))
	puts(llStr(cartN([[3, 4], [1, 2]])))
	puts(llStr(cartN([[1, 2], []])))
	puts(llStr(cartN([[], [1, 2]])))
	puts("")
	puts("[")
	cartN([[1776, 1789], [7, 12], [4, 14, 23], [0, 1]]).each do |p|
		puts((" " + listStr(p)))
	end
	puts("]")
	puts(llStr(cartN([[1, 2, 3], [30], [500, 100]])))
	puts(llStr(cartN([[1, 2, 3], [], [500, 100]])))
	puts("")
	puts(llStr(cartN(nil)))
	puts(llStr(cartN([])))
end

main()
