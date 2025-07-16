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

def copy(xs)
	out = []
	xs.each do |v|
		out = (out + [v])
	end
	return out
end

def cartN(lists)
	if (lists == nil)
		return []
	end
	a = lists
	if ((a).length == 0)
		return [[]]
	end
	out = []
	last = ((a).length - 1)
	left = cartN(a[0...last])
	left.each do |p|
		a[last].each do |x|
			row = copy(p)
			row = (row + [x])
			out = (out + [row])
		end
	end
	return out
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
