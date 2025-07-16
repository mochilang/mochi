def cart2(a, b)
	p = []
	a.each do |x|
		b.each do |y|
			p = (p + [[x, y]])
		end
	end
	return p
end

def llStr(lst)
	s = "["
	i = 0
	while (i < (lst).length)
		row = lst[i]
		s = (s + "[")
		j = 0
		while (j < (row).length)
			s = (s + (row[j]).to_s)
			if (j < ((row).length - 1))
				s = (s + " ")
			end
			j = (j + 1)
		end
		s = (s + "]")
		if (i < ((lst).length - 1))
			s = (s + " ")
		end
		i = (i + 1)
	end
	s = (s + "]")
	return s
end

def main()
	puts(llStr(cart2([1, 2], [3, 4])))
	puts(llStr(cart2([3, 4], [1, 2])))
	puts(llStr(cart2([1, 2], [])))
	puts(llStr(cart2([], [1, 2])))
end

main()
