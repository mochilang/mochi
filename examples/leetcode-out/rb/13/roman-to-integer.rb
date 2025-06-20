def romanToInt(s)
	values = {"I" => 1, "V" => 5, "X" => 10, "L" => 50, "C" => 100, "D" => 500, "M" => 1000}
	total = 0
	i = 0
	n = (s).length
	while (i < n)
		curr = values[s[i]]
		if ((i + 1) < n)
			_next = values[s[(i + 1)]]
			if (curr < _next)
				total = ((total + _next) - curr)
				i = (i + 2)
				next
			end
		end
		total = (total + curr)
		i = (i + 1)
	end
	return total
end

