def convert(s, numRows)
	if ((numRows <= 1) || (numRows >= (s).length))
		return s
	end
	rows = []
	i = 0
	while (i < numRows)
		rows = (rows + [""])
		i = (i + 1)
	end
	curr = 0
	step = 1
	for ch in s
		rows[curr] = (rows[curr] + ch)
		if (curr == 0)
			step = 1
		elsif (curr == (numRows - 1))
			step = (-1)
		end
		curr = (curr + step)
	end
	result = ""
	for row in rows
		result = (result + row)
	end
	return result
end

