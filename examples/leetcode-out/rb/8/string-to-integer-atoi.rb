def digit(ch)
	if (ch == "0")
		return 0
	end
	if (ch == "1")
		return 1
	end
	if (ch == "2")
		return 2
	end
	if (ch == "3")
		return 3
	end
	if (ch == "4")
		return 4
	end
	if (ch == "5")
		return 5
	end
	if (ch == "6")
		return 6
	end
	if (ch == "7")
		return 7
	end
	if (ch == "8")
		return 8
	end
	if (ch == "9")
		return 9
	end
	return (-1)
end

def myAtoi(s)
	i = 0
	n = (s).length
	while ((i < n) && (s[i] == " "[0]))
		i = (i + 1)
	end
	sign = 1
	if ((i < n) && (((s[i] == "+"[0]) || (s[i] == "-"[0]))))
		if (s[i] == "-"[0])
			sign = (-1)
		end
		i = (i + 1)
	end
	result = 0
	while (i < n)
		ch = s[i...(i + 1)]
		d = digit(ch)
		if (d < 0)
			break
		end
		result = ((result * 10) + d)
		i = (i + 1)
	end
	result = (result * sign)
	if (result > 2147483647)
		return 2147483647
	end
	if (result < ((-2147483648)))
		return (-2147483648)
	end
	return result
end

