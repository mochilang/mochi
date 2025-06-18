def myAtoi(s)
	i = 0
	n = (s).length
	while ((i < n) && (s[i] == " "))
		i = (i + 1)
	end
	sign = 1
	if ((i < n) && (((s[i] == "+") || (s[i] == "-"))))
		if (s[i] == "-")
			sign = (-1)
		end
		i = (i + 1)
	end
	digits = {"0" => 0, "1" => 1, "2" => 2, "3" => 3, "4" => 4, "5" => 5, "6" => 6, "7" => 7, "8" => 8, "9" => 9}
	result = 0
	while (i < n)
		ch = s[i]
		if (!((digits.include?(ch))))
			break
		end
		d = digits[ch]
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

