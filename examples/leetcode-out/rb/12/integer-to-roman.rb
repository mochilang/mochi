def intToRoman(num)
	values = [1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1]
	symbols = ["M", "CM", "D", "CD", "C", "XC", "L", "XL", "X", "IX", "V", "IV", "I"]
	result = ""
	i = 0
	while (num > 0)
		while (num >= values[i])
			result = (result + symbols[i])
			num = (num - values[i])
		end
		i = (i + 1)
	end
	return result
end

