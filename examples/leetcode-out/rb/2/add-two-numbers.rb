def addTwoNumbers(l1, l2)
	i = 0
	j = 0
	carry = 0
	result = []
	while (((i < (l1).length) || (j < (l2).length)) || (carry > 0))
		x = 0
		if (i < (l1).length)
			x = l1[i]
			i = (i + 1)
		end
		y = 0
		if (j < (l2).length)
			y = l2[j]
			j = (j + 1)
		end
		sum = ((x + y) + carry)
		digit = (sum % 10)
		carry = (sum / 10)
		result = (result + [digit])
	end
	return result
end

