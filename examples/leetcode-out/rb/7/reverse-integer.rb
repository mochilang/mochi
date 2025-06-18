def reverse(x)
	sign = 1
	n = x
	if (n < 0)
		sign = (-1)
		n = (-n)
	end
	rev = 0
	while (n != 0)
		digit = (n % 10)
		rev = ((rev * 10) + digit)
		n = (n / 10)
	end
	rev = (rev * sign)
	if ((rev < (((-2147483647) - 1))) || (rev > 2147483647))
		return 0
	end
	return rev
end

