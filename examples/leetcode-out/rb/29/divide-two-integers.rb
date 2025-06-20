def divide(dividend, divisor)
	if ((dividend == (((-2147483647) - 1))) && (divisor == ((-1))))
		return 2147483647
	end
	negative = false
	if (dividend < 0)
		negative = (!negative)
		dividend = (-dividend)
	end
	if (divisor < 0)
		negative = (!negative)
		divisor = (-divisor)
	end
	quotient = 0
	while (dividend >= divisor)
		temp = divisor
		multiple = 1
		while (dividend >= (temp + temp))
			temp = (temp + temp)
			multiple = (multiple + multiple)
		end
		dividend = (dividend - temp)
		quotient = (quotient + multiple)
	end
	if negative
		quotient = (-quotient)
	end
	if (quotient > 2147483647)
		return 2147483647
	end
	if (quotient < (((-2147483647) - 1)))
		return (-2147483648)
	end
	return quotient
end

