def toUnsigned16(n)
	u = n
	if (u < 0)
		u = (u + 65536)
	end
	return (u % 65536)
end

def bin16(n)
	u = toUnsigned16(n)
	bits = ""
	mask = 32768
	(0...16).each do |i|
		if (u >= mask)
			bits = (bits + "1")
			u = (u - mask)
		else
			bits = (bits + "0")
		end
		mask = ((mask / 2))
	end
	return bits
end

def bit_and(a, b)
	ua = toUnsigned16(a)
	ub = toUnsigned16(b)
	res = 0
	bit = 1
	(0...16).each do |i|
		if (((ua % 2) == 1) && ((ub % 2) == 1))
			res = (res + bit)
		end
		ua = ((ua / 2))
		ub = ((ub / 2))
		bit = (bit * 2)
	end
	return res
end

def bit_or(a, b)
	ua = toUnsigned16(a)
	ub = toUnsigned16(b)
	res = 0
	bit = 1
	(0...16).each do |i|
		if (((ua % 2) == 1) || ((ub % 2) == 1))
			res = (res + bit)
		end
		ua = ((ua / 2))
		ub = ((ub / 2))
		bit = (bit * 2)
	end
	return res
end

def bit_xor(a, b)
	ua = toUnsigned16(a)
	ub = toUnsigned16(b)
	res = 0
	bit = 1
	(0...16).each do |i|
		abit = (ua % 2)
		bbit = (ub % 2)
		if ((((abit == 1) && (bbit == 0))) || (((abit == 0) && (bbit == 1))))
			res = (res + bit)
		end
		ua = ((ua / 2))
		ub = ((ub / 2))
		bit = (bit * 2)
	end
	return res
end

def bit_not(a)
	ua = toUnsigned16(a)
	return (65535 - ua)
end

def shl(a, b)
	ua = toUnsigned16(a)
	i = 0
	while (i < b)
		ua = (((ua * 2)) % 65536)
		i = (i + 1)
	end
	return ua
end

def shr(a, b)
	ua = toUnsigned16(a)
	i = 0
	while (i < b)
		ua = ((ua / 2))
		i = (i + 1)
	end
	return ua
end

def las(a, b)
	return shl(a, b)
end

def ras(a, b)
	val = a
	i = 0
	while (i < b)
		if (val >= 0)
			val = ((val / 2))
		else
			val = ((((val - 1)) / 2))
		end
		i = (i + 1)
	end
	return toUnsigned16(val)
end

def rol(a, b)
	ua = toUnsigned16(a)
	left = shl(ua, b)
	right = shr(ua, (16 - b))
	return toUnsigned16((left + right))
end

def ror(a, b)
	ua = toUnsigned16(a)
	right = shr(ua, b)
	left = shl(ua, (16 - b))
	return toUnsigned16((left + right))
end

def bitwise(a, b)
	puts(("a:   " + bin16(a)))
	puts(("b:   " + bin16(b)))
	puts(("and: " + bin16(bit_and(a, b))))
	puts(("or:  " + bin16(bit_or(a, b))))
	puts(("xor: " + bin16(bit_xor(a, b))))
	puts(("not: " + bin16(bit_not(a))))
	if (b < 0)
		puts("Right operand is negative, but all shifts require an unsigned right operand (shift distance).")
		return nil
	end
	puts(("shl: " + bin16(shl(a, b))))
	puts(("shr: " + bin16(shr(a, b))))
	puts(("las: " + bin16(las(a, b))))
	puts(("ras: " + bin16(ras(a, b))))
	puts(("rol: " + bin16(rol(a, b))))
	puts(("ror: " + bin16(ror(a, b))))
end

bitwise((-460), 6)
