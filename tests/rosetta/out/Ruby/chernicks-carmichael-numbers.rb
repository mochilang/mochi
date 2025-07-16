def isPrime(n)
	if (n < 2)
		return false
	end
	if ((n % 2) == 0)
		return (n == 2)
	end
	if ((n % 3) == 0)
		return (n == 3)
	end
	d = 5
	while ((d * d) <= n)
		if ((n % d) == 0)
			return false
		end
		d = (d + 2)
		if ((n % d) == 0)
			return false
		end
		d = (d + 4)
	end
	return true
end

def bigTrim(a)
	n = (a).length
	while ((n > 1) && (a[(n - 1)] == 0))
		a = a[0...(n - 1)]
		n = (n - 1)
	end
	return a
end

def bigFromInt(x)
	if (x == 0)
		return [0]
	end
	digits = []
	n = x
	while (n > 0)
		digits = (digits + [(n % 10)])
		n = (n / 10)
	end
	return digits
end

def bigMulSmall(a, m)
	if (m == 0)
		return [0]
	end
	res = []
	carry = 0
	i = 0
	while (i < (a).length)
		prod = ((a[i] * m) + carry)
		res = (res + [(prod % 10)])
		carry = (prod / 10)
		i = (i + 1)
	end
	while (carry > 0)
		res = (res + [(carry % 10)])
		carry = (carry / 10)
	end
	return bigTrim(res)
end

def bigToString(a)
	s = ""
	i = ((a).length - 1)
	while (i >= 0)
		s = (s + (a[i]).to_s)
		i = (i - 1)
	end
	return s
end

def pow2(k)
	r = 1
	i = 0
	while (i < k)
		r = (r * 2)
		i = (i + 1)
	end
	return r
end

def ccFactors(n, m)
	p = ((6 * m) + 1)
	if (!isPrime(p))
		return []
	end
	prod = bigFromInt(p)
	p = ((12 * m) + 1)
	if (!isPrime(p))
		return []
	end
	prod = bigMulSmall(prod, p)
	i = 1
	while (i <= (n - 2))
		p = ((((pow2(i) * 9) * m)) + 1)
		if (!isPrime(p))
			return []
		end
		prod = bigMulSmall(prod, p)
		i = (i + 1)
	end
	return prod
end

def ccNumbers(start, _end)
	n = start
	while (n <= _end)
		m = 1
		if (n > 4)
			m = pow2((n - 4))
		end
		while true
			num = ccFactors(n, m)
			if ((num).length > 0)
				puts(((("a(" + (n).to_s) + ") = ") + bigToString(num)))
				break
			end
			if (n <= 4)
				m = (m + 1)
			else
				m = (m + pow2((n - 4)))
			end
		end
		n = (n + 1)
	end
end

ccNumbers(3, 9)
