def primesUpTo(n)
	sieve = []
	i = 0
	while (i <= n)
		sieve = (sieve + [true])
		i = (i + 1)
	end
	p = 2
	while ((p * p) <= n)
		if sieve[p]
			m = (p * p)
			while (m <= n)
				sieve[m] = false
				m = (m + p)
			end
		end
		p = (p + 1)
	end
	res = []
	x = 2
	while (x <= n)
		if sieve[x]
			res = (res + [x])
		end
		x = (x + 1)
	end
	return res
end

def sortInts(xs)
	res = []
	tmp = xs
	while ((tmp).length > 0)
		min = tmp[0]
		idx = 0
		i = 1
		while (i < (tmp).length)
			if (tmp[i] < min)
				min = tmp[i]
				idx = i
			end
			i = (i + 1)
		end
		res = (res + [min])
		out = []
		j = 0
		while (j < (tmp).length)
			if (j != idx)
				out = (out + [tmp[j]])
			end
			j = (j + 1)
		end
		tmp = out
	end
	return res
end

def commatize(n)
	s = (n).to_s
	i = ((s).length - 3)
	while (i >= 1)
		s = ((s[0...i] + ",") + s[i...(s).length])
		i = (i - 3)
	end
	return s
end

def getBrilliant(digits, limit, countOnly)
	brilliant = []
	count = 0
	pow = 1
	_next = 999999999999999
	k = 1
	while (k <= digits)
		s = []
		$primes.each do |p|
			if (p >= (pow * 10))
				break
			end
			if (p > pow)
				s = (s + [p])
			end
		end
		i = 0
		while (i < (s).length)
			j = i
			while (j < (s).length)
				prod = (s[i] * s[j])
				if (prod < limit)
					if countOnly
						count = (count + 1)
					else
						brilliant = (brilliant + [prod])
					end
				else
					if (prod < _next)
						_next = prod
					end
					break
				end
				j = (j + 1)
			end
			i = (i + 1)
		end
		pow = (pow * 10)
		k = (k + 1)
	end
	if countOnly
		return {"bc" => count, "next" => _next}
	end
	return {"bc" => brilliant, "next" => _next}
end

def main()
	puts("First 100 brilliant numbers:")
	r = getBrilliant(2, 10000, false)
	br = sortInts(r["bc"])
	br = br[0...100]
	i = 0
	while (i < (br).length)
		puts([((br[i]).to_s.call(4, " ") + " "), false].join(" "))
		if ((((i + 1)) % 10) == 0)
			puts(["", true].join(" "))
		end
		i = (i + 1)
	end
	puts(["", true].join(" "))
	k = 1
	while (k <= 13)
		limit = pow.call(10, k)
		r2 = getBrilliant(k, limit, true)
		total = r2["bc"]
		_next = r2["next"]
		climit = commatize(limit)
		ctotal = commatize((total + 1))
		cnext = commatize(_next)
		puts(((((("First >= " + climit.padStart.call(18, " ")) + " is ") + ctotal.padStart.call(14, " ")) + " in the series: ") + cnext.padStart.call(18, " ")))
		k = (k + 1)
	end
end

$primes = primesUpTo(3200000)
