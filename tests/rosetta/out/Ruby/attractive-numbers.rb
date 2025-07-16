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

def countPrimeFactors(n)
	if (n == 1)
		return 0
	end
	if isPrime(n)
		return 1
	end
	count = 0
	f = 2
	while true
		if ((n % f) == 0)
			count = (count + 1)
			n = (n / f)
			if (n == 1)
				return count
			end
			if isPrime(n)
				f = n
			end
		elsif (f >= 3)
			f = (f + 2)
		else
			f = 3
		end
	end
	return count
end

def pad4(n)
	s = (n).to_s
	while ((s).length < 4)
		s = (" " + s)
	end
	return s
end

def main()
	max = 120
	puts((("The attractive numbers up to and including " + (max).to_s) + " are:"))
	count = 0
	line = ""
	lineCount = 0
	i = 1
	while (i <= max)
		c = countPrimeFactors(i)
		if isPrime(c)
			line = (line + pad4(i))
			count = (count + 1)
			lineCount = (lineCount + 1)
			if (lineCount == 20)
				puts(line)
				line = ""
				lineCount = 0
			end
		end
		i = (i + 1)
	end
	if (lineCount > 0)
		puts(line)
	end
end

main()
