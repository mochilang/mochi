def sameDigits(n, b)
	f = (n % b)
	n = ((n / b))
	while (n > 0)
		if ((n % b) != f)
			return false
		end
		n = ((n / b))
	end
	return true
end

def isBrazilian(n)
	if (n < 7)
		return false
	end
	if (((n % 2) == 0) && (n >= 8))
		return true
	end
	b = 2
	while (b < (n - 1))
		if sameDigits(n, b)
			return true
		end
		b = (b + 1)
	end
	return false
end

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

def main()
	kinds = [" ", " odd ", " prime "]
	kinds.each do |kind|
		puts((("First 20" + kind) + "Brazilian numbers:"))
		c = 0
		n = 7
		while true
			if isBrazilian(n)
				puts(((n).to_s + " "))
				c = (c + 1)
				if (c == 20)
					puts("\n")
					break
				end
			end
			if (kind == " ")
				n = (n + 1)
			elsif (kind == " odd ")
				n = (n + 2)
			else
				while true
					n = (n + 2)
					if isPrime(n)
						break
					end
				end
			end
		end
	end
	n = 7
	c = 0
	while (c < 100000)
		if isBrazilian(n)
			c = (c + 1)
		end
		n = (n + 1)
	end
	puts(("The 100,000th Brazilian number: " + ((n - 1)).to_s))
end

main()
