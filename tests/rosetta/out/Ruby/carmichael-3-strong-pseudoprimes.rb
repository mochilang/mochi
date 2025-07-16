def mod(n, m)
	return (((((n % m)) + m)) % m)
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

def pad(n, width)
	s = (n).to_s
	while ((s).length < width)
		s = (" " + s)
	end
	return s
end

def carmichael(p1)
	(2...p1).each do |h3|
		(1...((h3 + p1))).each do |d|
			if (((((((h3 + p1)) * ((p1 - 1)))) % d) == 0) && (mod(((-p1) * p1), h3) == (d % h3)))
				p2 = (1 + (((((p1 - 1)) * ((h3 + p1))) / d)))
				if (!isPrime(p2))
					next
				end
				p3 = (1 + (((p1 * p2) / h3)))
				if (!isPrime(p3))
					next
				end
				if ((((p2 * p3)) % ((p1 - 1))) != 1)
					next
				end
				c = ((p1 * p2) * p3)
				puts(((((((pad(p1, 2) + "   ") + pad(p2, 4)) + "   ") + pad(p3, 5)) + "     ") + (c).to_s))
			end
		end
	end
end

puts("The following are Carmichael munbers for p1 <= 61:\n")
puts("p1     p2      p3     product")
puts("==     ==      ==     =======")
(2...62).each do |p1|
	if isPrime(p1)
		carmichael(p1)
	end
end
