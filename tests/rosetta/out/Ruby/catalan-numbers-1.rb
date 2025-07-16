def binom(n, k)
	if ((k < 0) || (k > n))
		return 0
	end
	kk = k
	if (kk > (n - kk))
		kk = (n - kk)
	end
	res = 1
	i = 0
	while (i < kk)
		res = ((res * ((n - i))))
		i = (i + 1)
		res = ((res / i))
	end
	return res
end

def catalan(n)
	return ((binom((2 * n), n) / ((n + 1))))
end

def main()
	(0...15).each do |i|
		puts((catalan(i)).to_s)
	end
end

main()
