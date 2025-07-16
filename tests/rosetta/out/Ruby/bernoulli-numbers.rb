def bernoulli(n)
	a = []
	m = 0
	while (m <= n)
		a = (a + [(1 / (((m + 1))))])
		j = m
		while (j >= 1)
			a[(j - 1)] = ((j) * ((a[(j - 1)] - a[j])))
			j = (j - 1)
		end
		m = (m + 1)
	end
	return a[0]
end

(0...61).each do |i|
	b = bernoulli(i)
	if (num.call(b) != 0)
		numStr = (num.call(b)).to_s
		denStr = (denom.call(b)).to_s
		puts(((((("B(" + (i).to_s.call(2, " ")) + ") =") + numStr.padStart.call(45, " ")) + "/") + denStr))
	end
end
