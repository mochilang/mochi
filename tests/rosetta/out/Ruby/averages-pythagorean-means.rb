def powf(base, exp)
	result = 1.0
	i = 0
	while (i < exp)
		result = (result * base)
		i = (i + 1)
	end
	return result
end

def nthRoot(x, n)
	low = 0.0
	high = x
	i = 0
	while (i < 60)
		mid = (((low + high)) / 2.0)
		if (powf(mid, n) > x)
			high = mid
		else
			low = mid
		end
		i = (i + 1)
	end
	return low
end

def main()
	sum = 0.0
	sumRecip = 0.0
	prod = 1.0
	n = 1
	while (n <= 10)
		f = n
		sum = (sum + f)
		sumRecip = (sumRecip + (1.0 / f))
		prod = (prod * f)
		n = (n + 1)
	end
	count = 10.0
	a = (sum / count)
	g = nthRoot(prod, 10)
	h = (count / sumRecip)
	puts(((((("A: " + (a).to_s) + " G: ") + (g).to_s) + " H: ") + (h).to_s))
	puts(("A >= G >= H: " + (((a >= g) && (g >= h))).to_s))
end

main()
