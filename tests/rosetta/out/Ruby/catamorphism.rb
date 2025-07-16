def add(a, b)
	return (a + b)
end

def sub(a, b)
	return (a - b)
end

def mul(a, b)
	return (a * b)
end

def fold(f, xs)
	r = xs[0]
	i = 1
	while (i < (xs).length)
		r = f.call(r, xs[i])
		i = (i + 1)
	end
	return r
end

$n = [1, 2, 3, 4, 5]
puts(fold(->(a, b){ add(a, b) }, $n))
puts(fold(->(a, b){ sub(a, b) }, $n))
puts(fold(->(a, b){ mul(a, b) }, $n))
