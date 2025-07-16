def egcd(a, b)
	if (a == 0)
		return [b, 0, 1]
	end
	res = egcd((b % a), a)
	g = res[0]
	x1 = res[1]
	y1 = res[2]
	return [g, (y1 - (((b / a)) * x1)), x1]
end

def modInv(a, m)
	r = egcd(a, m)
	if (r[0] != 1)
		return 0
	end
	x = r[1]
	if (x < 0)
		return (x + m)
	end
	return x
end

def crt(a, n)
	prod = 1
	i = 0
	while (i < (n).length)
		prod = (prod * n[i])
		i = (i + 1)
	end
	x = 0
	i = 0
	while (i < (n).length)
		ni = n[i]
		ai = a[i]
		p = (prod / ni)
		inv = modInv((p % ni), ni)
		x = (x + ((ai * inv) * p))
		i = (i + 1)
	end
	return (x % prod)
end

$n = [3, 5, 7]
$a = [2, 3, 2]
$res = crt($a, $n)
puts((($res).to_s + " <nil>"))
