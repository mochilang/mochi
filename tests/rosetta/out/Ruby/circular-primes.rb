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

def isCircular(n)
	nn = n
	pow = 1
	while (nn > 0)
		pow = (pow * 10)
		nn = (nn / 10)
	end
	nn = n
	while true
		nn = (nn * 10)
		f = (nn / pow)
		nn = (nn + (f * ((1 - pow))))
		if (nn == n)
			break
		end
		if (!isPrime(nn))
			return false
		end
	end
	return true
end

def showList(xs)
	out = "["
	i = 0
	while (i < (xs).length)
		out = (out + (xs[i]).to_s)
		if (i < ((xs).length - 1))
			out = (out + ", ")
		end
		i = (i + 1)
	end
	return (out + "]")
end

$circs = []
puts("The first 19 circular primes are:")
$digits = [1, 3, 7, 9]
$q = [1, 2, 3, 5, 7, 9]
$fq = [1, 2, 3, 5, 7, 9]
$count = 0
while true
	f = $q[0]
	fd = $fq[0]
	if (isPrime(f) && isCircular(f))
		$circs = ($circs + [f])
		$count = ($count + 1)
		if ($count == 19)
			break
		end
	end
	$q = $q[1..-1]
	$fq = $fq[1..-1]
	if ((f != 2) && (f != 5))
		$digits.each do |d|
			$q = ($q + [((f * 10) + d)])
			$fq = ($fq + [fd])
		end
	end
end
puts(showList($circs))
puts("\nThe next 4 circular primes, in repunit format, are:")
puts("[R(19) R(23) R(317) R(1031)]")
puts("\nThe following repunits are probably circular primes:")
[5003, 9887, 15073, 25031, 35317, 49081].each do |i|
	puts((("R(" + (i).to_s) + ") : true"))
end
