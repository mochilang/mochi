def _sliceString(s, i, j)
  start = i
  finish = j
  chars = s.chars
  n = chars.length
  start += n if start < 0
  finish += n if finish < 0
  start = 0 if start < 0
  finish = n if finish > n
  finish = start if finish < start
  chars[start...finish].join
end

def sinApprox(x)
	term = x
	sum = x
	n = 1
	while (n <= 8)
		denom = ((((2 * n)) * (((2 * n) + 1))))
		term = ((((-term) * x) * x) / denom)
		sum = (sum + term)
		n = (n + 1)
	end
	return sum
end

def cosApprox(x)
	term = 1.0
	sum = 1.0
	n = 1
	while (n <= 8)
		denom = (((((2 * n) - 1)) * ((2 * n))))
		term = ((((-term) * x) * x) / denom)
		sum = (sum + term)
		n = (n + 1)
	end
	return sum
end

def atanApprox(x)
	if (x > 1.0)
		return (($PI / 2.0) - (x / (((x * x) + 0.28))))
	end
	if (x < ((-1.0)))
		return (((-$PI) / 2.0) - (x / (((x * x) + 0.28))))
	end
	return (x / ((1.0 + ((0.28 * x) * x))))
end

def atan2Approx(y, x)
	if (x > 0.0)
		return atanApprox((y / x))
	end
	if (x < 0.0)
		if (y >= 0.0)
			return (atanApprox((y / x)) + $PI)
		end
		return (atanApprox((y / x)) - $PI)
	end
	if (y > 0.0)
		return ($PI / 2.0)
	end
	if (y < 0.0)
		return ((-$PI) / 2.0)
	end
	return 0.0
end

def digit(ch)
	digits = "0123456789"
	i = 0
	while (i < (digits).length)
		if (_sliceString(digits, i, (i + 1)) == ch)
			return i
		end
		i = (i + 1)
	end
	return 0
end

def parseTwo(s, idx)
	return ((digit(_sliceString(s, idx, (idx + 1))) * 10) + digit(_sliceString(s, (idx + 1), (idx + 2))))
end

def parseSec(s)
	h = parseTwo(s, 0)
	m = parseTwo(s, 3)
	sec = parseTwo(s, 6)
	return ((((((h * 60) + m)) * 60) + sec))
end

def pad(n)
	if (n < 10)
		return ("0" + (n).to_s)
	end
	return (n).to_s
end

def meanTime(times)
	ssum = 0.0
	csum = 0.0
	i = 0
	while (i < (times).length)
		sec = parseSec(times[i])
		ang = (((sec * 2.0) * $PI) / 86400.0)
		ssum = (ssum + sinApprox(ang))
		csum = (csum + cosApprox(ang))
		i = (i + 1)
	end
	theta = atan2Approx(ssum, csum)
	frac = (theta / ((2.0 * $PI)))
	while (frac < 0.0)
		frac = (frac + 1.0)
	end
	total = (frac * 86400.0)
	si = total
	h = ((si / 3600))
	m = ((((si % 3600)) / 60))
	s = ((si % 60))
	return ((((pad(h) + ":") + pad(m)) + ":") + pad(s))
end

def main()
	inputs = ["23:00:17", "23:40:20", "00:12:45", "00:17:19"]
	puts(meanTime(inputs))
end

$PI = 3.141592653589793
main()
