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

def indexOf(s, ch)
	i = 0
	while (i < (s).length)
		if (_sliceString(s, i, (i + 1)) == ch)
			return i
		end
		i = (i + 1)
	end
	return (-1)
end

def fmt3(x)
	y = (((((x * 1000.0)) + 0.5)) / 1000.0)
	s = (y).to_s
	dot = indexOf(s, ".")
	if (dot == (0 - 1))
		s = (s + ".000")
	else
		decs = (((s).length - dot) - 1)
		if (decs > 3)
			s = _sliceString(s, 0, (dot + 4))
		else
			while (decs < 3)
				s = (s + "0")
				decs = (decs + 1)
			end
		end
	end
	return s
end

def pad(s, width)
	out = s
	while ((out).length < width)
		out = (" " + out)
	end
	return out
end

def smaSeries(xs, period)
	res = []
	sum = 0.0
	i = 0
	while (i < (xs).length)
		sum = (sum + xs[i])
		if (i >= period)
			sum = (sum - xs[(i - period)])
		end
		denom = (i + 1)
		if (denom > period)
			denom = period
		end
		res = (res + [(sum / (denom))])
		i = (i + 1)
	end
	return res
end

def main()
	xs = [1.0, 2.0, 3.0, 4.0, 5.0, 5.0, 4.0, 3.0, 2.0, 1.0]
	sma3 = smaSeries(xs, 3)
	sma5 = smaSeries(xs, 5)
	puts("x       sma3   sma5")
	i = 0
	while (i < (xs).length)
		line = ((((pad(fmt3(xs[i]), 5) + "  ") + pad(fmt3(sma3[i]), 5)) + "  ") + pad(fmt3(sma5[i]), 5))
		puts(line)
		i = (i + 1)
	end
end

main()
