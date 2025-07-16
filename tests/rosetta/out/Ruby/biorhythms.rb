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

def floor(x)
	i = x
	if ((i) > x)
		i = (i - 1)
	end
	return i
end

def absFloat(x)
	if (x < 0.0)
		return (-x)
	end
	return x
end

def absInt(n)
	if (n < 0)
		return (-n)
	end
	return n
end

def parseIntStr(str)
	i = 0
	neg = false
	if (((str).length > 0) && (str[0...1] == "-"))
		neg = true
		i = 1
	end
	n = 0
	digits = {"0" => 0, "1" => 1, "2" => 2, "3" => 3, "4" => 4, "5" => 5, "6" => 6, "7" => 7, "8" => 8, "9" => 9}
	while (i < (str).length)
		n = ((n * 10) + digits[str[i...(i + 1)]])
		i = (i + 1)
	end
	if neg
		n = (-n)
	end
	return n
end

def parseDate(s)
	y = parseIntStr(s[0...4])
	m = parseIntStr(s[5...7])
	d = parseIntStr(s[8...10])
	return [y, m, d]
end

def leap(y)
	if ((y % 400) == 0)
		return true
	end
	if ((y % 100) == 0)
		return false
	end
	return ((y % 4) == 0)
end

def daysInMonth(y, m)
	feb = (leap(y) ? 29 : 28)
	lengths = [31, feb, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
	return lengths[(m - 1)]
end

def addDays(y, m, d, n)
	yy = y
	mm = m
	dd = d
	if (n >= 0)
		i = 0
		while (i < n)
			dd = (dd + 1)
			if (dd > daysInMonth(yy, mm))
				dd = 1
				mm = (mm + 1)
				if (mm > 12)
					mm = 1
					yy = (yy + 1)
				end
			end
			i = (i + 1)
		end
	else
		i = 0
		while (i > n)
			dd = (dd - 1)
			if (dd < 1)
				mm = (mm - 1)
				if (mm < 1)
					mm = 12
					yy = (yy - 1)
				end
				dd = daysInMonth(yy, mm)
			end
			i = (i - 1)
		end
	end
	return [yy, mm, dd]
end

def pad2(n)
	if (n < 10)
		return ("0" + (n).to_s)
	end
	return (n).to_s
end

def dateString(y, m, d)
	return (((((y).to_s + "-") + pad2(m)) + "-") + pad2(d))
end

def day(y, m, d)
	part1 = (367 * y)
	part2 = ((((7 * (((y + ((((m + 9)) / 12))))))) / 4))
	part3 = ((((275 * m)) / 9))
	return ((((part1 - part2) + part3) + d) - 730530)
end

def biorhythms(birth, target)
	bparts = parseDate(birth)
	by = bparts[0]
	bm = bparts[1]
	bd = bparts[2]
	tparts = parseDate(target)
	ty = tparts[0]
	tm = tparts[1]
	td = tparts[2]
	diff = absInt((day(ty, tm, td) - day(by, bm, bd)))
	puts(((("Born " + birth) + ", Target ") + target))
	puts(("Day " + (diff).to_s))
	cycles = ["Physical day ", "Emotional day", "Mental day   "]
	lengths = [23, 28, 33]
	quadrants = [["up and rising", "peak"], ["up but falling", "transition"], ["down and falling", "valley"], ["down but rising", "transition"]]
	i = 0
	while (i < 3)
		length = lengths[i]
		cycle = cycles[i]
		position = (diff % length)
		quadrant = (((position * 4)) / length)
		percent = sinApprox((((2.0 * $PI) * (position)) / (length)))
		percent = (floor((percent * 1000.0)) / 10.0)
		description = ""
		if (percent > 95.0)
			description = " peak"
		elsif (percent < ((-95.0)))
			description = " valley"
		elsif (absFloat(percent) < 5.0)
			description = " critical transition"
		else
			daysToAdd = (((((quadrant + 1)) * length) / 4) - position)
			res = addDays(ty, tm, td, daysToAdd)
			ny = res[0]
			nm = res[1]
			nd = res[2]
			transition = dateString(ny, nm, nd)
			trend = quadrants[quadrant][0]
			_next = quadrants[quadrant][1]
			pct = (percent).to_s
			if (!contains.call(pct, "."))
				pct = (pct + ".0")
			end
			description = ((((((((" " + pct) + "% (") + trend) + ", next ") + _next) + " ") + transition) + ")")
		end
		posStr = (position).to_s
		if (position < 10)
			posStr = (" " + posStr)
		end
		puts((((cycle + posStr) + " : ") + description))
		i = (i + 1)
	end
	puts("")
end

def main()
	pairs = [["1943-03-09", "1972-07-11"], ["1809-01-12", "1863-11-19"], ["1809-02-12", "1863-11-19"]]
	idx = 0
	while (idx < (pairs).length)
		p = pairs[idx]
		biorhythms(p[0], p[1])
		idx = (idx + 1)
	end
end

$PI = 3.141592653589793
$TWO_PI = 6.283185307179586
main()
