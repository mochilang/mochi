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

def floorf(x)
	return (x)
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

def fmtF3(x)
	y = (floorf(((x * 1000.0) + 0.5)) / 1000.0)
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

def padFloat3(x, width)
	s = fmtF3(x)
	while ((s).length < width)
		s = (" " + s)
	end
	return s
end

def fib1000()
	a = 0.0
	b = 1.0
	res = []
	i = 0
	while (i < 1000)
		res = (res + [b])
		t = b
		b = (b + a)
		a = t
		i = (i + 1)
	end
	return res
end

def leadingDigit(x)
	if (x < 0.0)
		x = (-x)
	end
	while (x >= 10.0)
		x = (x / 10.0)
	end
	while ((x > 0.0) && (x < 1.0))
		x = (x * 10.0)
	end
	return x
end

def show(nums, title)
	counts = [0, 0, 0, 0, 0, 0, 0, 0, 0]
	nums.each do |n|
		d = leadingDigit(n)
		if ((d >= 1) && (d <= 9))
			counts[(d - 1)] = (counts[(d - 1)] + 1)
		end
	end
	preds = [0.301, 0.176, 0.125, 0.097, 0.079, 0.067, 0.058, 0.051, 0.046]
	total = (nums).length
	puts(title)
	puts("Digit  Observed  Predicted")
	i = 0
	while (i < 9)
		obs = ((counts[i]) / (total))
		line = ((((("  " + ((i + 1)).to_s) + "  ") + padFloat3(obs, 9)) + "  ") + padFloat3(preds[i], 8))
		puts(line)
		i = (i + 1)
	end
end

def main()
	show(fib1000(), "First 1000 Fibonacci numbers")
end

main()
