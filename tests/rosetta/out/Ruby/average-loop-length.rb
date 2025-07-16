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

def absf(x)
	if (x < 0.0)
		return (-x)
	end
	return x
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

def fmtF(x)
	y = (floorf(((x * 10000.0) + 0.5)) / 10000.0)
	s = (y).to_s
	dot = indexOf(s, ".")
	if (dot == (0 - 1))
		s = (s + ".0000")
	else
		decs = (((s).length - dot) - 1)
		if (decs > 4)
			s = _sliceString(s, 0, (dot + 5))
		else
			while (decs < 4)
				s = (s + "0")
				decs = (decs + 1)
			end
		end
	end
	return s
end

def padInt(n, width)
	s = (n).to_s
	while ((s).length < width)
		s = (" " + s)
	end
	return s
end

def padFloat(x, width)
	s = fmtF(x)
	while ((s).length < width)
		s = (" " + s)
	end
	return s
end

def avgLen(n)
	tests = 10000
	sum = 0
	seed = 1
	t = 0
	while (t < tests)
		visited = []
		i = 0
		while (i < n)
			visited = (visited + [false])
			i = (i + 1)
		end
		x = 0
		while (!visited[x])
			visited[x] = true
			sum = (sum + 1)
			seed = ((((seed * 1664525) + 1013904223)) % 2147483647)
			x = (seed % n)
		end
		t = (t + 1)
	end
	return ((sum) / tests)
end

def ana(n)
	nn = n
	term = 1.0
	sum = 1.0
	i = (nn - 1.0)
	while (i >= 1.0)
		term = (term * ((i / nn)))
		sum = (sum + term)
		i = (i - 1.0)
	end
	return sum
end

def main()
	nmax = 20
	puts(" N    average    analytical    (error)")
	puts("===  =========  ============  =========")
	n = 1
	while (n <= nmax)
		a = avgLen(n)
		b = ana(n)
		err = ((absf((a - b)) / b) * 100.0)
		line = (((((((padInt(n, 3) + "  ") + padFloat(a, 9)) + "  ") + padFloat(b, 12)) + "  (") + padFloat(err, 6)) + "%)")
		puts(line)
		n = (n + 1)
	end
end

main()
