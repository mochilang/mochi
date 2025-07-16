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

def trimLeftZeros(s)
	i = 0
	while ((i < (s).length) && (_sliceString(s, i, (i + 1)) == "0"))
		i = (i + 1)
	end
	return _sliceString(s, i, (s).length)
end

def btString(s)
	s = trimLeftZeros(s)
	b = []
	i = ((s).length - 1)
	while (i >= 0)
		ch = _sliceString(s, i, (i + 1))
		if (ch == "+")
			b = (b + [1])
		else
			if (ch == "0")
				b = (b + [0])
			else
				if (ch == "-")
					b = (b + [(0 - 1)])
				else
					return {"bt" => [], "ok" => false}
				end
			end
		end
		i = (i - 1)
	end
	return {"bt" => b, "ok" => true}
end

def btToString(b)
	if ((b).length == 0)
		return "0"
	end
	r = ""
	i = ((b).length - 1)
	while (i >= 0)
		d = b[i]
		if (d == (0 - 1))
			r = (r + "-")
		else
			if (d == 0)
				r = (r + "0")
			else
				r = (r + "+")
			end
		end
		i = (i - 1)
	end
	return r
end

def btInt(i)
	if (i == 0)
		return []
	end
	n = i
	b = []
	while (n != 0)
		m = (n % 3)
		n = ((n / 3))
		if (m == 2)
			m = (0 - 1)
			n = (n + 1)
		else
			if (m == (0 - 2))
				m = 1
				n = (n - 1)
			end
		end
		b = (b + [m])
	end
	return b
end

def btToInt(b)
	r = 0
	pt = 1
	i = 0
	while (i < (b).length)
		r = (r + (b[i] * pt))
		pt = (pt * 3)
		i = (i + 1)
	end
	return r
end

def btNeg(b)
	r = []
	i = 0
	while (i < (b).length)
		r = (r + [(-b[i])])
		i = (i + 1)
	end
	return r
end

def btAdd(a, b)
	return btInt((btToInt(a) + btToInt(b)))
end

def btMul(a, b)
	return btInt((btToInt(a) * btToInt(b)))
end

def padLeft(s, w)
	r = s
	while ((r).length < w)
		r = (" " + r)
	end
	return r
end

def show(label, b)
	l = padLeft(label, 7)
	bs = padLeft(btToString(b), 12)
	is = padLeft((btToInt(b)).to_s, 7)
	puts(((((l + " ") + bs) + " ") + is))
end

def main()
	ares = btString("+-0++0+")
	a = ares["bt"]
	b = btInt((-436))
	cres = btString("+-++-")
	c = cres["bt"]
	show("a:", a)
	show("b:", b)
	show("c:", c)
	show("a(b-c):", btMul(a, btAdd(b, btNeg(c))))
end

main()
