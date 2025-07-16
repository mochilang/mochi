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

def parseIntBase(s, base)
	digits = "0123456789abcdefghijklmnopqrstuvwxyz"
	n = 0
	i = 0
	while (i < (s).length)
		j = 0
		v = 0
		while (j < (digits).length)
			if (_sliceString(digits, j, (j + 1)) == s[i...(i + 1)])
				v = j
				break
			end
			j = (j + 1)
		end
		n = ((n * base) + v)
		i = (i + 1)
	end
	return n
end

def intToBase(n, base)
	digits = "0123456789abcdefghijklmnopqrstuvwxyz"
	if (n == 0)
		return "0"
	end
	out = ""
	v = n
	while (v > 0)
		d = (v % base)
		out = (_sliceString(digits, d, (d + 1)) + out)
		v = (v / base)
	end
	return out
end

def subset(base, _begin, _end)
	b = parseIntBase(_begin, base)
	e = parseIntBase(_end, base)
	out = []
	k = b
	while (k <= e)
		ks = intToBase(k, base)
		mod = (base - 1)
		r1 = (parseIntBase(ks, base) % mod)
		r2 = (((parseIntBase(ks, base) * parseIntBase(ks, base))) % mod)
		if (r1 == r2)
			out = (out + [ks])
		end
		k = (k + 1)
	end
	return out
end

$testCases = [{"base" => 10, "begin" => "1", "end" => "100", "kaprekar" => ["1", "9", "45", "55", "99"]}, {"base" => 17, "begin" => "10", "end" => "gg", "kaprekar" => ["3d", "d4", "gg"]}]
$idx = 0
while ($idx < ($testCases).length)
	tc = $testCases[$idx]
	puts((((((("\nTest case base = " + (tc["base"]).to_s) + ", begin = ") + tc["begin"]) + ", end = ") + tc["end"]) + ":"))
	s = subset(tc["base"], tc["begin"], tc["end"])
	puts(("Subset:  " + (s).to_s))
	puts(("Kaprekar:" + (tc["kaprekar"]).to_s))
	sx = 0
	valid = true
	i = 0
	while (i < (tc["kaprekar"]).length)
		k = tc["kaprekar"][i]
		found = false
		while (sx < (s).length)
			if (s[sx] == k)
				found = true
				sx = (sx + 1)
				break
			end
			sx = (sx + 1)
		end
		if (!found)
			puts((("Fail:" + k) + " not in subset"))
			valid = false
			break
		end
		i = (i + 1)
	end
	if valid
		puts("Valid subset.")
	end
	$idx = ($idx + 1)
end
