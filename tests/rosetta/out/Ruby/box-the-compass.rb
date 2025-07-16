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

def padLeft(s, w)
	res = ""
	n = (w - (s).length)
	while (n > 0)
		res = (res + " ")
		n = (n - 1)
	end
	return (res + s)
end

def padRight(s, w)
	out = s
	i = (s).length
	while (i < w)
		out = (out + " ")
		i = (i + 1)
	end
	return out
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

def format2(f)
	s = (f).to_s
	idx = indexOf(s, ".")
	if (idx < 0)
		s = (s + ".00")
	else
		need = (idx + 3)
		if ((s).length > need)
			s = _sliceString(s, 0, need)
		else
			while ((s).length < need)
				s = (s + "0")
			end
		end
	end
	return s
end

def cpx(h)
	x = ((((h / 11.25)) + 0.5))
	x = (x % 32)
	if (x < 0)
		x = (x + 32)
	end
	return x
end

def degrees2compasspoint(h)
	return $compassPoint[cpx(h)]
end

$compassPoint = ["North", "North by east", "North-northeast", "Northeast by north", "Northeast", "Northeast by east", "East-northeast", "East by north", "East", "East by south", "East-southeast", "Southeast by east", "Southeast", "Southeast by south", "South-southeast", "South by east", "South", "South by west", "South-southwest", "Southwest by south", "Southwest", "Southwest by west", "West-southwest", "West by south", "West", "West by north", "West-northwest", "Northwest by west", "Northwest", "Northwest by north", "North-northwest", "North by west"]
$headings = [0.0, 16.87, 16.88, 33.75, 50.62, 50.63, 67.5, 84.37, 84.38, 101.25, 118.12, 118.13, 135.0, 151.87, 151.88, 168.75, 185.62, 185.63, 202.5, 219.37, 219.38, 236.25, 253.12, 253.13, 270.0, 286.87, 286.88, 303.75, 320.62, 320.63, 337.5, 354.37, 354.38]
puts("Index  Compass point         Degree")
$i = 0
while ($i < ($headings).length)
	h = $headings[$i]
	idx = (($i % 32) + 1)
	cp = degrees2compasspoint(h)
	puts((((((padLeft((idx).to_s, 4) + "   ") + padRight(cp, 19)) + " ") + format2(h)) + "°"))
	$i = ($i + 1)
end
