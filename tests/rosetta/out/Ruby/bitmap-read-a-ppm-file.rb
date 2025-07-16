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

Pixel = Struct.new(:R, :G, :B, keyword_init: true)

Bitmap = Struct.new(:w, :h, :max, :data, keyword_init: true)

def newBitmap(w, h, max)
	rows = []
	y = 0
	while (y < h)
		row = []
		x = 0
		while (x < w)
			row = (row + [Pixel.new(R: 0, G: 0, B: 0)])
			x = (x + 1)
		end
		rows = (rows + [row])
		y = (y + 1)
	end
	return Bitmap.new(w: w, h: h, max: max, data: rows)
end

def setPx(b, x, y, p)
	rows = b.data
	row = rows[y]
	row[x] = p
	rows[y] = row
	b.data = rows
end

def getPx(b, x, y)
	return b.data[y][x]
end

def splitLines(s)
	out = []
	cur = ""
	i = 0
	while (i < (s).length)
		ch = _sliceString(s, i, (i + 1))
		if (ch == "\n")
			out = (out + [cur])
			cur = ""
		else
			cur = (cur + ch)
		end
		i = (i + 1)
	end
	out = (out + [cur])
	return out
end

def splitWS(s)
	out = []
	cur = ""
	i = 0
	while (i < (s).length)
		ch = _sliceString(s, i, (i + 1))
		if ((((ch == " ") || (ch == "\t")) || (ch == "\r")) || (ch == "\n"))
			if ((cur).length > 0)
				out = (out + [cur])
				cur = ""
			end
		else
			cur = (cur + ch)
		end
		i = (i + 1)
	end
	if ((cur).length > 0)
		out = (out + [cur])
	end
	return out
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

def tokenize(s)
	lines = splitLines(s)
	toks = []
	i = 0
	while (i < (lines).length)
		line = lines[i]
		if (((line).length > 0) && (_sliceString(line, 0, 1) == "#"))
			i = (i + 1)
			next
		end
		parts = splitWS(line)
		j = 0
		while (j < (parts).length)
			toks = (toks + [parts[j]])
			j = (j + 1)
		end
		i = (i + 1)
	end
	return toks
end

def readP3(text)
	toks = tokenize(text)
	if ((toks).length < 4)
		return newBitmap(0, 0, 0)
	end
	if (toks[0] != "P3")
		return newBitmap(0, 0, 0)
	end
	w = parseIntStr(toks[1])
	h = parseIntStr(toks[2])
	maxv = parseIntStr(toks[3])
	idx = 4
	bm = newBitmap(w, h, maxv)
	y = (h - 1)
	while (y >= 0)
		x = 0
		while (x < w)
			r = parseIntStr(toks[idx])
			g = parseIntStr(toks[(idx + 1)])
			b = parseIntStr(toks[(idx + 2)])
			setPx(bm, x, y, Pixel.new(R: r, G: g, B: b))
			idx = (idx + 3)
			x = (x + 1)
		end
		y = (y - 1)
	end
	return bm
end

def toGrey(b)
	h = b.h
	w = b.w
	m = 0
	y = 0
	while (y < h)
		x = 0
		while (x < w)
			p = getPx(b, x, y)
			l = (((((p.R * 2126) + (p.G * 7152)) + (p.B * 722))) / 10000)
			if (l > b.max)
				l = b.max
			end
			setPx(b, x, y, Pixel.new(R: l, G: l, B: l))
			if (l > m)
				m = l
			end
			x = (x + 1)
		end
		y = (y + 1)
	end
	b.max = m
end

def pad(n, w)
	s = (n).to_s
	while ((s).length < w)
		s = (" " + s)
	end
	return s
end

def writeP3(b)
	h = b.h
	w = b.w
	max = b.max
	digits = ((max).to_s).length
	out = (((((("P3\n# generated from Bitmap.writeppmp3\n" + (w).to_s) + " ") + (h).to_s) + "\n") + (max).to_s) + "\n")
	y = (h - 1)
	while (y >= 0)
		line = ""
		x = 0
		while (x < w)
			p = getPx(b, x, y)
			line = ((((((line + "   ") + pad(p.R, digits)) + " ") + pad(p.G, digits)) + " ") + pad(p.B, digits))
			x = (x + 1)
		end
		out = ((out + line) + "\n")
		y = (y - 1)
	end
	return out
end

$ppmtxt = ((((((("P3\n" + "# feep.ppm\n") + "4 4\n") + "15\n") + " 0  0  0    0  0  0    0  0  0   15  0 15\n") + " 0  0  0    0 15  7    0  0  0    0  0  0\n") + " 0  0  0    0  0  0    0 15  7    0  0  0\n") + "15  0 15    0  0  0    0  0  0    0  0  0\n")
puts("Original Colour PPM file")
puts($ppmtxt)
$bm = readP3($ppmtxt)
puts("Grey PPM:")
toGrey($bm)
$out = writeP3($bm)
puts($out)
