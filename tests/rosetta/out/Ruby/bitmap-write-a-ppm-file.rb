Colour = Struct.new(:R, :G, :B, keyword_init: true)

Bitmap = Struct.new(:width, :height, :pixels, keyword_init: true)

def newBitmap(w, h, c)
	rows = []
	y = 0
	while (y < h)
		row = []
		x = 0
		while (x < w)
			row = (row + [c])
			x = (x + 1)
		end
		rows = (rows + [row])
		y = (y + 1)
	end
	return Bitmap.new(width: w, height: h, pixels: rows)
end

def setPixel(b, x, y, c)
	rows = b.pixels
	row = rows[y]
	row[x] = c
	rows[y] = row
	b.pixels = rows
end

def fillRect(b, x, y, w, h, c)
	yy = y
	while (yy < (y + h))
		xx = x
		while (xx < (x + w))
			setPixel(b, xx, yy, c)
			xx = (xx + 1)
		end
		yy = (yy + 1)
	end
end

def pad(n, width)
	s = (n).to_s
	while ((s).length < width)
		s = (" " + s)
	end
	return s
end

def writePPMP3(b)
	maxv = 0
	y = 0
	while (y < b.height)
		x = 0
		while (x < b.width)
			p = b.pixels[y][x]
			if (p.R > maxv)
				maxv = p.R
			end
			if (p.G > maxv)
				maxv = p.G
			end
			if (p.B > maxv)
				maxv = p.B
			end
			x = (x + 1)
		end
		y = (y + 1)
	end
	out = (((((("P3\n# generated from Bitmap.writeppmp3\n" + (b.width).to_s) + " ") + (b.height).to_s) + "\n") + (maxv).to_s) + "\n")
	numsize = ((maxv).to_s).length
	y = (b.height - 1)
	while (y >= 0)
		line = ""
		x = 0
		while (x < b.width)
			p = b.pixels[y][x]
			line = ((((((line + "   ") + pad(p.R, numsize)) + " ") + pad(p.G, numsize)) + " ") + pad(p.B, numsize))
			x = (x + 1)
		end
		out = (out + line)
		if (y > 0)
			out = (out + "\n")
		else
			out = (out + "\n")
		end
		y = (y - 1)
	end
	return out
end

def main()
	black = Colour.new(R: 0, G: 0, B: 0)
	white = Colour.new(R: 255, G: 255, B: 255)
	bm = newBitmap(4, 4, black)
	fillRect(bm, 1, 0, 1, 2, white)
	setPixel(bm, 3, 3, Colour.new(R: 127, G: 0, B: 63))
	ppm = writePPMP3(bm)
	puts(ppm)
end

main()
