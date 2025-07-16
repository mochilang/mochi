Pixel = Struct.new(:R, :G, :B, keyword_init: true)

Bitmap = Struct.new(:cols, :rows, :px, keyword_init: true)

def pixelFromRgb(c)
	r = ((((c / 65536))) % 256)
	g = ((((c / 256))) % 256)
	b = (c % 256)
	return Pixel.new(R: r, G: g, B: b)
end

def rgbFromPixel(p)
	return (((p.R * 65536) + (p.G * 256)) + p.B)
end

def NewBitmap(x, y)
	data = []
	row = 0
	while (row < y)
		r = []
		col = 0
		while (col < x)
			r = (r + [Pixel.new(R: 0, G: 0, B: 0)])
			col = (col + 1)
		end
		data = (data + [r])
		row = (row + 1)
	end
	return Bitmap.new(cols: x, rows: y, px: data)
end

def FillRgb(b, c)
	y = 0
	p = pixelFromRgb(c)
	while (y < b.rows)
		x = 0
		while (x < b.cols)
			px = b.px
			row = px[y]
			row[x] = p
			px[y] = row
			b.px = px
			x = (x + 1)
		end
		y = (y + 1)
	end
end

def SetPxRgb(b, x, y, c)
	if ((((x < 0) || (x >= b.cols)) || (y < 0)) || (y >= b.rows))
		return false
	end
	px = b.px
	row = px[y]
	row[x] = pixelFromRgb(c)
	px[y] = row
	b.px = px
	return true
end

def nextRand(seed)
	return ((((seed * 1664525) + 1013904223)) % 2147483648)
end

def main()
	bm = NewBitmap(400, 300)
	FillRgb(bm, 12615744)
	seed = now.call()
	i = 0
	while (i < 2000)
		seed = nextRand(seed)
		x = (seed % 400)
		seed = nextRand(seed)
		y = (seed % 300)
		SetPxRgb(bm, x, y, 8405024)
		i = (i + 1)
	end
	x = 0
	while (x < 400)
		y = 240
		while (y < 245)
			SetPxRgb(bm, x, y, 8405024)
			y = (y + 1)
		end
		y = 260
		while (y < 265)
			SetPxRgb(bm, x, y, 8405024)
			y = (y + 1)
		end
		x = (x + 1)
	end
	y = 0
	while (y < 300)
		x = 80
		while (x < 85)
			SetPxRgb(bm, x, y, 8405024)
			x = (x + 1)
		end
		x = 95
		while (x < 100)
			SetPxRgb(bm, x, y, 8405024)
			x = (x + 1)
		end
		y = (y + 1)
	end
end

main()
