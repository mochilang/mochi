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

def Extent(b)
	return {"cols" => b.cols, "rows" => b.rows}
end

def Fill(b, p)
	y = 0
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

def FillRgb(b, c)
	Fill(b, pixelFromRgb(c))
end

def SetPx(b, x, y, p)
	if ((((x < 0) || (x >= b.cols)) || (y < 0)) || (y >= b.rows))
		return false
	end
	px = b.px
	row = px[y]
	row[x] = p
	px[y] = row
	b.px = px
	return true
end

def SetPxRgb(b, x, y, c)
	return SetPx(b, x, y, pixelFromRgb(c))
end

def GetPx(b, x, y)
	if ((((x < 0) || (x >= b.cols)) || (y < 0)) || (y >= b.rows))
		return {"ok" => false}
	end
	row = b.px[y]
	return {"ok" => true, "pixel" => row[x]}
end

def GetPxRgb(b, x, y)
	r = GetPx(b, x, y)
	if (!r.ok)
		return {"ok" => false}
	end
	return {"ok" => true, "rgb" => rgbFromPixel(r.pixel)}
end

def ppmSize(b)
	header = (((("P6\n# Creator: Rosetta Code http://rosettacode.org/\n" + (b.cols).to_s) + " ") + (b.rows).to_s) + "\n255\n")
	return ((header).length + ((3 * b.cols) * b.rows))
end

def pixelStr(p)
	return (((((("{" + (p.R).to_s) + " ") + (p.G).to_s) + " ") + (p.B).to_s) + "}")
end

def main()
	bm = NewBitmap(300, 240)
	FillRgb(bm, 16711680)
	SetPxRgb(bm, 10, 20, 255)
	SetPxRgb(bm, 20, 30, 0)
	SetPxRgb(bm, 30, 40, 1056816)
	c1 = GetPx(bm, 0, 0)
	c2 = GetPx(bm, 10, 20)
	c3 = GetPx(bm, 30, 40)
	puts(((("Image size: " + (bm.cols).to_s) + " × ") + (bm.rows).to_s))
	puts(((ppmSize(bm)).to_s + " bytes when encoded as PPM."))
	if c1.ok
		puts(("Pixel at (0,0) is " + pixelStr(c1.pixel)))
	end
	if c2.ok
		puts(("Pixel at (10,20) is " + pixelStr(c2.pixel)))
	end
	if c3.ok
		p = c3.pixel
		r16 = (p.R * 257)
		g16 = (p.G * 257)
		b16 = (p.B * 257)
		puts(((((("Pixel at (30,40) has R=" + (r16).to_s) + ", G=") + (g16).to_s) + ", B=") + (b16).to_s))
	end
end

main()
