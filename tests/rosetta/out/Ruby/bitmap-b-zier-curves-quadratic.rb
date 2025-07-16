Pixel = Struct.new(:r, :g, :b, keyword_init: true)

def pixelFromRgb(rgb)
	r = ((((rgb / 65536)) % 256))
	g = ((((rgb / 256)) % 256))
	b = ((rgb % 256))
	return Pixel.new(r: r, g: g, b: b)
end

def newBitmap(cols, rows)
	d = []
	y = 0
	while (y < rows)
		row = []
		x = 0
		while (x < cols)
			row = (row + [Pixel.new(r: 0, g: 0, b: 0)])
			x = (x + 1)
		end
		d = (d + [row])
		y = (y + 1)
	end
	return {"cols" => cols, "rows" => rows, "data" => d}
end

def setPx(b, x, y, p)
	cols = b["cols"]
	rows = b["rows"]
	if ((((x >= 0) && (x < cols)) && (y >= 0)) && (y < rows))
		b["data"][y][x] = p
	end
end

def fill(b, p)
	cols = b["cols"]
	rows = b["rows"]
	y = 0
	while (y < rows)
		x = 0
		while (x < cols)
			b["data"][y][x] = p
			x = (x + 1)
		end
		y = (y + 1)
	end
end

def fillRgb(b, rgb)
	fill(b, pixelFromRgb(rgb))
end

def line(b, x0, y0, x1, y1, p)
	dx = (x1 - x0)
	if (dx < 0)
		dx = (-dx)
	end
	dy = (y1 - y0)
	if (dy < 0)
		dy = (-dy)
	end
	sx = (-1)
	if (x0 < x1)
		sx = 1
	end
	sy = (-1)
	if (y0 < y1)
		sy = 1
	end
	err = (dx - dy)
	while true
		setPx(b, x0, y0, p)
		if ((x0 == x1) && (y0 == y1))
			break
		end
		e2 = (2 * err)
		if (e2 > ((0 - dy)))
			err = (err - dy)
			x0 = (x0 + sx)
		end
		if (e2 < dx)
			err = (err + dx)
			y0 = (y0 + sy)
		end
	end
end

def bezier2(b, x1, y1, x2, y2, x3, y3, p)
	px = []
	py = []
	i = 0
	while (i <= $b2Seg)
		px = (px + [0])
		py = (py + [0])
		i = (i + 1)
	end
	fx1 = x1
	fy1 = y1
	fx2 = x2
	fy2 = y2
	fx3 = x3
	fy3 = y3
	i = 0
	while (i <= $b2Seg)
		c = ((i) / ($b2Seg))
		a = (1.0 - c)
		a2 = (a * a)
		b2 = ((2.0 * c) * a)
		c2 = (c * c)
		px[i] = ((((a2 * fx1) + (b2 * fx2)) + (c2 * fx3)))
		py[i] = ((((a2 * fy1) + (b2 * fy2)) + (c2 * fy3)))
		i = (i + 1)
	end
	x0 = px[0]
	y0 = py[0]
	i = 1
	while (i <= $b2Seg)
		x = px[i]
		y = py[i]
		line(b, x0, y0, x, y, p)
		x0 = x
		y0 = y
		i = (i + 1)
	end
end

$b2Seg = 20
$b = newBitmap(400, 300)
fillRgb($b, 14614575)
bezier2($b, 20, 150, 500, (-100), 300, 280, pixelFromRgb(4165615))
