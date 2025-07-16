def initGrid(size)
	g = []
	y = 0
	while (y < size)
		row = []
		x = 0
		while (x < size)
			row = (row + [" "])
			x = (x + 1)
		end
		g = (g + [row])
		y = (y + 1)
	end
	return g
end

def set(g, x, y)
	if ((((x >= 0) && (x < (g[0]).length)) && (y >= 0)) && (y < (g).length))
		g[y][x] = "#"
	end
end

def circle(r)
	size = ((r * 2) + 1)
	g = initGrid(size)
	x = r
	y = 0
	err = (1 - r)
	while (y <= x)
		set(g, (r + x), (r + y))
		set(g, (r + y), (r + x))
		set(g, (r - x), (r + y))
		set(g, (r - y), (r + x))
		set(g, (r - x), (r - y))
		set(g, (r - y), (r - x))
		set(g, (r + x), (r - y))
		set(g, (r + y), (r - x))
		y = (y + 1)
		if (err < 0)
			err = ((err + (2 * y)) + 1)
		else
			x = (x - 1)
			err = ((err + (2 * ((y - x)))) + 1)
		end
	end
	return g
end

def trimRight(row)
	_end = (row).length
	while ((_end > 0) && (row[(_end - 1)] == " "))
		_end = (_end - 1)
	end
	s = ""
	i = 0
	while (i < _end)
		s = (s + row[i])
		i = (i + 1)
	end
	return s
end

$g = circle(10)
$g.each do |row|
	puts(trimRight(row))
end
