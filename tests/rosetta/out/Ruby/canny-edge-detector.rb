def conv2d(img, k)
	h = (img).length
	w = (img[0]).length
	n = (k).length
	half = (n / 2)
	out = []
	y = 0
	while (y < h)
		row = []
		x = 0
		while (x < w)
			sum = 0.0
			j = 0
			while (j < n)
				i = 0
				while (i < n)
					yy = ((y + j) - half)
					if (yy < 0)
						yy = 0
					end
					if (yy >= h)
						yy = (h - 1)
					end
					xx = ((x + i) - half)
					if (xx < 0)
						xx = 0
					end
					if (xx >= w)
						xx = (w - 1)
					end
					sum = (sum + (img[yy][xx] * k[j][i]))
					i = (i + 1)
				end
				j = (j + 1)
			end
			row = (row + [sum])
			x = (x + 1)
		end
		out = (out + [row])
		y = (y + 1)
	end
	return out
end

def gradient(img)
	hx = [[(-1.0), 0.0, 1.0], [(-2.0), 0.0, 2.0], [(-1.0), 0.0, 1.0]]
	hy = [[1.0, 2.0, 1.0], [0.0, 0.0, 0.0], [(-1.0), (-2.0), (-1.0)]]
	gx = conv2d(img, hx)
	gy = conv2d(img, hy)
	h = (img).length
	w = (img[0]).length
	out = []
	y = 0
	while (y < h)
		row = []
		x = 0
		while (x < w)
			g = ((gx[y][x] * gx[y][x]) + (gy[y][x] * gy[y][x]))
			row = (row + [g])
			x = (x + 1)
		end
		out = (out + [row])
		y = (y + 1)
	end
	return out
end

def threshold(g, t)
	h = (g).length
	w = (g[0]).length
	out = []
	y = 0
	while (y < h)
		row = []
		x = 0
		while (x < w)
			if (g[y][x] >= t)
				row = (row + [1])
			else
				row = (row + [0])
			end
			x = (x + 1)
		end
		out = (out + [row])
		y = (y + 1)
	end
	return out
end

def printMatrix(m)
	y = 0
	while (y < (m).length)
		line = ""
		x = 0
		while (x < (m[0]).length)
			line = (line + (m[y][x]).to_s)
			if (x < ((m[0]).length - 1))
				line = (line + " ")
			end
			x = (x + 1)
		end
		puts(line)
		y = (y + 1)
	end
end

def main()
	img = [[0.0, 0.0, 0.0, 0.0, 0.0], [0.0, 255.0, 255.0, 255.0, 0.0], [0.0, 255.0, 255.0, 255.0, 0.0], [0.0, 255.0, 255.0, 255.0, 0.0], [0.0, 0.0, 0.0, 0.0, 0.0]]
	g = gradient(img)
	edges = threshold(g, (1020.0 * 1020.0))
	printMatrix(edges)
end

$PI = 3.141592653589793
main()
