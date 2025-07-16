def image()
	return [[0, 0, 10000], [65535, 65535, 65535], [65535, 65535, 65535]]
end

def histogram(g, bins)
	if (bins <= 0)
		bins = (g[0]).length
	end
	h = []
	i = 0
	while (i < bins)
		h = (h + [0])
		i = (i + 1)
	end
	y = 0
	while (y < (g).length)
		row = g[y]
		x = 0
		while (x < (row).length)
			p = row[x]
			idx = ((((p * ((bins - 1)))) / 65535))
			h[idx] = (h[idx] + 1)
			x = (x + 1)
		end
		y = (y + 1)
	end
	return h
end

def medianThreshold(h)
	lb = 0
	ub = ((h).length - 1)
	lSum = 0
	uSum = 0
	while (lb <= ub)
		if ((lSum + h[lb]) < (uSum + h[ub]))
			lSum = (lSum + h[lb])
			lb = (lb + 1)
		else
			uSum = (uSum + h[ub])
			ub = (ub - 1)
		end
	end
	return ((((ub * 65535)) / (h).length))
end

def threshold(g, t)
	out = []
	y = 0
	while (y < (g).length)
		row = g[y]
		newRow = []
		x = 0
		while (x < (row).length)
			if (row[x] < t)
				newRow = (newRow + [0])
			else
				newRow = (newRow + [65535])
			end
			x = (x + 1)
		end
		out = (out + [newRow])
		y = (y + 1)
	end
	return out
end

def printImage(g)
	y = 0
	while (y < (g).length)
		row = g[y]
		line = ""
		x = 0
		while (x < (row).length)
			if (row[x] == 0)
				line = (line + "0")
			else
				line = (line + "1")
			end
			x = (x + 1)
		end
		puts(line)
		y = (y + 1)
	end
end

def main()
	img = image()
	h = histogram(img, 0)
	puts(("Histogram: " + (h).to_s))
	t = medianThreshold(h)
	puts(("Threshold: " + (t).to_s))
	bw = threshold(img, t)
	printImage(bw)
end

main()
