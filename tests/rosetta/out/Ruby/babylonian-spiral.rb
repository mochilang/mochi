def push(h, it)
	h = (h + [it])
	i = ((h).length - 1)
	while ((i > 0) && (h[(i - 1)]["s"] > h[i]["s"]))
		tmp = h[(i - 1)]
		h[(i - 1)] = h[i]
		h[i] = tmp
		i = (i - 1)
	end
	return h
end

def step(h, nv, dir)
	while (((h).length == 0) || ((nv * nv) <= h[0]["s"]))
		h = push(h, {"s" => (nv * nv), "a" => nv, "b" => 0})
		nv = (nv + 1)
	end
	s = h[0]["s"]
	v = []
	while (((h).length > 0) && (h[0]["s"] == s))
		it = h[0]
		h = h[1..-1]
		v = (v + [[it["a"], it["b"]]])
		if (it["a"] > it["b"])
			h = push(h, {"s" => ((it["a"] * it["a"]) + (((it["b"] + 1)) * ((it["b"] + 1)))), "a" => it["a"], "b" => (it["b"] + 1)})
		end
	end
	list = []
	v.each do |p|
		list = (list + [p])
	end
	temp = list
	temp.each do |p|
		if (p[0] != p[1])
			list = (list + [[p[1], p[0]]])
		end
	end
	temp = list
	temp.each do |p|
		if (p[1] != 0)
			list = (list + [[p[0], (-p[1])]])
		end
	end
	temp = list
	temp.each do |p|
		if (p[0] != 0)
			list = (list + [[(-p[0]), p[1]]])
		end
	end
	bestDot = (-999999999)
	best = dir
	list.each do |p|
		cross = ((p[0] * dir[1]) - (p[1] * dir[0]))
		if (cross >= 0)
			dot = ((p[0] * dir[0]) + (p[1] * dir[1]))
			if (dot > bestDot)
				bestDot = dot
				best = p
			end
		end
	end
	return {"d" => best, "heap" => h, "n" => nv}
end

def positions(n)
	pos = []
	x = 0
	y = 0
	dir = [0, 1]
	heap = []
	nv = 1
	i = 0
	while (i < n)
		pos = (pos + [[x, y]])
		st = step(heap, nv, dir)
		dir = st["d"]
		heap = st["heap"]
		nv = st["n"]
		x = (x + dir[0])
		y = (y + dir[1])
		i = (i + 1)
	end
	return pos
end

def pad(s, w)
	r = s
	while ((r).length < w)
		r = (r + " ")
	end
	return r
end

def main()
	pts = positions(40)
	puts("The first 40 Babylonian spiral points are:")
	line = ""
	i = 0
	while (i < (pts).length)
		p = pts[i]
		s = pad((((("(" + (p[0]).to_s) + ", ") + (p[1]).to_s) + ")"), 10)
		line = (line + s)
		if ((((i + 1)) % 10) == 0)
			puts(line)
			line = ""
		end
		i = (i + 1)
	end
end

main()
