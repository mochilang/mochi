Point = Struct.new(:x, :y, keyword_init: true)

def sqrtApprox(x)
	g = x
	i = 0
	while (i < 40)
		g = (((g + (x / g))) / 2.0)
		i = (i + 1)
	end
	return g
end

def hypot(x, y)
	return sqrtApprox(((x * x) + (y * y)))
end

def circles(p1, p2, r)
	if ((p1.x == p2.x) && (p1.y == p2.y))
		if (r == 0.0)
			return [p1, p1, "Coincident points with r==0.0 describe a degenerate circle."]
		end
		return [p1, p2, "Coincident points describe an infinite number of circles."]
	end
	if (r == 0.0)
		return [p1, p2, "R==0.0 does not describe circles."]
	end
	dx = (p2.x - p1.x)
	dy = (p2.y - p1.y)
	q = hypot(dx, dy)
	if (q > (2.0 * r))
		return [p1, p2, "Points too far apart to form circles."]
	end
	m = Point.new(x: (((p1.x + p2.x)) / 2.0), y: (((p1.y + p2.y)) / 2.0))
	if (q == (2.0 * r))
		return [m, m, "Points form a diameter and describe only a single circle."]
	end
	d = sqrtApprox(((r * r) - ((q * q) / 4.0)))
	ox = ((d * dx) / q)
	oy = ((d * dy) / q)
	return [Point.new(x: (m.x - oy), y: (m.y + ox)), Point.new(x: (m.x + oy), y: (m.y - ox)), "Two circles."]
end

$Two = "Two circles."
$R0 = "R==0.0 does not describe circles."
$Co = "Coincident points describe an infinite number of circles."
$CoR0 = "Coincident points with r==0.0 describe a degenerate circle."
$Diam = "Points form a diameter and describe only a single circle."
$Far = "Points too far apart to form circles."
$td = [[Point.new(x: 0.1234, y: 0.9876), Point.new(x: 0.8765, y: 0.2345), 2.0], [Point.new(x: 0.0, y: 2.0), Point.new(x: 0.0, y: 0.0), 1.0], [Point.new(x: 0.1234, y: 0.9876), Point.new(x: 0.1234, y: 0.9876), 2.0], [Point.new(x: 0.1234, y: 0.9876), Point.new(x: 0.8765, y: 0.2345), 0.5], [Point.new(x: 0.1234, y: 0.9876), Point.new(x: 0.1234, y: 0.9876), 0.0]]
$td.each do |tc|
	p1 = tc[0]
	p2 = tc[1]
	r = tc[2]
	puts((((("p1:  {" + (p1.x).to_s) + " ") + (p1.y).to_s) + "}"))
	puts((((("p2:  {" + (p2.x).to_s) + " ") + (p2.y).to_s) + "}"))
	puts(("r:  " + (r).to_s))
	res = circles(p1, p2, r)
	c1 = res[0]
	c2 = res[1]
	caseStr = res[2]
	puts(("   " + caseStr))
	if ((caseStr == "Points form a diameter and describe only a single circle.") || (caseStr == "Coincident points with r==0.0 describe a degenerate circle."))
		puts((((("   Center:  {" + (c1.x).to_s) + " ") + (c1.y).to_s) + "}"))
	else
		if (caseStr == "Two circles.")
			puts((((("   Center 1:  {" + (c1.x).to_s) + " ") + (c1.y).to_s) + "}"))
			puts((((("   Center 2:  {" + (c2.x).to_s) + " ") + (c2.y).to_s) + "}"))
		end
	end
	puts("")
end
