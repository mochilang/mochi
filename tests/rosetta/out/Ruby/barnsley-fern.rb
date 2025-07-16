def randInt(s, n)
	_next = ((((s * 1664525) + 1013904223)) % 2147483647)
	return [_next, (_next % n)]
end

$xMin = (-2.182)
$xMax = 2.6558
$yMin = 0.0
$yMax = 9.9983
$width = 60
$nIter = 10000
$dx = ($xMax - $xMin)
$dy = ($yMax - $yMin)
$height = ((($width * $dy) / $dx))
$grid = []
$row = 0
while ($row < $height)
	line = []
	col = 0
	while (col < $width)
		line = (line + [" "])
		col = (col + 1)
	end
	$grid = ($grid + [line])
	$row = ($row + 1)
end
$seed = 1
$x = 0.0
$y = 0.0
$ix = (((($width) * (($x - $xMin))) / $dx))
$iy = (((($height) * (($yMax - $y))) / $dy))
if (((($ix >= 0) && ($ix < $width)) && ($iy >= 0)) && ($iy < $height))
	$grid[$iy][$ix] = "*"
end
$i = 0
while ($i < $nIter)
	res = randInt($seed, 100)
	$seed = res[0]
	r = res[1]
	if (r < 85)
		nx = ((0.85 * $x) + (0.04 * $y))
		ny = ((((-0.04) * $x) + (0.85 * $y)) + 1.6)
		$x = nx
		$y = ny
	elsif (r < 92)
		nx = ((0.2 * $x) - (0.26 * $y))
		ny = (((0.23 * $x) + (0.22 * $y)) + 1.6)
		$x = nx
		$y = ny
	elsif (r < 99)
		nx = (((-0.15) * $x) + (0.28 * $y))
		ny = (((0.26 * $x) + (0.24 * $y)) + 0.44)
		$x = nx
		$y = ny
	else
		$x = 0.0
		$y = (0.16 * $y)
	end
	$ix = (((($width) * (($x - $xMin))) / $dx))
	$iy = (((($height) * (($yMax - $y))) / $dy))
	if (((($ix >= 0) && ($ix < $width)) && ($iy >= 0)) && ($iy < $height))
		$grid[$iy][$ix] = "*"
	end
	$i = ($i + 1)
end
$row = 0
while ($row < $height)
	line = ""
	col = 0
	while (col < $width)
		line = (line + $grid[$row][col])
		col = (col + 1)
	end
	puts(line)
	$row = ($row + 1)
end
