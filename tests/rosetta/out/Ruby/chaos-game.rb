def randInt(s, n)
	_next = ((((s * 1664525) + 1013904223)) % 2147483647)
	return [_next, (_next % n)]
end

$width = 60
$height = (($width * 0.86602540378))
$iterations = 5000
$grid = []
$y = 0
while ($y < $height)
	line = []
	x = 0
	while (x < $width)
		line = (line + [" "])
		x = (x + 1)
	end
	$grid = ($grid + [line])
	$y = ($y + 1)
end
$seed = 1
$vertices = [[0, ($height - 1)], [($width - 1), ($height - 1)], [(($width / 2)), 0]]
$px = (($width / 2))
$py = (($height / 2))
$i = 0
while ($i < $iterations)
	r = randInt($seed, 3)
	$seed = r[0]
	idx = r[1]
	v = $vertices[idx]
	$px = (((($px + v[0])) / 2))
	$py = (((($py + v[1])) / 2))
	if (((($px >= 0) && ($px < $width)) && ($py >= 0)) && ($py < $height))
		$grid[$py][$px] = "*"
	end
	$i = ($i + 1)
end
$y = 0
while ($y < $height)
	line = ""
	x = 0
	while (x < $width)
		line = (line + $grid[$y][x])
		x = (x + 1)
	end
	puts(line)
	$y = ($y + 1)
end
