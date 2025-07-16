def sqrtApprox(x)
	guess = x
	i = 0
	while (i < 20)
		guess = (((guess + (x / guess))) / 2.0)
		i = (i + 1)
	end
	return guess
end

$n = 10
$sum = 0.0
$x = 1
while ($x <= $n)
	$sum = ($sum + (($x) * ($x)))
	$x = ($x + 1)
end
$rms = sqrtApprox(($sum / ($n)))
puts(($rms).to_s)
