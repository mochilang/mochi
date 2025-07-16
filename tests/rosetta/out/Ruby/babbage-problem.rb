$target = 269696
$modulus = 1000000
$n = 1
while true
	square = ($n * $n)
	ending = (square % $modulus)
	if (ending == $target)
		puts(((("The smallest number whose square ends with " + ($target).to_s) + " is ") + ($n).to_s))
		break
	end
	$n = ($n + 1)
end
