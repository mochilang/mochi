$n = 15
$t = []
(0...(($n + 2))).each do |_|
	$t = ($t + [0])
end
$t[1] = 1
(1...(($n + 1))).each do |i|
	j = i
	while (j > 1)
		$t[j] = ($t[j] + $t[(j - 1)])
		j = (j - 1)
	end
	$t[(i + 1)] = $t[i]
	j = (i + 1)
	while (j > 1)
		$t[j] = ($t[j] + $t[(j - 1)])
		j = (j - 1)
	end
	cat = ($t[(i + 1)] - $t[i])
	if (i < 10)
		puts((((" " + (i).to_s) + " : ") + (cat).to_s))
	else
		puts((((i).to_s + " : ") + (cat).to_s))
	end
end
