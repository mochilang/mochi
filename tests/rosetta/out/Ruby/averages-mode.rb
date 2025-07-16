$arr1 = [2, 7, 1, 8, 2]
$counts1 = {}
$keys1 = []
$i = 0
while ($i < ($arr1).length)
	v = $arr1[$i]
	if ($counts1.to_h.key?(v))
		$counts1[v] = ($counts1[v] + 1)
	else
		$counts1[v] = 1
		$keys1 = ($keys1 + [v])
	end
	$i = ($i + 1)
end
$max1 = 0
$i = 0
while ($i < ($keys1).length)
	k = $keys1[$i]
	c = $counts1[k]
	if (c > $max1)
		$max1 = c
	end
	$i = ($i + 1)
end
$modes1 = []
$i = 0
while ($i < ($keys1).length)
	k = $keys1[$i]
	if ($counts1[k] == $max1)
		$modes1 = ($modes1 + [k])
	end
	$i = ($i + 1)
end
puts(($modes1).to_s)
$arr2 = [2, 7, 1, 8, 2, 8]
$counts2 = {}
$keys2 = []
$i = 0
while ($i < ($arr2).length)
	v = $arr2[$i]
	if ($counts2.to_h.key?(v))
		$counts2[v] = ($counts2[v] + 1)
	else
		$counts2[v] = 1
		$keys2 = ($keys2 + [v])
	end
	$i = ($i + 1)
end
$max2 = 0
$i = 0
while ($i < ($keys2).length)
	k = $keys2[$i]
	c = $counts2[k]
	if (c > $max2)
		$max2 = c
	end
	$i = ($i + 1)
end
$modes2 = []
$i = 0
while ($i < ($keys2).length)
	k = $keys2[$i]
	if ($counts2[k] == $max2)
		$modes2 = ($modes2 + [k])
	end
	$i = ($i + 1)
end
puts(($modes2).to_s)
