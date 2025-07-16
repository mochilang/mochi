def _sliceString(s, i, j)
  start = i
  finish = j
  chars = s.chars
  n = chars.length
  start += n if start < 0
  finish += n if finish < 0
  start = 0 if start < 0
  finish = n if finish > n
  finish = start if finish < start
  chars[start...finish].join
end

def char(n)
	letters = "abcdefghijklmnopqrstuvwxyz"
	idx = (n - 97)
	if ((idx < 0) || (idx >= (letters).length))
		return "?"
	end
	return _sliceString(letters, idx, (idx + 1))
end

def fromBytes(bs)
	s = ""
	i = 0
	while (i < (bs).length)
		s = (s + char(bs[i]))
		i = (i + 1)
	end
	return s
end

$b = [98, 105, 110, 97, 114, 121]
puts(($b).to_s)
$c = $b
puts(($c).to_s)
puts((($b == $c)).to_s)
$d = []
$i = 0
while ($i < ($b).length)
	$d = ($d + [$b[$i]])
	$i = ($i + 1)
end
$d[1] = 97
$d[4] = 110
puts(fromBytes($b))
puts(fromBytes($d))
puts(((($b).length == 0)).to_s)
$z = ($b + [122])
puts(fromBytes($z))
$sub = $b[1...3]
puts(fromBytes($sub))
$f = []
$i = 0
while ($i < ($d).length)
	val = $d[$i]
	if (val == 110)
		$f = ($f + [109])
	else
		$f = ($f + [val])
	end
	$i = ($i + 1)
end
puts(((fromBytes($d) + " -> ") + fromBytes($f)))
$rem = []
$rem = ($rem + [$b[0]])
$i = 3
while ($i < ($b).length)
	$rem = ($rem + [$b[$i]])
	$i = ($i + 1)
end
puts(fromBytes($rem))
