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

def setChar(s, idx, ch)
	return ((_sliceString(s, 0, idx) + ch) + _sliceString(s, (idx + 1), (s).length))
end

$width = 81
$height = 5
$lines = []
(0...$height).each do |i|
	row = ""
	j = 0
	while (j < $width)
		row = (row + "*")
		j = (j + 1)
	end
	$lines = ($lines + [row])
end
$stack = [{"start" => 0, "len" => $width, "index" => 1}]
while (($stack).length > 0)
	frame = $stack[(($stack).length - 1)]
	$stack = $stack[0...(($stack).length - 1)]
	start = frame["start"]
	lenSeg = frame["len"]
	index = frame["index"]
	seg = ((lenSeg / 3))
	if (seg == 0)
		next
	end
	i = index
	while (i < $height)
		j = (start + seg)
		while (j < (start + (2 * seg)))
			$lines[i] = setChar($lines[i], j, " ")
			j = (j + 1)
		end
		i = (i + 1)
	end
	$stack = ($stack + [{"start" => start, "len" => seg, "index" => (index + 1)}])
	$stack = ($stack + [{"start" => (start + (seg * 2)), "len" => seg, "index" => (index + 1)}])
end
$lines.each do |line|
	puts(line)
end
