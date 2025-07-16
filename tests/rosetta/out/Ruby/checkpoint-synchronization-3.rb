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

def lower(ch)
	up = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
	low = "abcdefghijklmnopqrstuvwxyz"
	i = 0
	while (i < (up).length)
		if (ch == _sliceString(up, i, (i + 1)))
			return _sliceString(low, i, (i + 1))
		end
		i = (i + 1)
	end
	return ch
end

$partList = ["A", "B", "C", "D"]
$nAssemblies = 3
$partList.each do |p|
	puts((p + " worker running"))
end
(1...(($nAssemblies + 1))).each do |cycle|
	puts(("begin assembly cycle " + (cycle).to_s))
	a = ""
	$partList.each do |p|
		puts((p + " worker begins part"))
		puts(((p + " worker completed ") + (p).to_s.downcase))
		a = (a + (p).to_s.downcase)
	end
	puts((((a + " assembled.  cycle ") + (cycle).to_s) + " complete"))
end
$partList.each do |p|
	puts((p + " worker stopped"))
end
