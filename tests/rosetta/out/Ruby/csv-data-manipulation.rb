def _joinStrings(parts, sep)
  raise 'join expects list' unless parts.is_a?(Array)
  parts.map(&:to_s).join(sep)
end

def join(xs, sep)
	res = ""
	i = 0
	while (i < (xs).length)
		if (i > 0)
			res = (res + sep)
		end
		res = (res + xs[i])
		i = (i + 1)
	end
	return res
end

def parseIntStr(str)
	i = 0
	neg = false
	if (((str).length > 0) && (str[0...1] == "-"))
		neg = true
		i = 1
	end
	n = 0
	digits = {"0" => 0, "1" => 1, "2" => 2, "3" => 3, "4" => 4, "5" => 5, "6" => 6, "7" => 7, "8" => 8, "9" => 9}
	while (i < (str).length)
		n = ((n * 10) + digits[str[i...(i + 1)]])
		i = (i + 1)
	end
	if neg
		n = (-n)
	end
	return n
end

$rows = [["A", "B", "C"], ["1", "2", "3"], ["4", "5", "6"], ["7", "8", "9"]]
$rows[0] = ($rows[0] + ["SUM"])
$i = 1
while ($i < ($rows).length)
	sum = 0
	$rows[$i].each do |s|
		sum = (sum + parseIntStr(s))
	end
	$rows[$i] = ($rows[$i] + [(sum).to_s])
	$i = ($i + 1)
end
$rows.each do |r|
	puts(_joinStrings(r, ","))
end
