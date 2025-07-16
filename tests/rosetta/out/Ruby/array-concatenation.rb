def concatInts(a, b)
	out = []
	a.each do |v|
		out = (out + [v])
	end
	b.each do |v|
		out = (out + [v])
	end
	return out
end

def concatAny(a, b)
	out = []
	a.each do |v|
		out = (out + [v])
	end
	b.each do |v|
		out = (out + [v])
	end
	return out
end

$a = [1, 2, 3]
$b = [7, 12, 60]
puts((concatInts($a, $b)).to_s)
$i = [1, 2, 3]
$j = ["Crosby", "Stills", "Nash", "Young"]
puts((concatAny($i, $j)).to_s)
$l = [1, 2, 3]
$m = [7, 12, 60]
puts((concatInts($l, $m)).to_s)
