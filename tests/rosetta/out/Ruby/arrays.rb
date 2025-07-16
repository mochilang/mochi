def listStr(xs)
	s = "["
	i = 0
	while (i < (xs).length)
		s = (s + (xs[i]).to_s)
		if ((i + 1) < (xs).length)
			s = (s + " ")
		end
		i = (i + 1)
	end
	s = (s + "]")
	return s
end

$a = [0, 0, 0, 0, 0]
puts(("len(a) = " + (($a).length).to_s))
puts(("a = " + listStr($a)))
$a[0] = 3
puts(("a = " + listStr($a)))
puts(("a[0] = " + ($a[0]).to_s))
$s = $a[0...4]
$cap_s = 5
puts(("s = " + listStr($s)))
puts(((("len(s) = " + (($s).length).to_s) + "  cap(s) = ") + ($cap_s).to_s))
$s = $a[0...5]
puts(("s = " + listStr($s)))
$a[0] = 22
$s[0] = 22
puts(("a = " + listStr($a)))
puts(("s = " + listStr($s)))
$s = ($s + [4])
$s = ($s + [5])
$s = ($s + [6])
$cap_s = 10
puts(("s = " + listStr($s)))
puts(((("len(s) = " + (($s).length).to_s) + "  cap(s) = ") + ($cap_s).to_s))
$a[4] = (-1)
puts(("a = " + listStr($a)))
puts(("s = " + listStr($s)))
$s = []
(0...8).each do |i|
	$s = ($s + [0])
end
$cap_s = 8
puts(("s = " + listStr($s)))
puts(((("len(s) = " + (($s).length).to_s) + "  cap(s) = ") + ($cap_s).to_s))
