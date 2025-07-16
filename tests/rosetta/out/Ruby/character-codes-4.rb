def chr(n)
	if (n == 97)
		return "a"
	end
	if (n == 960)
		return "π"
	end
	if (n == 65)
		return "A"
	end
	return "?"
end

$b = 97
$r = 960
puts(((chr(97) + " ") + chr(960)))
puts(((chr($b) + " ") + chr($r)))
