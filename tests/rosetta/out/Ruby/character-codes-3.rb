def ord(ch)
	if (ch == "a")
		return 97
	end
	if (ch == "π")
		return 960
	end
	if (ch == "A")
		return 65
	end
	return 0
end

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

$b = ord("a")
$r = ord("π")
$s = "aπ"
puts(((((($b).to_s + " ") + ($r).to_s) + " ") + $s))
puts((((("string cast to []rune: [" + ($b).to_s) + " ") + ($r).to_s) + "]"))
puts(((("    string range loop: " + ($b).to_s) + " ") + ($r).to_s))
puts("         string bytes: 0x61 0xcf 0x80")
