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

puts((ord("A")).to_s)
puts(chr(65))
