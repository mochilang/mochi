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

puts(chr(97))
puts(chr(960))
puts((chr(97) + chr(960)))
