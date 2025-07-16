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

puts((ord("a")).to_s)
puts((ord("π")).to_s)
