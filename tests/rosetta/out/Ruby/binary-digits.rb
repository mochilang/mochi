def toBin(n)
	if (n == 0)
		return "0"
	end
	bits = ""
	x = n
	while (x > 0)
		bits = (((x % 2)).to_s + bits)
		x = ((x / 2))
	end
	return bits
end

(0...16).each do |i|
	puts(toBin(i))
end
